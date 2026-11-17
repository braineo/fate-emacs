;;; fate-currency.el --- Currency converter with rate charts -*- lexical-binding: t -*-

;;; Commentary:
;; `fate/currency' opens a converter listing the currencies in
;; `fate-currency-list'.  RET on a row makes it the base (marked ▸) and
;; asks for an amount; typing a digit anywhere re-enters the base amount.
;; Amounts may be expressions such as "120*3+15" (evaluated by
;; `calc-eval'); every other row shows the amount converted.
;;
;; Under the list a full-width SVG line chart plots the row at point
;; against the base.  TAB cycles the range (1d 5d 1m 1y 5y); ←/→ or a
;; mouse click move a cursor along the line to read a single point.
;; `+'/`-' add/remove currencies and save the list to `custom-file'.
;;
;; Rates come from Yahoo Finance's chart endpoint (unofficial, no API key),
;; fetched asynchronously with `plz' and cached per symbol and range.
;; Currency names come from Yahoo's currency list, cached on disk for 30 days.

;;; Code:

(require 'svg)
(require 'seq)
(require 'cl-lib)
(require 'color)
(use-package plz)
(declare-function plz "plz")

;;; Options and data

(defcustom fate-currency-list '("USD" "EUR" "CNY" "JPY" "GBP" "HKD")
  "ISO 4217 codes shown in the `fate/currency' buffer, in order.
Edited by `fate/currency-add' and `fate/currency-remove'."
  :type '(repeat string)
  :group 'fate)

(defconst fate-currency--ranges
  ;; (LABEL YAHOO-RANGE INTERVAL AXIS-FORMAT CURSOR-FORMAT TTL-SECONDS)
  '(("1d" "1d"  "5m"  "%H:%M"   "%F %H:%M" 300)
    ("5d" "5d"  "15m" "%a %d"   "%F %H:%M" 300)
    ("1m" "1mo" "1h"  "%b %d"   "%F %H:%M" 3600)
    ("1y" "1y"  "1d"  "%b %Y"   "%F"       3600)
    ("5y" "5y"  "1wk" "%Y"      "%F"       3600))
  "Chart ranges and how each is fetched and labelled.")

;;; Faces

(defface fate-currency-title
  '((t :inherit (bold font-lock-function-name-face)))
  "Face for the title line." :group 'fate)

(defface fate-currency-hint
  '((t :inherit shadow))
  "Face for hints and inactive range labels." :group 'fate)

(defface fate-currency-base
  '((t :inherit highlight :weight bold))
  "Face for the base row marker and the active range." :group 'fate)

(defface fate-currency-code
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for currency codes." :group 'fate)

(defface fate-currency-amount
  '((t :inherit font-lock-string-face))
  "Face for converted amounts." :group 'fate)

;;; State

(defvar-local fate-currency--base nil "Code of the base currency.")
(defvar-local fate-currency--amount 1 "Amount, in the base currency.")
(defvar-local fate-currency--target nil "Code charted against the base.")
(defvar-local fate-currency--range "1d" "Label of the charted range.")
(defvar-local fate-currency--cursor nil
  "Index of the chart cursor into the points; nil means the latest point.")
(defvar-local fate-currency--chart nil
  "Geometry of the drawn chart: (LEFT PLOT-WIDTH N WIDTH HEIGHT).")

(defvar fate-currency--cache (make-hash-table :test #'equal)
  "Map \"SYMBOL/RANGE\" to (FETCH-TIME . DATA).
DATA is a plist (:price P :points [(TIME . CLOSE) ...]) or the symbol `error'.")

(defvar fate-currency--pending (make-hash-table :test #'equal)
  "Keys of `fate-currency--cache' with a fetch in flight.")

(defvar fate-currency--names nil
  "Alist of (CODE . NAME) for every currency Yahoo quotes; nil until fetched.")

(defvar fate-currency--names-pending nil
  "t while the currency list is being fetched, `failed' after a failed fetch.")

(defvar fate-cache-directory)
(defconst fate-currency-names-file
  (expand-file-name "currencies.eld" fate-cache-directory)
  "File caching Yahoo's currency list between sessions.")

;;; Fetching

(defun fate/currency--parse (json)
  "Extract price and (TIME . CLOSE) points from a Yahoo chart JSON alist."
  (condition-case nil
      (let* ((result (aref (alist-get 'result (alist-get 'chart json)) 0))
             (times (alist-get 'timestamp result))
             (closes (alist-get 'close (aref (alist-get 'quote (alist-get 'indicators result)) 0)))
             points)
        (dotimes (i (length times))
          (when-let* ((c (aref closes i)))
            (push (cons (aref times i) c) points)))
        (list :price (alist-get 'regularMarketPrice (alist-get 'meta result))
              :points (vconcat (nreverse points))))
    (error 'error)))

(defun fate/currency--store (key data)
  "Cache DATA under KEY and redraw the converter."
  (remhash key fate-currency--pending)
  (puthash key (cons (current-time) data) fate-currency--cache)
  (fate/currency--redraw))

(defun fate/currency--redraw ()
  "Redraw the converter buffer, if it exists."
  (when-let* ((buf (get-buffer "*currency*")))
    (with-current-buffer buf (fate/currency--render))))

(defun fate/currency--get (symbol range)
  "Return cached data for Yahoo SYMBOL over RANGE (a label like \"1m\").
Missing or stale data starts an async fetch that redraws on arrival.
Returns nil while nothing is cached yet, else a plist or `error'."
  (let* ((spec (assoc range fate-currency--ranges))
         (key (concat symbol "/" range))
         (hit (gethash key fate-currency--cache)))
    (when (and (or (null hit) (> (float-time (time-since (car hit))) (nth 5 spec)))
               (not (gethash key fate-currency--pending)))
      (puthash key t fate-currency--pending)
      (plz 'get (format "https://query1.finance.yahoo.com/v8/finance/chart/%s?range=%s&interval=%s"
                        symbol (nth 1 spec) (nth 2 spec))
        :headers '(("User-Agent" . "Mozilla/5.0"))
        :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil))
        :then (lambda (json) (fate/currency--store key (fate/currency--parse json)))
        :else (lambda (_) (fate/currency--store key 'error))))
    (cdr hit)))

(defun fate/currency--per-usd (code)
  "Units of CODE per US dollar: a number, nil while loading, or `error'."
  (if (equal code "USD")
      1.0
    (let ((data (fate/currency--get (format "USD%s=X" code) "1d")))
      (if (consp data) (plist-get data :price) data))))

(defun fate/currency--parse-names (json)
  "Return the (CODE . NAME) alist, sorted by code, from Yahoo's currency JSON."
  (sort (seq-map (lambda (c) (cons (alist-get 'shortName c) (alist-get 'longName c)))
                 (alist-get 'result (alist-get 'currencies json)))
        (lambda (a b) (string< (car a) (car b)))))

(defun fate/currency--names-age ()
  "Seconds since the currency list file was written, or nil if absent."
  (when-let* ((attrs (file-attributes fate-currency-names-file)))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun fate/currency--fetch-names (&optional wait)
  "Fetch the currency list from Yahoo into memory and `fate-currency-names-file'.
In the background (redrawing on arrival), or synchronously when WAIT."
  (let* ((url "https://query1.finance.yahoo.com/v1/finance/currencies")
         (headers '(("User-Agent" . "Mozilla/5.0")))
         (as (lambda () (json-parse-buffer :object-type 'alist :null-object nil)))
         (store (lambda (json)
                  (setq fate-currency--names (fate/currency--parse-names json)
                        fate-currency--names-pending nil)
                  (with-temp-file fate-currency-names-file
                    (prin1 fate-currency--names (current-buffer))))))
    (if wait
        (condition-case err
            (funcall store (plz 'get url :headers headers :as as))
          (error (message "Currency list unavailable: %s" (error-message-string err))))
      (setq fate-currency--names-pending t)
      (plz 'get url :headers headers :as as
        :then (lambda (json) (funcall store json) (fate/currency--redraw))
        :else (lambda (_) (setq fate-currency--names-pending 'failed))))))

(defun fate/currency--names (&optional wait)
  "Return the (CODE . NAME) alist of every currency Yahoo quotes.
Read from `fate-currency-names-file'; fetched again in the background
once that is older than 30 days (one attempt per session, or per
`fate/currency-refresh').  With WAIT, fetch synchronously when nothing
is available at all."
  (let ((age (fate/currency--names-age)))
    (when (and (null fate-currency--names) age)
      (setq fate-currency--names
            (with-temp-buffer
              (insert-file-contents fate-currency-names-file)
              (ignore-errors (read (current-buffer))))))
    (cond ((and wait (null fate-currency--names)) (fate/currency--fetch-names t))
          ((and (not fate-currency--names-pending)
                (or (null fate-currency--names) (and age (> age (* 30 86400)))))
           (fate/currency--fetch-names))))
  fate-currency--names)

;;; Formatting and input

(defun fate/currency--format (n &optional min-decimals)
  "Format N with thousands separators.
Uses at least MIN-DECIMALS (default 2) decimals and enough to show four
significant digits of small values."
  (let* ((d (if (or (zerop n) (>= (abs n) 1)) (or min-decimals 2)
              (min 8 (max (or min-decimals 2) (- 3 (floor (log (abs n) 10)))))))
         (parts (split-string (format (format "%%.%df" d) n) "\\."))
         (int (car parts)))
    (while (string-match "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\(,\\|\\'\\)" int)
      (setq int (replace-match "\\1,\\2\\3" t nil int)))
    (if (cdr parts) (concat int "." (cadr parts)) int)))

(defun fate/currency--eval (input)
  "Return the non-negative number INPUT denotes.
Anything but a plain number is evaluated by `calc-eval'."
  (let* ((s (string-trim (string-replace "," "" input)))
         (r (if (string-match-p "\\`[0-9]*\\.?[0-9]+\\'" s)
                (string-to-number s)
              (let ((v (calc-eval s)))
                (and (stringp v) (string-match-p "\\`[-0-9.e]+\\'" v)
                     (string-to-number v))))))
    (unless (and r (>= r 0))
      (user-error "Not a valid amount: %s" input))
    r))

(defun fate/currency--name (code)
  "Return the display name of CODE."
  (or (cdr (assoc code (fate/currency--names))) code))

(defun fate/currency--color (face)
  "Return FACE's foreground as a #rrggbb string usable in SVG."
  (let ((c (or (face-foreground face nil 'default) "gray")))
    (if-let* ((rgb (color-name-to-rgb c)))
        (apply #'color-rgb-to-hex (append rgb '(2)))
      "#808080")))

;;; Chart

(defun fate/currency--ticks (lo hi)
  "Return (STEP . TICKS): round values spanning LO..HI, about five of them."
  (let* ((raw (/ (- hi lo) 5.0))
         (mag (expt 10.0 (floor (log raw 10))))
         (step (* mag (seq-find (lambda (m) (>= (* m mag) raw)) '(1 2 2.5 5 10)))))
    (cons step (mapcar (lambda (k) (* k step))
                       (number-sequence (ceiling (/ lo step)) (floor (/ hi step)))))))

(defun fate/currency--fixed (decimals v)
  "Format V with DECIMALS digits after the point."
  (format (format "%%.%df" decimals) v))

(defun fate/currency--chart-svg (points cursor spec w h)
  "Return an SVG line chart of POINTS sized W x H pixels.
CURSOR indexes the highlighted point; SPEC is the range entry.
Also records the plot geometry in `fate-currency--chart'."
  (let* ((n (length points))
         (vals (seq-into (seq-map #'cdr points) 'vector))
         (lo (seq-min vals)) (hi (seq-max vals))
         (pad (if (= lo hi) (max (* 0.01 (abs lo)) 1e-6) (* 0.06 (- hi lo))))
         (ymin (- lo pad)) (ymax (+ hi pad))
         (ticks (fate/currency--ticks ymin ymax))
         (ydec (seq-find (lambda (d) (let ((v (* (car ticks) (expt 10 d))))
                                        (< (abs (- v (round v))) 1e-6)))
                         (number-sequence 0 8) 8))
         (fs (* 0.8 (frame-char-height)))
         (family (face-attribute 'default :family nil t))
         (fg (fate/currency--color 'default))
         (dim (fate/currency--color 'shadow))
         (ink (fate/currency--color 'font-lock-function-name-face))
         (accent (fate/currency--color 'font-lock-keyword-face))
         (labels (mapcar (lambda (v) (fate/currency--fixed ydec v)) (cdr ticks)))
         (left 8)
         (top (* 1.6 fs))
         (right (- w (* fs 0.62 (apply #'max 1 (mapcar #'length labels))) 16))
         (bottom (- h (* 2 fs)))
         (pw (- right left))
         (ph (- bottom top))
         (x (lambda (i) (if (= n 1) (+ left (/ pw 2.0)) (+ left (* i (/ pw (float (1- n))))))))
         (y (lambda (v) (+ top (* ph (/ (- ymax v) (- ymax ymin))))))
         (line (seq-map-indexed (lambda (p i) (cons (funcall x i) (funcall y (cdr p)))) points))
         (svg (svg-create w h)))
    (setq fate-currency--chart (list left pw n w h))
    (cl-flet ((text (str tx ty &rest args)
                (apply #'svg-text svg str :x tx :y ty :font-size fs
                       :font-family family args)))
      ;; Horizontal gridlines with value labels on the right.
      (seq-mapn (lambda (v label)
                  (let ((ty (funcall y v)))
                    (svg-line svg left ty right ty :stroke dim :stroke-opacity 0.3)
                    (text label (+ right 8) (+ ty (* 0.35 fs)) :fill dim)))
                (cdr ticks) labels)
      ;; Time labels along the bottom, about six of them.
      (let ((k (min 6 n)))
        (dotimes (j k)
          (let* ((i (if (= k 1) 0 (round (* j (/ (1- n) (float (1- k)))))))
                 (tx (funcall x i)))
            (svg-line svg tx bottom tx (+ bottom 4) :stroke dim)
            (text (format-time-string (nth 3 spec) (car (aref points i)))
                  tx (+ bottom (* 1.4 fs)) :fill dim
                  :text-anchor (cond ((= j 0) "start") ((= j (1- k)) "end") (t "middle"))))))
      ;; Area under the line, then the line itself.
      (svg-polygon svg (append (list (cons left bottom)) line (list (cons right bottom)))
                   :fill ink :fill-opacity 0.12 :stroke "none")
      (svg-polyline svg line :fill "none" :stroke ink :stroke-width 2
                    :stroke-linejoin "round")
      ;; High and low markers.
      (dolist (m (list (cons (seq-position vals hi) -0.6) (cons (seq-position vals lo) 1.3)))
        (let* ((i (car m)) (px (funcall x i)) (py (funcall y (aref vals i))))
          (svg-circle svg px py 3 :fill accent)
          (text (fate/currency--fixed (1+ ydec) (aref vals i)) px (+ py (* (cdr m) fs))
                :fill accent :text-anchor (if (> px (/ w 2)) "end" "start"))))
      ;; Cursor: vertical guide, dot, and its value/time label at the top.
      (let* ((i cursor) (px (funcall x i)) (p (aref points i)))
        (svg-line svg px top px bottom :stroke fg :stroke-opacity 0.5
                  :stroke-dasharray "4 4")
        (svg-circle svg px (funcall y (cdr p)) 5 :fill ink :stroke fg :stroke-width 1.5)
        (text (format "%s  %s" (fate/currency--format (cdr p) 4)
                      (format-time-string (nth 4 spec) (car p)))
              px (* 1.1 fs) :fill fg :font-weight "bold"
              :text-anchor (if (> px (/ w 2)) "end" "start"))))
    svg))

(defun fate/currency-click (event)
  "Move the chart cursor to the point nearest the mouse EVENT."
  (interactive "e")
  (pcase-let ((`(,left ,pw ,n . ,_) fate-currency--chart)
              (`(,px . ,_) (posn-object-x-y (event-start event))))
    (when (> n 1)
      (setq fate-currency--cursor
            (max 0 (min (1- n) (round (* (/ (- px left) (float pw)) (1- n))))))
      (fate/currency--render))))

(defvar-keymap fate-currency-chart-map
  :doc "Keymap active on the chart image."
  "<mouse-1>" #'fate/currency-click)

(defun fate/currency--insert-chart (base target win)
  "Insert the chart of BASE→TARGET sized to WIN (which may be nil)."
  (let* ((spec (assoc fate-currency--range fate-currency--ranges))
         (data (fate/currency--get (concat base target "=X") fate-currency--range))
         (points (and (consp data) (plist-get data :points))))
    (insert " " (propertize (format "%s → %s" base target) 'face 'fate-currency-title) "   ")
    (dolist (r fate-currency--ranges)
      (insert (propertize (format " %s " (car r)) 'face
                          (if (equal (car r) fate-currency--range)
                              'fate-currency-base 'fate-currency-hint))))
    (insert "\n\n")
    (cond
     ((eq data 'error) (insert (propertize "  No data for this pair.\n" 'face 'error)))
     ((not (display-images-p))
      (insert (propertize "  The chart needs a graphical frame.\n" 'face 'fate-currency-hint)))
     ((seq-empty-p points) (insert (propertize "  Loading…\n" 'face 'fate-currency-hint)))
     (t
      (let* ((w (if win (- (window-body-width win t) (* 2 (frame-char-width)))
                  (or (nth 3 fate-currency--chart) 800)))
             (h (if win (max 220 (round (* 0.5 (window-body-height win t))))
                  (or (nth 4 fate-currency--chart) 320)))
             (n (length points))
             (cursor (min (1- n) (or fate-currency--cursor (1- n)))))
        (insert " " (propertize " " 'display
                                (svg-image (fate/currency--chart-svg points cursor spec w h)
                                           :scale 1 :ascent 'center)
                                'keymap fate-currency-chart-map
                                'pointer 'hand)
                "\n"))))))

;;; Rendering

(defun fate/currency--base-code ()
  "Return the base code, falling back to the first listed currency."
  (if (member fate-currency--base fate-currency-list)
      fate-currency--base
    (car fate-currency-list)))

(defun fate/currency--target-code (base)
  "Return the code charted against BASE."
  (let ((others (remove base fate-currency-list)))
    (if (member fate-currency--target others) fate-currency--target (car others))))

(defun fate/currency--convert (code base)
  "Return the current amount converted from BASE to CODE, or nil/`error'."
  (let ((to (fate/currency--per-usd code))
        (from (fate/currency--per-usd base)))
    (cond ((or (eq to 'error) (eq from 'error)) 'error)
          ((and to from) (* fate-currency--amount (/ to from))))))

(defun fate/currency--render ()
  "Redraw the converter list and chart in the current buffer."
  (let* ((inhibit-read-only t)
         (win (get-buffer-window (current-buffer)))
         (line (line-number-at-pos (if win (window-point win) (point))))
         (base (fate/currency--base-code))
         (target (and base (fate/currency--target-code base)))
         (width (apply #'max 8 (mapcar (lambda (c) (length (fate/currency--name c)))
                                       fate-currency-list))))
    (setq fate-currency--base base)
    (erase-buffer)
    (insert (propertize "Currency" 'face 'fate-currency-title)
            (propertize "  —  " 'face 'fate-currency-hint)
            (if base (format "%s %s" (fate/currency--format fate-currency--amount) base) "")
            "\n"
            (propertize (concat "  0-9: base amount   RET: set base   n/p: row   TAB: range   ←/→: scrub   "
                                "+/-: add/remove   w: copy   g: refresh   q: quit\n\n")
                        'face 'fate-currency-hint))
    (if (null base)
        (insert "  No currencies.  Press + to add one.\n")
      (dolist (code fate-currency-list)
        (let* ((base-p (equal code base))
               (value (fate/currency--convert code base))
               (rate (and (numberp value) (/ value (max fate-currency--amount 1e-12)))))
          (insert (propertize
                   (format "%s %s  %s  %18s   %s"
                           (if base-p (propertize "▸" 'face 'fate-currency-base) " ")
                           (propertize code 'face 'fate-currency-code)
                           (string-pad (fate/currency--name code) width)
                           (propertize (cond ((numberp value) (fate/currency--format value))
                                             ((eq value 'error) "n/a")
                                             (t "…"))
                                       'face 'fate-currency-amount)
                           (if (and rate (not base-p) (> fate-currency--amount 0))
                               (propertize (format "1 %s = %s %s" base
                                                   (fate/currency--format rate 4) code)
                                           'face 'fate-currency-hint)
                             ""))
                   'fate-currency-code code)
                  "\n")))
      (when target
        (insert "\n")
        (fate/currency--insert-chart base target win)))
    (goto-char (point-min))
    (forward-line (1- line))
    (when win (set-window-point win (point)))))

;;; Commands

(defun fate/currency--code-at-point ()
  "Return the currency code on the current line, if any."
  (get-text-property (line-beginning-position) 'fate-currency-code))

(defun fate/currency--read-amount (code initial)
  "Prompt for an amount in CODE, pre-filled with INITIAL, and make CODE the base."
  (let ((old-base (fate/currency--base-code))
        (input (read-from-minibuffer (format "Amount in %s (arithmetic ok): " code)
                                     initial)))
    (setq fate-currency--amount (fate/currency--eval input))
    (unless (equal code old-base)
      ;; Keep the chart on the pair just swapped, now seen from the other side.
      (when (equal code fate-currency--target)
        (setq fate-currency--target old-base))
      (setq fate-currency--base code
            fate-currency--cursor nil))
    (fate/currency--render)))

(defun fate/currency-input ()
  "Make the row at point the base and read its amount.
The prompt starts with the row's current converted value."
  (interactive)
  (let* ((code (or (fate/currency--code-at-point) (user-error "Not on a currency row")))
         (current (fate/currency--convert code (fate/currency--base-code))))
    (fate/currency--read-amount code (and (numberp current) (format "%.2f" current)))))

(defun fate/currency-digit ()
  "Enter a new base amount, starting with the typed digit.
Works from any row; the base currency stays the same."
  (interactive)
  (fate/currency--read-amount (or (fate/currency--base-code) (user-error "No currencies"))
                              (this-command-keys)))

(defun fate/currency--goto-row (code)
  "Move point to the row for CODE."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (fate/currency--code-at-point) code)))
    (forward-line 1)))

(defun fate/currency-next-row (&optional n)
  "Move to the Nth next currency row (previous when N is negative)."
  (interactive "p")
  (let ((codes fate-currency-list)
        (here (fate/currency--code-at-point)))
    (when codes
      (fate/currency--goto-row
       (nth (max 0 (min (1- (length codes))
                        (if here (+ (seq-position codes here) n) 0)))
            codes)))))

(defun fate/currency-prev-row (&optional n)
  "Move to the Nth previous currency row."
  (interactive "p")
  (fate/currency-next-row (- n)))

(defun fate/currency--follow-point ()
  "Chart the row at point when it changes, for `post-command-hook'."
  (let ((code (fate/currency--code-at-point)))
    (when (and code
               (not (equal code fate-currency--target))
               (not (equal code (fate/currency--base-code))))
      (setq fate-currency--target code
            fate-currency--cursor nil)
      (fate/currency--render))))

(defun fate/currency--on-resize (win)
  "Redraw when WIN's size no longer matches the drawn chart."
  (with-current-buffer (window-buffer win)
    (unless (equal (- (window-body-width win t) (* 2 (frame-char-width)))
                   (nth 3 fate-currency--chart))
      (fate/currency--render))))

(defun fate/currency--cycle-range (delta)
  "Move the chart range by DELTA entries."
  (let* ((labels (mapcar #'car fate-currency--ranges))
         (i (seq-position labels fate-currency--range)))
    (setq fate-currency--range (nth (mod (+ i delta) (length labels)) labels)
          fate-currency--cursor nil)
    (fate/currency--render)))

(defun fate/currency-next-range ()
  "Show the next (longer) range."
  (interactive)
  (fate/currency--cycle-range 1))

(defun fate/currency-prev-range ()
  "Show the previous (shorter) range."
  (interactive)
  (fate/currency--cycle-range -1))

(defun fate/currency--scrub (delta)
  "Move the chart cursor by DELTA points."
  (let ((n (or (nth 2 fate-currency--chart) 0)))
    (when (> n 0)
      (setq fate-currency--cursor
            (max 0 (min (1- n) (+ (or fate-currency--cursor (1- n)) delta))))
      (fate/currency--render))))

(defun fate/currency-scrub-forward (&optional n)
  "Move the chart cursor N points later."
  (interactive "p")
  (fate/currency--scrub n))

(defun fate/currency-scrub-backward (&optional n)
  "Move the chart cursor N points earlier."
  (interactive "p")
  (fate/currency--scrub (- n)))

(defun fate/currency-add (code)
  "Add currency CODE after the row at point and save the list."
  (interactive
   (let* ((names (seq-remove (lambda (c) (member (car c) fate-currency-list))
                             (fate/currency--names t)))
          (completion-extra-properties
           (list :annotation-function
                 (lambda (code)
                   (when-let* ((name (cdr (assoc code names))))
                     (concat (propertize " " 'display '(space :align-to 8))
                             (propertize name 'face 'completions-annotations)))))))
     (list (upcase (completing-read "Add currency: " names)))))
  (unless (string-match-p "\\`[A-Z]\\{3\\}\\'" code)
    (user-error "Not an ISO currency code: %s" code))
  (when (member code fate-currency-list)
    (user-error "%s is already listed" code))
  (let* ((here (seq-position fate-currency-list (fate/currency--code-at-point)))
         (i (if here (1+ here) (length fate-currency-list))))
    (customize-save-variable 'fate-currency-list
                             (append (seq-take fate-currency-list i) (list code)
                                     (seq-drop fate-currency-list i))))
  (fate/currency--render)
  (fate/currency--goto-row code))

(defun fate/currency-remove ()
  "Remove the row at point and save the list."
  (interactive)
  (let ((code (or (fate/currency--code-at-point) (user-error "Not on a currency row"))))
    (customize-save-variable 'fate-currency-list (remove code fate-currency-list))
    (fate/currency--render)))

(defun fate/currency-copy ()
  "Copy the conversion for the row at point, or every row on the base row."
  (interactive)
  (let* ((base (fate/currency--base-code))
         (here (fate/currency--code-at-point))
         (codes (if (or (null here) (equal here base)) (remove base fate-currency-list)
                  (list here)))
         (text (mapconcat
                (lambda (code)
                  (let ((v (fate/currency--convert code base)))
                    (format "%s %s = %s %s" (fate/currency--format fate-currency--amount) base
                            (if (numberp v) (fate/currency--format v) "n/a") code)))
                codes "\n")))
    (kill-new text)
    (message "Copied:\n%s" text)))

(defun fate/currency-refresh ()
  "Drop cached rates and fetch them, and the currency list, again."
  (interactive)
  (clrhash fate-currency--cache)
  (fate/currency--fetch-names)
  (fate/currency--render))

(defvar-keymap fate-currency-mode-map
  :doc "Keymap for `fate-currency-mode'."
  "0" #'fate/currency-digit "1" #'fate/currency-digit "2" #'fate/currency-digit
  "3" #'fate/currency-digit "4" #'fate/currency-digit "5" #'fate/currency-digit
  "6" #'fate/currency-digit "7" #'fate/currency-digit "8" #'fate/currency-digit
  "9" #'fate/currency-digit "." #'fate/currency-digit
  "RET"       #'fate/currency-input
  "n"         #'fate/currency-next-row
  "j"         #'fate/currency-next-row
  "<down>"    #'fate/currency-next-row
  "p"         #'fate/currency-prev-row
  "k"         #'fate/currency-prev-row
  "<up>"      #'fate/currency-prev-row
  "TAB"       #'fate/currency-next-range
  "<backtab>" #'fate/currency-prev-range
  "<right>"   #'fate/currency-scrub-forward
  "l"         #'fate/currency-scrub-forward
  "<left>"    #'fate/currency-scrub-backward
  "h"         #'fate/currency-scrub-backward
  "+"         #'fate/currency-add
  "-"         #'fate/currency-remove
  "w"         #'fate/currency-copy
  "g"         #'fate/currency-refresh
  "q"         #'quit-window)

(define-derived-mode fate-currency-mode special-mode "Currency"
  "Major mode for converting between currencies and charting their rates.

\\{fate-currency-mode-map}"
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'fate/currency--follow-point nil t)
  (add-hook 'window-size-change-functions #'fate/currency--on-resize nil t))

;;;###autoload
(defun fate/currency ()
  "Open the currency converter."
  (interactive)
  (let ((buf (get-buffer-create "*currency*")))
    (pop-to-buffer buf)
    (unless (derived-mode-p 'fate-currency-mode)
      (fate-currency-mode))
    (fate/currency--render)
    (fate/currency--goto-row (fate/currency--base-code))))

(provide 'fate-currency)
;;; fate-currency.el ends here
