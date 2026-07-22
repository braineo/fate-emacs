;;; fate-worldtime.el --- Text-mode world time comparison -*- lexical-binding: t -*-

;;; Commentary:
;; A text version of https://www.worldtimebuddy.com.  `fate/worldtime'
;; opens a buffer that compares several timezones side by side: one row
;; per zone with a 24-hour strip, aligned by absolute time so a vertical
;; column is a single shared moment.  Scrub the reference time with the
;; arrow keys (or f/b) and every zone updates together; the offset of each
;; zone versus the local ("home") zone is shown on each row.
;;
;; The compared zones come from the built-in `world-clock-list' (falling
;; back to `zoneinfo-style-world-list' when it is t), the same variables
;; that drive `M-x world-clock'.  The home zone (marked with ⌂) is the
;; system's local zone; if it already appears in the list that row is used,
;; otherwise a row for it is added.  Any row can be chosen as the "base"
;; zone (marked ▸, moved with the up/down keys): all ahead/behind offsets
;; and the time in the title are relative to it.

;;; Code:

(require 'time)
(require 'time-date)
(require 'calendar)
(require 'seq)

(declare-function org-read-date "org")

;; All faces inherit from standard, theme-defined faces so the display
;; picks up the active theme's palette automatically.

(defface fate-worldtime-title
  '((t :inherit (bold font-lock-function-name-face)))
  "Face for the title line."
  :group 'fate-worldtime)

(defface fate-worldtime-hint
  '((t :inherit shadow))
  "Face for the key-hint line and title separators."
  :group 'fate-worldtime)

(defface fate-worldtime-selected
  '((t :inherit highlight :weight bold))
  "Face for the currently selected time column."
  :group 'fate-worldtime)

(defface fate-worldtime-home
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the home (local) timezone label."
  :group 'fate-worldtime)

(defface fate-worldtime-label
  '((t :inherit font-lock-function-name-face))
  "Face for ordinary timezone labels."
  :group 'fate-worldtime)

(defface fate-worldtime-abbrev
  '((t :inherit font-lock-type-face))
  "Face for the timezone abbreviation (e.g. JST)."
  :group 'fate-worldtime)

(defface fate-worldtime-time
  '((t :inherit font-lock-string-face))
  "Face for the per-zone date and time."
  :group 'fate-worldtime)

(defface fate-worldtime-ahead
  '((t :inherit success))
  "Face for a positive (ahead-of-base) offset."
  :group 'fate-worldtime)

(defface fate-worldtime-behind
  '((t :inherit error))
  "Face for a negative (behind-base) offset."
  :group 'fate-worldtime)

(defface fate-worldtime-neutral
  '((t :inherit font-lock-comment-face))
  "Face for the base row's zero offset."
  :group 'fate-worldtime)

(defface fate-worldtime-business
  '((t :inherit font-lock-constant-face))
  "Face for business-hour cells (09-17)."
  :group 'fate-worldtime)

(defface fate-worldtime-fringe
  '((t :inherit font-lock-comment-face))
  "Face for early-morning and evening hour cells."
  :group 'fate-worldtime)

(defface fate-worldtime-night
  '((t :inherit shadow))
  "Face for nighttime hour cells."
  :group 'fate-worldtime)

(defface fate-worldtime-day-boundary
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the midnight (day-boundary) hour cell."
  :group 'fate-worldtime)

(defvar-local fate-worldtime--ref nil
  "The currently selected absolute instant (a Lisp time value).")

(defvar-local fate-worldtime--base nil
  "Index into the entry list of the base zone.
Ahead/behind offsets and the title time are computed relative to it.
Defaults to the home (local) row.")

;;; Helpers

(defun fate/worldtime--truncate-hour (time)
  "Return TIME truncated to the start of its hour (local zone)."
  (let ((d (decode-time time)))
    (setf (decoded-time-second d) 0
          (decoded-time-minute d) 0)
    (encode-time d)))

(defun fate/worldtime--zones ()
  "Return the compared zones as a list of (ZONE . LABEL) conses.
Read from `world-clock-list', or `zoneinfo-style-world-list' when it is t."
  (let ((list (if (listp world-clock-list)
                  world-clock-list
                zoneinfo-style-world-list)))
    (mapcar (lambda (e) (cons (nth 0 e) (nth 1 e))) list)))

(defun fate/worldtime--same-as-local-p (zone)
  "Non-nil if ZONE currently matches the local zone's offset and abbrev."
  (and zone
       (= (decoded-time-zone (decode-time fate-worldtime--ref zone))
          (decoded-time-zone (decode-time fate-worldtime--ref)))
       (string= (format-time-string "%Z" fate-worldtime--ref zone)
                (format-time-string "%Z" fate-worldtime--ref))))

(defun fate/worldtime--home-p (zone)
  "Non-nil if ZONE is the home (local) zone.
A nil ZONE is the synthesized local row."
  (or (null zone) (fate/worldtime--same-as-local-p zone)))

(defun fate/worldtime--home-zone-name ()
  "Return a label for the local zone, used when it is not in the list."
  (or (getenv "TZ")
      (format-time-string "%Z" fate-worldtime--ref)
      "Local"))

(defun fate/worldtime--entries ()
  "Return the rows as (ZONE . LABEL) conses, ensuring a home row exists.
If the local zone is already in the list, that row is the home row;
otherwise a synthesized local row (ZONE nil) is prepended."
  (let ((zones (fate/worldtime--zones)))
    (if (seq-some (lambda (e) (fate/worldtime--same-as-local-p (car e))) zones)
        zones
      (cons (cons nil (fate/worldtime--home-zone-name)) zones))))

(defun fate/worldtime--home-index (entries)
  "Return the index of the home row within ENTRIES (0 if none found)."
  (let ((home (seq-find (lambda (e) (fate/worldtime--home-p (car e))) entries)))
    (or (and home (seq-position entries home #'eq)) 0)))

(defun fate/worldtime--window-start (zone)
  "Return midnight, in ZONE, of the day containing `fate-worldtime--ref'."
  (let ((d (decode-time fate-worldtime--ref zone)))
    (setf (decoded-time-second d) 0
          (decoded-time-minute d) 0
          (decoded-time-hour d) 0)
    (encode-time d)))

(defun fate/worldtime--selected-index (ws)
  "Return the 0-based strip column of `fate-worldtime--ref' given window WS."
  (round (/ (float-time (time-subtract fate-worldtime--ref ws)) 3600)))

(defun fate/worldtime--offset-minutes (zone base-zone)
  "Return ZONE's UTC offset minus BASE-ZONE's, in minutes, at the ref."
  (/ (- (decoded-time-zone (decode-time fate-worldtime--ref zone))
        (decoded-time-zone (decode-time fate-worldtime--ref base-zone)))
     60))

(defun fate/worldtime--format-offset (minutes)
  "Format MINUTES as a signed offset label like \"+1\" or \"-9:30\"."
  (if (zerop minutes)
      "0"
    (let* ((sign (if (< minutes 0) "-" "+"))
           (m (abs minutes))
           (h (/ m 60))
           (mm (% m 60)))
      (if (zerop mm)
          (format "%s%d" sign h)
        (format "%s%d:%02d" sign h mm)))))

;;; Rendering

(defun fate/worldtime--insert-row (entry base-zone home-p base-p ws sel width)
  "Insert the two lines for ENTRY (ZONE . LABEL).
BASE-ZONE is the base zone offsets are measured against.  HOME-P marks the
home row, BASE-P the base row.  WS is the window-start instant, SEL the
selected column, WIDTH the label column width."
  (let* ((zone (car entry))
         (label (or (cdr entry) ""))
         (marker (concat (if base-p (propertize "▸" 'face 'fate-worldtime-selected) " ")
                         (if home-p (propertize "⌂" 'face 'fate-worldtime-home) " ")))
         (abbrev (propertize (format-time-string "%Z" fate-worldtime--ref zone)
                             'face 'fate-worldtime-abbrev))
         (mins (unless base-p (fate/worldtime--offset-minutes zone base-zone)))
         (offstr (cond (base-p (propertize "±0" 'face 'fate-worldtime-neutral))
                       ((> mins 0) (propertize (fate/worldtime--format-offset mins)
                                               'face 'fate-worldtime-ahead))
                       ((< mins 0) (propertize (fate/worldtime--format-offset mins)
                                               'face 'fate-worldtime-behind))
                       (t (propertize (fate/worldtime--format-offset mins)
                                      'face 'fate-worldtime-neutral))))
         (timestr (propertize (format-time-string "%a %F %H:%M" fate-worldtime--ref zone)
                              'face 'fate-worldtime-time))
         ;; A face list merges earlier-wins per attribute.  Put `home' first
         ;; so its foreground shows through, while `selected' (which only adds
         ;; a background via `highlight') still supplies the highlight.
         (labelface (append (and home-p '(fate-worldtime-home))
                            (and base-p '(fate-worldtime-selected))
                            (and (not home-p) (not base-p) '(fate-worldtime-label)))))
    ;; Header line.
    (insert (format "%s %s %-5s %-6s %s\n"
                    marker
                    (string-pad (propertize label 'face labelface) width)
                    abbrev offstr timestr))
    ;; Hour strip.
    (insert "     ")
    (dotimes (j 24)
      (let* ((instant (time-add ws (* j 3600)))
             (hour (string-to-number (format-time-string "%H" instant zone)))
             (face (cond ((= j sel) 'fate-worldtime-selected)
                         ((= hour 0) 'fate-worldtime-day-boundary)
                         ((<= 9 hour 17) 'fate-worldtime-business)
                         ((or (<= 7 hour 8) (<= 18 hour 21)) 'fate-worldtime-fringe)
                         (t 'fate-worldtime-night))))
        ;; Separator stays unfaced so the highlight covers only the digits.
        (insert " ")
        (insert (propertize (format "%02d" hour) 'face face))))
    (insert "\n\n")))

(defun fate/worldtime--render ()
  "Render the world time grid into the current buffer."
  (let* ((inhibit-read-only t)
         (line (line-number-at-pos))
         (entries (fate/worldtime--entries))
         (count (length entries))
         (base (min (or fate-worldtime--base (fate/worldtime--home-index entries))
                    (1- count)))
         (base-entry (nth base entries))
         (base-zone (car base-entry))
         (base-label (or (cdr base-entry) ""))
         (ws (fate/worldtime--window-start base-zone))
         (sel (fate/worldtime--selected-index ws))
         (width (apply #'max 6 (mapcar (lambda (e) (length (or (cdr e) "")))
                                       entries)))
         (index 0))
    (setq fate-worldtime--base base)
    (erase-buffer)
    (let ((sep (propertize "  —  " 'face 'fate-worldtime-hint)))
      (insert (propertize "World Time" 'face 'fate-worldtime-title) sep
              (propertize (format-time-string "%A %d %B %Y  %H:%M %Z"
                                              fate-worldtime--ref base-zone)
                          'face 'fate-worldtime-time)
              sep
              (propertize base-label 'face 'fate-worldtime-label)
              "\n"))
    (insert (propertize
             (concat "  f/b or ←/→: ±hour   ↑/↓: base zone   n/p: ±day   "
                     ".: pick   t: now   g: refresh   q: quit\n\n")
             'face 'fate-worldtime-hint))
    (dolist (entry entries)
      (fate/worldtime--insert-row entry base-zone
                                  (fate/worldtime--home-p (car entry))
                                  (= index base) ws sel width)
      (setq index (1+ index)))
    (goto-char (point-min))
    (forward-line (1- line))))

;;; Commands

(defun fate/worldtime-forward-hour ()
  "Move the reference time forward by one hour."
  (interactive)
  (setq fate-worldtime--ref (time-add fate-worldtime--ref 3600))
  (fate/worldtime--render))

(defun fate/worldtime-backward-hour ()
  "Move the reference time back by one hour."
  (interactive)
  (setq fate-worldtime--ref (time-subtract fate-worldtime--ref 3600))
  (fate/worldtime--render))

(defun fate/worldtime-next-day ()
  "Move the reference time forward by one day."
  (interactive)
  (setq fate-worldtime--ref (time-add fate-worldtime--ref 86400))
  (fate/worldtime--render))

(defun fate/worldtime-prev-day ()
  "Move the reference time back by one day."
  (interactive)
  (setq fate-worldtime--ref (time-subtract fate-worldtime--ref 86400))
  (fate/worldtime--render))

(defun fate/worldtime-set-time ()
  "Set the reference (local) time by picking it from the calendar.
Uses `org-read-date': the popup calendar opens on the current reference
date with today marked; navigate with the arrow keys, or type a date and
time such as \"2026-12-25 14:00\", \"+3d\", or \"fri\"."
  (interactive)
  (require 'org)
  ;; Highlight today with the theme's `calendar-today' face while picking.
  (let ((calendar-today-visible-hook
         (cons #'calendar-mark-today calendar-today-visible-hook)))
    (setq fate-worldtime--ref
          (org-read-date t t nil "Reference time" fate-worldtime--ref)))
  (fate/worldtime--render))

(defun fate/worldtime--rebase (delta)
  "Move the base zone by DELTA rows.
The base zone's wall-clock reading is kept constant (the absolute moment
shifts), so the highlighted column does not jump."
  (let* ((entries (fate/worldtime--entries))
         (count (length entries))
         (old (min (or fate-worldtime--base (fate/worldtime--home-index entries))
                   (1- count)))
         (new (mod (+ old delta) count))
         ;; The wall clock currently shown in the old base zone.
         (dec (decode-time fate-worldtime--ref (car (nth old entries)))))
    ;; Reinterpret that same wall clock in the new base zone.
    (setf (decoded-time-dst dec) -1
          (decoded-time-zone dec) (car (nth new entries)))
    (setq fate-worldtime--ref (encode-time dec)
          fate-worldtime--base new))
  (fate/worldtime--render))

(defun fate/worldtime-next-zone ()
  "Move the base zone to the next row, keeping the selected hour."
  (interactive)
  (fate/worldtime--rebase 1))

(defun fate/worldtime-prev-zone ()
  "Move the base zone to the previous row, keeping the selected hour."
  (interactive)
  (fate/worldtime--rebase -1))

(defun fate/worldtime-now ()
  "Reset the reference time to the current hour."
  (interactive)
  (setq fate-worldtime--ref (fate/worldtime--truncate-hour (current-time)))
  (fate/worldtime--render))

(defun fate/worldtime-refresh ()
  "Redraw the grid, re-reading `world-clock-list'."
  (interactive)
  (fate/worldtime--render))

(defun fate/worldtime-kill ()
  "Copy the base (selected) and home zone times to the kill ring.
Produces one ready-to-send line per zone at the current reference, e.g.:

  Wed 2026-07-22 05:00 PDT
  Wed 2026-07-22 20:00 CST"
  (interactive)
  (let* ((entries (fate/worldtime--entries))
         (base-zone (car (nth (min (or fate-worldtime--base
                                       (fate/worldtime--home-index entries))
                                   (1- (length entries)))
                              entries)))
         (home-zone (car (nth (fate/worldtime--home-index entries) entries)))
         (text (mapconcat
                (lambda (zone)
                  (format-time-string "%a %F %H:%M %Z" fate-worldtime--ref zone))
                (delete-dups (list base-zone home-zone))
                "\n")))
    (kill-new text)
    (message "Copied to kill ring:\n%s" text)))

(defvar fate-worldtime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'fate/worldtime-forward-hour)
    (define-key map (kbd "l") #'fate/worldtime-forward-hour)
    (define-key map (kbd "<right>") #'fate/worldtime-forward-hour)
    (define-key map (kbd "b") #'fate/worldtime-backward-hour)
    (define-key map (kbd "h") #'fate/worldtime-backward-hour)
    (define-key map (kbd "<left>") #'fate/worldtime-backward-hour)
    (define-key map (kbd "<down>") #'fate/worldtime-next-zone)
    (define-key map (kbd "j") #'fate/worldtime-next-zone)
    (define-key map (kbd "<up>") #'fate/worldtime-prev-zone)
    (define-key map (kbd "k") #'fate/worldtime-prev-zone)
    (define-key map (kbd "n") #'fate/worldtime-next-day)
    (define-key map (kbd "p") #'fate/worldtime-prev-day)
    (define-key map (kbd ".") #'fate/worldtime-set-time)
    (define-key map (kbd "t") #'fate/worldtime-now)
    (define-key map (kbd "g") #'fate/worldtime-refresh)
    (define-key map (kbd "w") #'fate/worldtime-kill)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `fate-worldtime-mode'.")

(define-derived-mode fate-worldtime-mode special-mode "WorldTime"
  "Major mode for comparing the time across timezones.

\\{fate-worldtime-mode-map}"
  (setq truncate-lines t)
  (setq fate-worldtime--ref (fate/worldtime--truncate-hour (current-time))))

;;;###autoload
(defun fate/worldtime ()
  "Open a buffer comparing the current time across timezones."
  (interactive)
  (let ((buf (get-buffer-create "*worldtime*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'fate-worldtime-mode)
        (fate-worldtime-mode))
      (fate/worldtime--render))
    (pop-to-buffer buf)))

(provide 'fate-worldtime)
;;; fate-worldtime.el ends here
