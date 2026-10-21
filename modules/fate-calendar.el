;;; fate-calendar.el --- Calendar with China/Japan public holidays -*- lexical-binding: t -*-

;;; Commentary:
;; Pull China and Japan public holidays from Google's public holiday
;; calendars, and China's 调休 make-up working days (上班) from
;; NateScarlet/holiday-cn, as iCal feeds.  Cache them under
;; `fate-cache-directory', list them with the other holidays (`h' / `a'
;; in the calendar), and mark them in the calendar with a per-feed face.
;; Observances are skipped.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'holidays)
(require 'icalendar-parser)
(require 'icalendar-macs)
(require 'time-date)

(use-package plz)
(declare-function plz "plz")

(defface fate-calendar-cn-holiday
  '((t :inherit error :weight bold))
  "Face for China public holidays in the calendar."
  :group 'calendar-faces)

(defface fate-calendar-cn-workday
  '((t :inherit shadow :weight bold :box (:line-width -1)))
  "Face for China 调休 make-up working days in the calendar."
  :group 'calendar-faces)

(defface fate-calendar-jp-holiday
  '((t :inherit success :weight bold))
  "Face for Japan public holidays in the calendar."
  :group 'calendar-faces)

(defface fate-calendar-shared-holiday
  '((t :inherit fate-calendar-cn-holiday :underline t))
  "Face for days that are public holidays in both China and Japan."
  :group 'calendar-faces)

(defconst fate-calendar-feeds
  '((cn-holiday "CN" "https://calendar.google.com/calendar/ical/zh_cn.china%23holiday%40group.v.calendar.google.com/public/basic.ics"
                fate-calendar-cn-holiday (description . "公众假期"))
    (jp-holiday "JP" "https://calendar.google.com/calendar/ical/ja.japanese%23holiday%40group.v.calendar.google.com/public/basic.ics"
                fate-calendar-jp-holiday (description . "祝日"))
    (cn-workday "CN" "https://cdn.jsdelivr.net/gh/NateScarlet/holiday-cn@master/holiday-cn.ics"
                fate-calendar-cn-workday (summary . "上班")))
  "Feeds as (NAME LABEL URL FACE (PROPERTY . PREFIX)).
Only events whose PROPERTY (`summary' or `description') starts with
PREFIX are kept.  The Google URLs' locale prefix (zh_cn., ja.) sets the
language of the names, and with it how public holidays are tagged.")

(defvar fate-calendar--holidays nil
  "Alist of (NAME . ((DATE TEXT) ...)) parsed from the cached feeds.")

(defvar fate-calendar-hidden-countries nil
  "Labels (see `fate-calendar-feeds') whose entries are neither marked nor listed.")

(defun fate-calendar--shown ()
  "Entries of `fate-calendar--holidays' whose country is not hidden."
  (seq-remove (lambda (e)
                (member (nth 1 (assq (car e) fate-calendar-feeds))
                        fate-calendar-hidden-countries))
              fate-calendar--holidays))

(defun fate-calendar--cache-file (name)
  (expand-file-name (format "holidays-%s.ics" name) fate-cache-directory))

(defun fate-calendar--parse (file label filter)
  "Return the events in iCal FILE matching FILTER as ((M D Y) TEXT) entries.
An event spanning several days yields one entry per day."
  (when-let* ((cal (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8))
                       (insert-file-contents file))
                     (icalendar-parse))))
    (icalendar-with-component cal ((icalendar-vevent :all events))
      (let (res)
        (dolist (ev events res)
          (icalendar-with-component ev
              ((icalendar-dtstart :value start :value-type type)
               (icalendar-dtend :value end)
               (icalendar-summary :value summary)
               (icalendar-description :value desc))
            (when (and (eq type 'icalendar-date)
                       (string-prefix-p (cdr filter)
                                        (or (if (eq (car filter) 'summary) summary desc) "")))
              ;; DTEND is exclusive.
              (let ((a (calendar-absolute-from-gregorian start)))
                (dotimes (i (if end (- (calendar-absolute-from-gregorian end) a) 1))
                  (push (list (calendar-gregorian-from-absolute (+ a i))
                              (format "[%s] %s" label summary))
                        res))))))))))

(defun fate-calendar--load ()
  "Parse every cached feed and redraw the calendar if it is open."
  (setq fate-calendar--holidays
        (mapcar (pcase-lambda (`(,name ,label ,_ ,_ ,filter))
                  (let ((file (fate-calendar--cache-file name)))
                    (cons name (and (file-exists-p file)
                                    (fate-calendar--parse file label filter)))))
                fate-calendar-feeds))
  (when-let* ((buf (get-buffer calendar-buffer)))
    (with-current-buffer buf (calendar-redraw))))

(defun fate-calendar-update-holidays (&optional force)
  "Download holiday feeds older than 30 days (all of them with FORCE)."
  (interactive (list t))
  (pcase-dolist (`(,name ,_ ,url . ,_) fate-calendar-feeds)
    (let ((file (fate-calendar--cache-file name)))
      (when (or force
                (not (file-exists-p file))
                (time-less-p (file-attribute-modification-time (file-attributes file))
                             (time-subtract nil (days-to-time 30))))
        (plz 'get url
          :as 'file
          :then (lambda (tmp)
                  (rename-file tmp file t)
                  (fate-calendar--load))
          :else (lambda (err)
                  (message "Holiday feed %s failed: %s" name
                           (or (plz-error-message err)
                               (plz-error-curl-error err)
                               (plz-response-status (plz-error-response err))))))))))

(defun fate-calendar-holiday-list ()
  "Feed entries visible in the calendar, for `calendar-holidays'."
  (holiday-filter-visible-calendar
   (apply #'append (mapcar #'cdr (fate-calendar--shown)))))

(defun fate-calendar-mark-holidays (&rest _)
  "Re-mark feed entries with their feed's face over the generic one.
Runs after `calendar-mark-holidays', i.e. on every redraw and on \\`x'."
  (with-current-buffer (calendar-get-buffer)
    (let ((days (make-hash-table :test #'equal)))
      (pcase-dolist (`(,name . ,hs) (fate-calendar--shown))
        (let ((face (nth 3 (assq name fate-calendar-feeds))))
          (dolist (h (holiday-filter-visible-calendar hs))
            (cl-pushnew face (gethash (car h) days)))))
      (maphash
       (lambda (date faces)
         (save-excursion
           (calendar-cursor-to-visible-date date)
           (let ((ov (make-overlay (1- (point)) (1+ (point)))))
             ;; Beat the `holiday' overlay `calendar-mark-holidays' left here.
             (overlay-put ov 'priority 10)
             ;; Delete it once a redraw erases the buffer under it.
             (overlay-put ov 'evaporate t)
             (overlay-put ov 'face
                          (cond ((memq 'fate-calendar-cn-workday faces)
                                 'fate-calendar-cn-workday)
                                ((cdr faces) 'fate-calendar-shared-holiday)
                                (t (car faces)))))))
       days))))

(defun fate-calendar-toggle-country (label)
  "Show or hide the holidays of country LABEL, then redraw the calendar."
  (setq fate-calendar-hidden-countries
        (if (member label fate-calendar-hidden-countries)
            (delete label fate-calendar-hidden-countries)
          (cons label fate-calendar-hidden-countries)))
  ;; US holidays are Emacs's built-in `holiday-general-holidays'.
  (when (equal label "US")
    (setq calendar-holidays
          (if (member label fate-calendar-hidden-countries)
              (seq-difference calendar-holidays holiday-general-holidays)
            (append holiday-general-holidays calendar-holidays))))
  (with-current-buffer (calendar-get-buffer) (calendar-redraw)))

(defun fate-calendar--toggle-description (label name)
  (format "%s %s" (if (member label fate-calendar-hidden-countries) "[ ]" "[x]") name))

(use-package calendar
  :ensure nil
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1)
  ;; nano-calendar look: month name on a full-width bar.
  (calendar-month-header
   '(let* ((s (format "%s %d" (calendar-month-name month) year))
           (pad (- 20 (length s))))
      (propertize (concat (make-string (/ pad 2) ?\s) s
                          (make-string (- pad (/ pad 2)) ?\s))
                  'font-lock-face 'calendar-month-header)))
  :custom-face
  (calendar-month-header ((t :inherit (bold highlight))))
  (calendar-weekday-header ((t :inherit default :underline t)))
  (calendar-weekend-header ((t :inherit shadow :underline t)))
  (calendar-today ((t :inverse-video t :weight bold)))
  :hook
  (calendar-today-visible . calendar-mark-today)
  ;; `calendar-mode' runs before the window is fitted, so the fit sees this.
  (calendar-mode . (lambda ()
                     (text-scale-set 1.1)
                     (setq-local line-spacing 0.1)))
  :config
  (advice-add 'calendar-mark-holidays :after #'fate-calendar-mark-holidays)
  ;; Only US (general) holidays from Emacs; the feeds cover CN/JP.  Drops
  ;; the religious, Chinese and solar groups (e.g. a duplicate 春节).
  (setq calendar-holidays
        (append holiday-general-holidays '((fate-calendar-holiday-list))))
  (fate-calendar--load)
  (fate-calendar-update-holidays))

(use-package casual-calendar
  :ensure nil
  :bind (:map calendar-mode-map ("C-o" . casual-calendar))
  :config
  (transient-append-suffix 'casual-calendar-tmenu "u"
    '("C" (lambda () (interactive) (fate-calendar-toggle-country "CN"))
      :description (lambda () (fate-calendar--toggle-description "CN" "China"))
      :transient t))
  (transient-append-suffix 'casual-calendar-tmenu "C"
    '("J" (lambda () (interactive) (fate-calendar-toggle-country "JP"))
      :description (lambda () (fate-calendar--toggle-description "JP" "Japan"))
      :transient t))
  (transient-append-suffix 'casual-calendar-tmenu "J"
    '("U" (lambda () (interactive) (fate-calendar-toggle-country "US"))
      :description (lambda () (fate-calendar--toggle-description "US" "US"))
      :transient t)))

(provide 'fate-calendar)
;;; fate-calendar.el ends here
