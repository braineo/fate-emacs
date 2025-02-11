;;; fate-rg.el --- Modern ripgrep interface with transient -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (consult "0.34"))
;; Keywords: search, tools
;; URL: https://github.com/yourusername/fate-rg

;;; Commentary:

;; A modern ripgrep interface for Emacs with transient UI and live preview.

;;; Code:

(require 'transient)
(require 'consult)
(require 'project)

(defgroup fate-rg nil
  "Modern ripgrep interface with transient UI."
  :group 'tools)

(defface fate-rg-file-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for file names in results.")

(defface fate-rg-match-count
  '((t :inherit font-lock-comment-face))
  "Face for match counts in results.")

(defface fate-rg-match
  '((t :inherit highlight))
  "Face for matched text.")

(defface fate-rg-replace-old
  '((t :inherit diff-removed :strike-through t))
  "Face for text to be replaced.")

(defface fate-rg-replace-new
  '((t :inherit diff-added))
  "Face for replacement text.")

;; Variables
(defvar fate-rg--search-string ""
  "Current search string.")

(defvar fate-rg--replace-string ""
  "Current replace string.")

(defvar fate-rg--case-sensitive nil
  "Whether search is case sensitive.")

(defvar fate-rg--exact-match nil
  "Whether to search for exact matches.")

(defvar fate-rg--regexp nil
  "Whether to use regular expressions.")

(defvar fate-rg--preserve-case nil
  "Whether to preserve case in replacements.")

(defvar fate-rg--include-patterns nil
  "File patterns to include.")

(defvar fate-rg--exclude-patterns nil
  "File patterns to exclude.")

(defvar fate-rg--search-buffers nil
  "Whether to search only in open buffers.")

(defvar fate-rg--respect-ignore nil
  "Whether to respect .gitignore and similar files.")

(defvar fate-rg--search-history nil
  "History for search strings.")

(defvar fate-rg--replace-history nil
  "History for replacement strings.")

(defvar fate-rg--glob-history nil
  "History for glob patterns.")

;; Core search functions
(defun fate-rg--build-command ()
  "Build the rg command based on current settings."
  (let ((cmd '("rg" "--color=always" "--line-number" "--no-heading")))
    (when fate-rg--case-sensitive
      (push "--case-sensitive" cmd))
    (when fate-rg--exact-match
      (push "--fixed-strings" cmd))
    (unless fate-rg--regexp
      (push "--fixed-strings" cmd))
    (when fate-rg--include-patterns
      (dolist (pattern fate-rg--include-patterns)
        (push (format "--glob=%s" pattern) cmd)))
    (when fate-rg--exclude-patterns
      (dolist (pattern fate-rg--exclude-patterns)
        (push (format "--glob=!%s" pattern) cmd)))
    (unless fate-rg--respect-ignore
      (push "--no-ignore" cmd))
    (push fate-rg--search-string cmd)
    (when fate-rg--search-buffers
      (setq cmd (append cmd (mapcar #'buffer-file-name
                                   (seq-filter #'buffer-file-name
                                             (buffer-list))))))
    (string-join (nreverse cmd) " ")))

(defun fate-rg--get-results ()
  "Get search results using ripgrep."
  (when (and fate-rg--search-string
             (not (string-empty-p fate-rg--search-string)))
    (let* ((default-directory (if (project-current)
                                 (project-root (project-current))
                               default-directory))
           (command (fate-rg--build-command))
           (output (shell-command-to-string command)))
      (fate-rg--process-results output))))

(defun fate-rg--process-results (output)
  "Process raw ripgrep OUTPUT into structured data."
  (let ((results (make-hash-table :test 'equal))
        (current-file nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)" line)
        (let* ((file (match-string 1 line))
               (line-num (string-to-number (match-string 2 line)))
               (content (match-string 3 line))
               (file-results (or (gethash file results)
                               (puthash file nil results))))
          (push (cons line-num content) file-results)
          (puthash file file-results results))))
    results))

(defun fate-rg--preserve-case (original replacement)
  "Preserve case of ORIGINAL in REPLACEMENT."
  (cond
   ((string-match-p "^[[:upper:]]+$" original)
    (upcase replacement))
   ((string-match-p "^[[:upper:]][[:lower:]]+$" original)
    (capitalize replacement))
   (t replacement)))

;; Autoloaded entry point
;;;###autoload
(defun fate-rg ()
  "Start fate-rg interface."
  (interactive)
  (fate-rg-transient))

;;;
;;; Transient
;;;



;; Define transient classes for our input commands
(transient-define-argument fate-rg--arg-search ()
  :description "Search string"
  :class 'transient-option
  :shortarg "-s"
  :argument "--search=")

(transient-define-argument fate-rg--arg-replace ()
  :description "Replace string"
  :class 'transient-option
  :shortarg "-r"
  :argument "--replace=")

(transient-define-argument fate-rg--arg-glob ()
  :description "Include pattern"
  :class 'transient-option
  :shortarg "-i"
  :argument "--include=")

;; Define infix commands
(transient-define-infix fate-rg--set-search ()
  :class 'transient-option
  :description "Search string"
  :shortarg "-s"
  :argument "--search="
  :reader (lambda (prompt _initial-input _history)
            (setq fate-rg--search-string
                  (read-string (format "%s: " prompt)
                             fate-rg--search-string
                             'fate-rg--search-history))
            fate-rg--search-string))

(transient-define-infix fate-rg--set-replace ()
  :class 'transient-option
  :description "Replace string"
  :shortarg "-r"
  :argument "--replace="
  :reader (lambda (prompt _initial-input _history)
            (setq fate-rg--replace-string
                  (read-string (format "%s: " prompt)
                             fate-rg--replace-string
                             'fate-rg--replace-history))
            fate-rg--replace-string))

(transient-define-infix fate-rg--set-glob ()
  :class 'transient-option
  :description "Include pattern"
  :shortarg "-i"
  :argument "--include="
  :reader (lambda (prompt _initial-input _history)
            (let ((pattern (read-string (format "%s: " prompt)
                                      nil
                                      'fate-rg--glob-history)))
              (unless (string-empty-p pattern)
                (push pattern fate-rg--include-patterns))
              pattern)))

;; Toggle commands
(defun fate-rg--toggle-case ()
  "Toggle case sensitivity."
  (interactive)
  (setq fate-rg--case-sensitive (not fate-rg--case-sensitive))
  (fate-rg-preview))

(defun fate-rg--toggle-exact ()
  "Toggle exact match."
  (interactive)
  (setq fate-rg--exact-match (not fate-rg--exact-match))
  (fate-rg-preview))

(defun fate-rg--toggle-regexp ()
  "Toggle regular expression search."
  (interactive)
  (setq fate-rg--regexp (not fate-rg--regexp))
  (fate-rg-preview))

(defun fate-rg--toggle-preserve-case ()
  "Toggle case preservation in replacements."
  (interactive)
  (setq fate-rg--preserve-case (not fate-rg--preserve-case))
  (fate-rg-preview))

(defun fate-rg--toggle-buffers ()
  "Toggle searching in open buffers only."
  (interactive)
  (setq fate-rg--search-buffers (not fate-rg--search-buffers))
  (fate-rg-preview))

(defun fate-rg--toggle-ignore ()
  "Toggle respecting ignore files."
  (interactive)
  (setq fate-rg--respect-ignore (not fate-rg--respect-ignore))
  (fate-rg-preview))

(transient-define-prefix fate-rg-transient ()
  "Modern ripgrep interface."
  :value '("--smart-case")
  ["Search Options"
   :description (lambda ()
                 (format "Search: %s Replace: %s"
                         (propertize fate-rg--search-string 'face 'transient-value)
                         (propertize fate-rg--replace-string 'face 'transient-value)))
   ("s" fate-rg--set-search)
   ("r" fate-rg--set-replace)
   ("c" "Case sensitive" fate-rg--toggle-case
    :toggle fate-rg--case-sensitive)
   ("e" "Exact match" fate-rg--toggle-exact
    :toggle fate-rg--exact-match)
   ("x" "Regular expression" fate-rg--toggle-regexp
    :toggle fate-rg--regexp)
   ("p" "Preserve case" fate-rg--toggle-preserve-case
    :toggle fate-rg--preserve-case)]

  ["File Options"
   ("i" fate-rg--set-glob)
   ("b" "Search buffers" fate-rg--toggle-buffers
    :toggle fate-rg--search-buffers)
   ("g" "Respect ignore files" fate-rg--toggle-ignore
    :toggle fate-rg--respect-ignore)]

  ["Actions"
   ("RET" "Execute" fate-rg-execute)
   ("P" "Preview" fate-rg-preview)])

;;;
;;; Preview
;;;


(defvar fate-rg-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'fate-rg-preview-visit)
    (define-key map (kbd "e") #'fate-rg-preview-exclude)
    (define-key map (kbd "n") #'fate-rg-preview-next)
    (define-key map (kbd "p") #'fate-rg-preview-previous)
    (define-key map (kbd "q") #'fate-rg-preview-quit)
    map)
  "Keymap for `fate-rg-preview-mode'.")

(define-derived-mode fate-rg-preview-mode special-mode "fate-rg-preview"
  "Major mode for fate-rg preview buffer."
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local truncate-lines t)
  ;; Setup mouse hover effects
  (add-hook 'post-command-hook #'fate-rg--update-hover-buttons nil t))

(defun fate-rg--update-hover-buttons ()
  "Update button visibility based on mouse position."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(mouse-face))
    (when-let ((pos (mouse-position)))
      (let* ((mouse-row (cdr (posn-col-row (mouse-position))))
             (window-edges (window-inside-edges))
             (relative-y (- (cdr pos) (nth 1 window-edges))))
        (save-excursion
          (goto-char (window-start))
          (vertical-motion relative-y)
          (when (looking-at "^▸ \\(.*\\) \\[")
            (add-text-properties
             (line-beginning-position)
             (line-end-position)
             `(mouse-face highlight
               help-echo "Click to toggle file"
               keymap ,(let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1]
                          #'fate-rg-preview-toggle-file)
                        map)))))))))

(defun fate-rg--format-preview (results)
  "Format RESULTS for preview buffer."
  (with-temp-buffer
    (maphash
     (lambda (file matches)
       (let ((count (length matches)))
         ;; File header
         (insert (propertize (format "▸ %s " file)
                            'face 'fate-rg-file-name)
                 (propertize (format "[%d match%s]\n"
                                   count
                                   (if (= count 1) "" "es"))
                            'face 'fate-rg-match-count))
         ;; Matches
         (dolist (match (sort matches (lambda (a b) (< (car a) (car b)))))
           (let* ((line-num (car match))
                  (content (cdr match))
                  (replaced-content
                   (when (and fate-rg--replace-string
                             (not (string-empty-p fate-rg--replace-string)))
                     (fate-rg--preview-replacement content))))
             (insert (format "  %4d: " line-num))
             (if replaced-content
                 (insert replaced-content)
               (insert (propertize content
                                 'face 'fate-rg-match)))
             (insert "\n")))))
     results)
    (buffer-string)))

(defun fate-rg--preview-replacement (line)
  "Preview replacement in LINE."
  (let* ((regexp (if fate-rg--regexp
                     fate-rg--search-string
                   (regexp-quote fate-rg--search-string)))
         (case-fold-search (not fate-rg--case-sensitive)))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((replacement (if fate-rg--preserve-case
                              (fate-rg--preserve-case
                               (match-string 0)
                               fate-rg--replace-string)
                            fate-rg--replace-string)))
          (add-text-properties (match-beginning 0) (match-end 0)
                             '(face fate-rg-replace-old))
          (insert (propertize replacement
                             'face 'fate-rg-replace-new))))
      (buffer-string))))

(defun fate-rg-preview ()
  "Update the preview buffer with search results."
  (interactive)
  (let ((buffer (get-buffer-create "*fate-rg-preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fate-rg-preview-mode)
        (save-excursion
          (insert (fate-rg--format-preview (fate-rg--get-results))))
        (goto-char (point-min))))
    (unless (get-buffer-window buffer)
      (display-buffer-in-side-window
       buffer
       '((side . right)
         (window-width . 0.4))))))

(defun fate-rg-preview-toggle-file ()
  "Toggle expansion of file under point."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\([▸▾]\\) \\(.*\\) \\[")
        (let ((expanded (string= (match-string 1) "▾"))
              (file (match-string 2))
              (start (point)))
          (replace-match (if expanded "▸" "▾") nil nil nil 1)
          (forward-line 1)
          (let ((file-content-start (point)))
            (while (and (not (eobp))
                       (looking-at "^  "))
              (forward-line 1))
            (if expanded
                (delete-region file-content-start (point))
              (let ((matches (gethash file (fate-rg--get-results))))
                (save-excursion
                  (goto-char file-content-start)
                  (dolist (match (sort matches (lambda (a b) (< (car a) (car b)))))
                    (insert (format "  %4d: %s\n"
                                  (car match)
                                  (cdr match)))))))))))))

(defun fate-rg-preview-visit ()
  "Visit the match at point."
  (interactive)
  (let* ((pos (point))
         (file nil)
         (line-num nil))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; If on a file line, visit the file
       ((looking-at "^[▸▾] \\(.*\\) \\[")
        (setq file (match-string 1)))
       ;; If on a match line, visit the specific match
       ((looking-at "^  \\([0-9]+\\):")
        (setq line-num (string-to-number (match-string 1)))
        (while (and (not (bobp))
                   (not (looking-at "^[▸▾] \\(.*\\) \\[")))
          (forward-line -1))
        (when (looking-at "^[▸▾] \\(.*\\) \\[")
          (setq file (match-string 1)))))
      (when file
        (find-file file)
        (when line-num
          (goto-char (point-min))
          (forward-line (1- line-num)))))))

(defun fate-rg-preview-exclude ()
  "Exclude the current match or file from results."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Exclude file
       ((looking-at "^[▸▾] \\(.*\\) \\[")
        (let ((file (match-string 1)))
          (push (file-name-nondirectory file) fate-rg--exclude-patterns)
          (fate-rg-preview)))
       ;; Exclude match line
       ((looking-at "^  \\([0-9]+\\):")
        (let ((line-num (string-to-number (match-string 1))))
          (kill-whole-line)
          (while (and (not (bobp))
                     (not (looking-at "^[▸▾]")))
            (forward-line -1))
          (when (looking-at "^[▸▾] .* \\[\\([0-9]+\\) match")
            (let ((count (1- (string-to-number (match-string 1)))))
              (replace-match (number-to-string count) nil nil nil 1)))))))))

(defun fate-rg-preview-next ()
  "Move to next match."
  (interactive)
  (forward-line)
  (while (and (not (eobp))
              (not (looking-at "^  \\([0-9]+\\):")))
    (forward-line)))

(defun fate-rg-preview-previous ()
  "Move to previous match."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (looking-at "^  \\([0-9]+\\):")))
    (forward-line -1)))

(defun fate-rg-preview-quit ()
  "Quit preview window."
  (interactive)
  (quit-window))


(provide 'fate-rg)

;;; fate-rg.el ends here
