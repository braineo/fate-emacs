;;; fate-delta.el --- Delta side-by-side diff viewer -*- lexical-binding: t -*-

;;; Commentary:
;; Provides `fate-delta-mode` to view ANSI colorized `delta --side-by-side`
;; outputs, and jump to the corresponding line in the original file.
;;
;; Provides integrations with Magit as well.

;;; Code:

(require 'ansi-color)
(require 'magit)
(require 'transient)

(defgroup fate-delta nil
  "Delta side-by-side viewer mode."
  :group 'tools)

(defun fate/delta--current-file ()
  "Return the file path from the delta header at or before point."
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward "^\\(?:Δ \\|added: \\|deleted: \\|removed: \\|copied: \\|renamed: .* ⟶ \\)\\(.+\\)$" nil t)
        (string-trim (match-string 1))
      (user-error "Could not find file header. Did you run delta with --file-style=plain?"))))

(defun fate/delta--current-line-numbers ()
  "Return a cons (LEFT-LINE . RIGHT-LINE) based on the delta output at point.
If a line number is not present, it is nil."
  (let ((line-str (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        left-line right-line)
    ;; Parse side-by-side line format using the unicode vertical pipe │
    ;; Example: "│  1  │a       │  1  │a"  or  "│     │        │  2  │modified"
    (when (string-match "^.*?│[ \t]*\\([0-9]+\\)?[ \t]*│.*?│[ \t]*\\([0-9]+\\)?[ \t]*│" line-str)
      (setq left-line (match-string 1 line-str))
      (setq right-line (match-string 2 line-str)))
    (cons (if left-line (string-to-number left-line) nil)
          (if right-line (string-to-number right-line) nil))))

(defun fate/delta-goto-file-at-point ()
  "Jump to the file and line number corresponding to the point in delta output."
  (interactive)
  (let ((col (current-column))
        (half-width (/ (window-width) 2))
        (lines (fate/delta--current-line-numbers))
        target-line target-file)
    ;; Determine which side the cursor is on
    (if (< col half-width)
        (setq target-line (car lines))
      (setq target-line (cdr lines)))

    ;; If the targeted side is empty (e.g., clicking on a newly added line on the left side),
    ;; default to the other available line.
    (unless target-line
      (setq target-line (or (car lines) (cdr lines))))

    (unless target-line
      (user-error "No valid line number found at point"))

    (setq target-file (fate/delta--current-file))
    (let* ((file-path (expand-file-name target-file (magit-toplevel)))
           (buf (find-file-other-window file-path)))
      (with-current-buffer buf
        (when target-line
          (goto-char (point-min))
          (forward-line (1- target-line))
          (recenter))))))

(defvar-local fate-delta--target-revision nil
  "The base branch or commit used to generate the delta output.")

(defvar-local fate-delta--saved-window-config nil
  "Window configuration saved before opening side-by-side files.")

(defvar fate-delta-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") 'fate/delta-review-quit)
    map))

(define-minor-mode fate-delta-review-mode
  "Minor mode enabled when reviewing files side-by-side from delta."
  :init-value nil
  :lighter " Delta-Review"
  :keymap fate-delta-review-mode-map)

(defun fate/delta--open-base-file (filename revision)
  "Open the base version of FILENAME at REVISION via git show.
Stores the file in /tmp and applies the correct major mode."
  (let* ((magit-root (magit-toplevel))
         (hash (string-trim (shell-command-to-string (format "git -C %s rev-parse %s"
                                                             (shell-quote-argument magit-root)
                                                             (shell-quote-argument revision)))))
         (repo-hash (md5 magit-root))
         (encoded-path (replace-regexp-in-string "/" "-" filename))
         (output-file (format "/tmp/fate-delta-%s-%s-%s" repo-hash hash encoded-path))
         (git-cmd (format "git -C %s show %s:%s > %s 2>/dev/null"
                          (shell-quote-argument magit-root)
                          (shell-quote-argument hash)
                          (shell-quote-argument filename)
                          (shell-quote-argument output-file))))
    (unless (file-exists-p output-file)
      (shell-command git-cmd))
    (let ((buf (find-file-noselect output-file)))
      (with-current-buffer buf
        (normal-mode)
        (setq buffer-read-only t)
        (fate-delta-review-mode 1))
      buf)))

(defun fate/delta--open-working-file (filename)
  "Open the working tree version of FILENAME."
  (let* ((magit-root (magit-toplevel))
         (file-path (expand-file-name filename magit-root)))
    (let ((buf (find-file-noselect file-path)))
      (with-current-buffer buf
        (fate-delta-review-mode 1))
      buf)))

(defun fate/delta-open-side-by-side ()
  "Open both base and working files side-by-side."
  (interactive)
  (let* ((filename (fate/delta--current-file))
         (lines (fate/delta--current-line-numbers))
         (revision fate-delta--target-revision)
         (crd-buf (current-buffer)))
    (unless revision
      (user-error "No target revision set in this buffer. Was it created using fate/delta-code-review-diff?"))
    (setq fate-delta--saved-window-config (current-window-configuration))
    (let ((base-buf (fate/delta--open-base-file filename revision))
          (work-buf (fate/delta--open-working-file filename)))
      (delete-other-windows)
      (switch-to-buffer base-buf)
      (when (car lines)
        (goto-char (point-min))
        (forward-line (1- (car lines)))
        (recenter))
      (split-window-right)
      (other-window 1)
      (switch-to-buffer work-buf)
      (when (cdr lines)
        (goto-char (point-min))
        (forward-line (1- (cdr lines)))
        (recenter))
      (with-current-buffer base-buf
        (setq-local fate-delta--crd-buffer crd-buf))
      (with-current-buffer work-buf
        (setq-local fate-delta--crd-buffer crd-buf)))))

(defun fate/delta-review-quit ()
  "Quit the side-by-side review and restore the layout."
  (interactive)
  (let ((crd-buf (or (and (boundp 'fate-delta--crd-buffer) fate-delta--crd-buffer)
                     (get-buffer "*delta-crd*"))))
    (when (bound-and-true-p fate-delta-review-mode)
      (fate-delta-review-mode -1))
    (when (and buffer-file-name (string-match-p "/tmp/fate-delta-" buffer-file-name))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer))))
    (if (buffer-live-p crd-buf)
        (with-current-buffer crd-buf
          (when fate-delta--saved-window-config
            (set-window-configuration fate-delta--saved-window-config))
          (pop-to-buffer crd-buf))
      (message "Original *delta-crd* buffer is dead."))))

(transient-define-prefix fate/delta-transient ()
  "Transient menu for fate-delta-mode."
  ["Actions"
   ("o" "Open Side-By-Side" fate/delta-open-side-by-side)
   ("b" "Open Base File" (lambda () (interactive)
                           (let* ((filename (fate/delta--current-file))
                                  (revision fate-delta--target-revision)
                                  (crd-buf (current-buffer))
                                  (buf (fate/delta--open-base-file filename revision)))
                             (let ((lines (fate/delta--current-line-numbers)))
                               (setq fate-delta--saved-window-config (current-window-configuration))
                               (with-current-buffer buf
                                 (setq-local fate-delta--crd-buffer crd-buf))
                               (pop-to-buffer buf)
                               (when (car lines)
                                 (goto-char (point-min))
                                 (forward-line (1- (car lines)))
                                 (recenter))))))
   ("w" "Open Working File" (lambda () (interactive)
                              (let* ((filename (fate/delta--current-file))
                                     (crd-buf (current-buffer))
                                     (buf (fate/delta--open-working-file filename)))
                                (let ((lines (fate/delta--current-line-numbers)))
                                  (setq fate-delta--saved-window-config (current-window-configuration))
                                  (with-current-buffer buf
                                    (setq-local fate-delta--crd-buffer crd-buf))
                                  (pop-to-buffer buf)
                                  (when (cdr lines)
                                    (goto-char (point-min))
                                    (forward-line (1- (cdr lines)))
                                    (recenter))))))
   ("RET" "Goto location at point" fate/delta-goto-file-at-point)])

(defun fate/delta--cleanup-crd ()
  "Kill all temporary base files spawned by this diff view and clear minor modes."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p fate-delta-review-mode)
        (fate-delta-review-mode -1)
        (when (and buffer-file-name (string-match-p "/tmp/fate-delta-" buffer-file-name))
          (when (file-exists-p buffer-file-name)
            (delete-file buffer-file-name))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(defun fate/delta-quit-crd ()
  "Quit the delta review buffer safely."
  (interactive)
  (fate/delta--cleanup-crd)
  (quit-window t))

(defvar fate-delta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'fate/delta-goto-file-at-point)
    (define-key map (kbd "o") 'fate/delta-transient)
    (define-key map (kbd "q") 'fate/delta-quit-crd)
    map))

(define-derived-mode fate-delta-mode special-mode "Delta"
  "Major mode for viewing git delta side-by-side diffs.
\\{fate-delta-mode-map}"
  (setq buffer-read-only t))

;;;###autoload
(defun fate/delta-code-review-diff (&optional target-revision)
  "Run the Code Review Diff (CRD) workflow in a Delta side-by-side view.
TARGET-REVISION defaults to origin/master if not provided."
  (interactive
   (list (read-string "Target revision (default origin/master): " nil nil "origin/master")))
  (let* ((buf (get-buffer-create "*delta-crd*"))
         (inhibit-read-only t)
         (width (window-width))
         (revision (if (or (null target-revision) (string= target-revision "")) "origin/master" target-revision))
         (filter-args '( ":(exclude)*.gql.ts"
                         ":(exclude)*.po"
                         ":(exclude,glob)**/mocks/*"
                         ":(exclude)*.spec.*"))
         (git-args (delq nil (flatten-tree (list "git" "diff" "--ignore-all-space" revision "--" "src" filter-args))))
         (delta-args (list "delta" "--side-by-side" (format "--width=%d" width) "--paging=never" "--file-style=plain"))
         (cmd (concat (mapconcat #'shell-quote-argument git-args " ") " 2>&1 | "
                      (mapconcat #'shell-quote-argument delta-args " ")))
         (default-dir default-directory))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook #'fate/delta--cleanup-crd nil t)
      (setq default-directory default-dir)
      (erase-buffer)
      (message "Running delta code review diff...")
      (let ((output (shell-command-to-string cmd)))
        (if (string-empty-p output)
            (message "No diff output generated (Working tree might be clean, or target branch is identical).")
          (insert output)
          (ansi-color-apply-on-region (point-min) (point-max))
          (fate-delta-mode)
          (setq-local fate-delta--target-revision revision)
          (goto-char (point-min))
          (message "Done")))
     (pop-to-buffer buf)
     (delete-other-windows))))

(provide 'fate-delta)
;;; fate-delta.el ends here
