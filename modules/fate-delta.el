;;; fate-delta.el --- Delta side-by-side diff viewer -*- lexical-binding: t -*-

;;; Commentary:
;; Provides `fate-delta-mode` to view ANSI colorized `delta --side-by-side`
;; outputs, and jump to the corresponding line in the original file.

;;; Code:

(require 'magit)
(require 'transient)
(require 'subr-x)

(use-package xterm-color)

(defgroup fate-delta nil
  "Delta side-by-side viewer mode."
  :group 'tools)

(defcustom fate-delta-exclude-patterns
  '(":(exclude)*.gql.ts"
    ":(exclude)*.po"
    ":(exclude,glob)**/mocks/*"
    ":(exclude)*.spec.*")
  "List of pathspec patterns to exclude from the diff."
  :type '(repeat string)
  :group 'fate-delta)

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

(defvar-local fate-delta--crd-buffer nil
  "Reference to the *delta-crd* buffer that spawned this review buffer.")

(defvar-local fate-delta--review-buffers nil
  "List of buffers spawned by this delta-crd session.")

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
Stores the file in `temporary-file-directory' and applies the correct major mode."
  (let* ((magit-root (magit-toplevel))
         (hash (with-temp-buffer
                 (if (zerop (call-process "git" nil t nil "-C" magit-root "rev-parse" revision))
                     (string-trim (buffer-string))
                   (user-error "Invalid revision: %s" revision))))
         (repo-hash (md5 magit-root))
         (encoded-path (replace-regexp-in-string "/" "-" filename))
         (output-file (expand-file-name (format "fate-delta-%s-%s-%s" repo-hash hash encoded-path) temporary-file-directory))
         (obj (concat hash ":" filename)))
    (unless (file-exists-p output-file)
      (with-temp-file output-file
        (call-process "git" nil t nil "-C" magit-root "show" obj)))
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
      (pop-to-buffer base-buf '(display-buffer-use-some-window))
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
      (dolist (b (list base-buf work-buf))
        (with-current-buffer b
          (setq-local fate-delta--crd-buffer crd-buf))
        (with-current-buffer crd-buf
          (push b fate-delta--review-buffers))))))

(defun fate/delta-review-quit ()
  "Quit the side-by-side review and restore the layout."
  (interactive)
  (let ((crd-buf (or (and (boundp 'fate-delta--crd-buffer) fate-delta--crd-buffer)
                     (get-buffer "*delta-crd*"))))
    (when (bound-and-true-p fate-delta-review-mode)
      (fate-delta-review-mode -1))
    (when (and buffer-file-name (string-prefix-p temporary-file-directory buffer-file-name)
               (string-match-p "fate-delta-" buffer-file-name))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer))))
    (if (buffer-live-p crd-buf)
        (with-current-buffer crd-buf
          (when fate-delta--saved-window-config
            (set-window-configuration fate-delta--saved-window-config))
          (pop-to-buffer crd-buf))
      (message "Original *delta-crd* buffer is dead."))))

(defun fate/delta-open-base-file ()
  "Open the base version of the file at point."
  (interactive)
  (let* ((filename (fate/delta--current-file))
         (revision fate-delta--target-revision)
         (lines (fate/delta--current-line-numbers))
         (crd-buf (current-buffer))
         (buf (fate/delta--open-base-file filename revision)))
    (setq fate-delta--saved-window-config (current-window-configuration))
    (with-current-buffer buf
      (setq-local fate-delta--crd-buffer crd-buf))
    (with-current-buffer crd-buf
      (push buf fate-delta--review-buffers))
    (pop-to-buffer buf)
    (when (car lines)
      (goto-char (point-min))
      (forward-line (1- (car lines)))
      (recenter))))

(defun fate/delta-open-working-file ()
  "Open the working tree version of the file at point."
  (interactive)
  (let* ((filename (fate/delta--current-file))
         (lines (fate/delta--current-line-numbers))
         (crd-buf (current-buffer))
         (buf (fate/delta--open-working-file filename)))
    (setq fate-delta--saved-window-config (current-window-configuration))
    (with-current-buffer buf
      (setq-local fate-delta--crd-buffer crd-buf))
    (with-current-buffer crd-buf
      (push buf fate-delta--review-buffers))
    (pop-to-buffer buf)
    (when (cdr lines)
      (goto-char (point-min))
      (forward-line (1- (cdr lines)))
      (recenter))))

(transient-define-prefix fate/delta-transient ()
  "Transient menu for fate-delta-mode."
  ["Actions"
   ("o" "Open Side-By-Side" fate/delta-open-side-by-side)
   ("b" "Open Base File" fate/delta-open-base-file)
   ("w" "Open Working File" fate/delta-open-working-file)
   ("RET" "Goto location at point" fate/delta-goto-file-at-point)])

(defun fate/delta--cleanup-crd ()
  "Kill all temporary base files spawned by this diff view and clear minor modes."
  (dolist (buf fate-delta--review-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p fate-delta-review-mode)
          (fate-delta-review-mode -1))
        (when (and buffer-file-name
                   (string-prefix-p temporary-file-directory buffer-file-name))
          (when (file-exists-p buffer-file-name)
            (delete-file buffer-file-name))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf))))))
  (setq fate-delta--review-buffers nil))

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

(defun fate/delta--read-diff-paths (revision)
  "Prompt the user to select diff paths from changed files against REVISION."
  (let ((changed-dirs
         (delete-dups
          (mapcar (lambda (f) (car (split-string f "/")))
                  (magit-changed-files revision)))))
    (if changed-dirs
        (completing-read-multiple "Diff paths (comma separated): " changed-dirs nil nil
                                  (car changed-dirs))
      (list "."))))

;;;###autoload
(defun fate/delta-code-review-diff (&optional target-revision)
  "Run the Code Review Diff (CRD) workflow in a Delta side-by-side view.
TARGET-REVISION defaults to origin/master if not provided."
  (interactive
   (list (magit-read-other-branch-or-commit "Diff against")))
  (let* ((revision (or target-revision "origin/master"))
         (diff-paths (fate/delta--read-diff-paths revision))
         (buf (get-buffer-create "*delta-crd*"))
         (width (window-width))
         (git-args (delq nil (flatten-tree (list "git" "diff" "--ignore-all-space" revision "--" diff-paths fate-delta-exclude-patterns))))
         (delta-args (list "delta" "--side-by-side" (format "--width=%d" width) "--paging=never" "--file-style=plain"))
         (cmd (concat (mapconcat #'shell-quote-argument git-args " ") " 2>&1 | "
                      (mapconcat #'shell-quote-argument delta-args " ")))
         (default-dir default-directory))
    (with-current-buffer buf
      (fundamental-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (add-hook 'kill-buffer-hook #'fate/delta--cleanup-crd nil t)
      (setq default-directory default-dir)
      (message "Running delta code review diff..."))
    (pop-to-buffer buf '(display-buffer-full-frame))
    (let ((proc (start-process-shell-command "delta-crd" buf cmd)))
      (set-process-filter
       proc
       (lambda (proc output)
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (xterm-color-filter output)))))))
      (set-process-sentinel
       proc
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (zerop (buffer-size (process-buffer proc)))
               (message "No diff output generated (Working tree might be clean, or target branch is identical).")
             (with-current-buffer (process-buffer proc)
               (fate-delta-mode)
               (setq-local fate-delta--target-revision revision)
               (goto-char (point-min))
               (message "Done")))))))))

(provide 'fate-delta)
;;; fate-delta.el ends here
