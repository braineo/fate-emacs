;;; fate-delta.el --- Delta side-by-side diff viewer -*- lexical-binding: t -*-

;;; Commentary:
;; Provides `fate-delta-mode` to view ANSI colorized `delta --side-by-side`
;; outputs, and jump to the corresponding line in the original file.
;;
;; Provides integrations with Magit as well.

;;; Code:

(require 'ansi-color)
(require 'magit)

(defgroup fate-delta nil
  "Delta side-by-side viewer mode."
  :group 'tools)

(defun fate/delta-goto-file-at-point ()
  "Jump to the file and line number corresponding to the point in delta output."
  (interactive)
  (let ((col (current-column))
        (half-width (/ (window-width) 2))
        left-line right-line line-str
        target-line target-file)
    (setq line-str (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    ;; Parse side-by-side line format using the unicode vertical pipe │
    ;; Example: "│  1  │a       │  1  │a"  or  "│     │        │  2  │modified"
    (when (string-match "^.*?│[ \t]*\\([0-9]+\\)?[ \t]*│.*?│[ \t]*\\([0-9]+\\)?[ \t]*│" line-str)
      (setq left-line (match-string 1 line-str))
      (setq right-line (match-string 2 line-str)))

    ;; Determine which side the cursor is on
    (if (< col half-width)
        (setq target-line left-line)
      (setq target-line right-line))

    ;; If the targeted side is empty (e.g., clicking on a newly added line on the left side),
    ;; default to the other available line.
    (unless target-line
      (setq target-line (or left-line right-line)))

    (unless target-line
      (user-error "No valid line number found at point"))

    ;; Search backward for the file name header
    ;; E.g., "Δ filename.txt", "added: filename.txt", "deleted: filename.txt"
    (save-excursion
      (if (re-search-backward "^\\(?:Δ \\|added: \\|deleted: \\|renamed: .* ⟶ \\)\\(.+\\)$" nil t)
          (setq target-file (match-string 1))
        (user-error "Could not find file header. Did you run delta with --file-style=plain?")))

    (when target-file
      (let ((file-path (expand-file-name (string-trim target-file) (magit-toplevel))))
        (if (file-exists-p file-path)
            (progn
              (find-file-other-window file-path)
              (goto-char (point-min))
              (forward-line (1- (string-to-number target-line)))
              (recenter))
          (user-error "File %s does not exist" file-path))))))

(defvar fate-delta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'fate/delta-goto-file-at-point)
    map))

(define-derived-mode fate-delta-mode special-mode "Delta"
  "Major mode for viewing git delta side-by-side diffs.
\\{fate-delta-mode-map}"
  (setq buffer-read-only t))

;;;###autoload
(defun fate/delta-code-review-diff (&optional target-branch)
  "Run the Code Review Diff (CRD) workflow in a Delta side-by-side view.
TARGET-BRANCH defaults to origin/master if not provided."
  (interactive
   (list (read-string "Target branch (default origin/master): " nil nil "origin/master")))
  (let* ((buf (get-buffer-create "*delta-crd*"))
         (inhibit-read-only t)
         (width (window-width))
         (branch (if (or (null target-branch) (string= target-branch "")) "origin/master" target-branch))
         (filter-args '( ":(exclude)*.gql.ts"
                         ":(exclude)*.po"
                         ":(exclude,glob)**/mocks/*"
                         ":(exclude)*.spec.*"))
         (git-args (delq nil (flatten-tree (list "git" "diff" "--ignore-all-space" branch "--" "src" filter-args))))
         (delta-args (list "delta" "--side-by-side" (format "--width=%d" width) "--paging=never" "--file-style=plain"))
         (cmd (concat (mapconcat #'shell-quote-argument git-args " ") " 2>&1 | "
                      (mapconcat #'shell-quote-argument delta-args " ")))
         (default-dir default-directory))
    (with-current-buffer buf
      (setq default-directory default-dir)
      (erase-buffer)
      (message "Running delta code review diff...")
      (let ((output (shell-command-to-string cmd)))
        (if (string-empty-p output)
            (message "No diff output generated (Working tree might be clean, or target branch is identical).")
          (insert output)
          (ansi-color-apply-on-region (point-min) (point-max))
          (fate-delta-mode)
          (goto-char (point-min))
          (message "Done")))
     (display-buffer buf))))

(provide 'fate-delta)
;;; fate-delta.el ends here
