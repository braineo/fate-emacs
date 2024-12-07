;;; fate-git.el --- git related packages             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:


;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'core-load-paths)
  (require 'fate-custom))

(use-package sqlite3)

(use-package magit
  :custom
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (magit-diff-refine-hunk t "Show fine differences for the current diff hunk only.")
  :bind
  (("C-x g" . magit-status)
   ("s-m" . magit-file-dispatch)))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :after magit
  :config
  (if 'fate/forge-alist
    (add-to-list 'forge-alist fate/forge-alist)))

(use-package git-modes
  :defer t)

(use-package diff-hl
  :diminish
  :hook
  ((after-init . global-diff-hl-mode)
   (after-init . diff-hl-flydiff-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode . diff-hl-dired-mode))
  :config
  (progn
    (setq vc-git-diff-switches '("--histogram"))
    (unless (display-graphic-p)
      ;; There's no fringe when Emacs is running in the console
      (diff-hl-margin-mode 1))))

(use-package blamer
  :bind (("C-c i" . blamer-show-posframe-commit-info))
  :defer t)

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :hook (ediff-quit . winner-undo))

(defun fate/ediff-any (&optional major-mode-name)
    "Open two temporary buffers, launch ediff to compare them, and clean up on exit."
    (interactive (list (read-minibuffer "Optional major mode (e.g., json): ")))
    ;; Save current window configuration
    (let ((buf1 (generate-new-buffer "*ediff-buffer-1*"))
          (buf2 (generate-new-buffer "*ediff-buffer-2*")))
      ;; Display the buffers side by side
      (delete-other-windows)
      (switch-to-buffer buf1)
      (message (concat "Buffer 1: Enter content and press "
                 (propertize "C-c C-c" 'face 'help-key-binding)
                 " to proceed to the next buffer."))
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c")
          (lambda  ()
            (interactive)
            (delete-other-windows)
            (switch-to-buffer buf2)
            (message (concat "Buffer 2: Enter content and press "
                       (propertize "C-c C-c" 'face 'help-key-binding)
                       "to start ediff."))
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (when major-mode
                    (with-current-buffer buf1
                      (funcall (intern (concat (symbol-name major-mode-name) "-mode"))))
                    (with-current-buffer buf2
                      (funcall (intern (concat (symbol-name major-mode-name) "-mode"))))
                    (when (string-prefix-p "json" (symbol-name major-mode-name))
                      (mapc (lambda (buf)
                              (with-current-buffer buf
                                (fate/json-pretty-print)))
                        '(buf1 buf2))))
                  (ediff-buffers buf1 buf2))
                (use-local-map map))))
          (use-local-map map)))))

(provide 'fate-git)
;;; fate-git.el ends here
