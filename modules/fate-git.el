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
    (setq forge-alist (append forge-alist fate/forge-alist)))

  (defun fate/forge-topic-user-completion-at-point()
    (let ((bol (line-beginning-position))
          repo)
      (and
        (looking-back "@\\(?2:[[:alnum:]._-]*\\)" bol)
        (setq repo (forge-get-repository :tracked))
        (list
          (match-beginning 2)
          (match-end 0)
          (mapcar
            (lambda (row)
              ;; user id and user name
              (propertize (cadr row)
                :title (format " %s" (or (caddr row) ""))))
            (oref repo assignees))
          :annotation-function (##get-text-property 0 :title %)))))

  (defun fate/forge-user-reference-setup()
    (when-let ((repo (forge-get-repository :tracked?)))
      (add-hook 'completion-at-point-functions
        #'fate/forge-topic-user-completion-at-point nil t)))
  ;; Add user name highlight
  (font-lock-add-keywords 'forge-post-mode
   '(("\\(?:^\\|[^[:alnum:]]\\)\\(@[[:alnum:]._-]+\\)" 1 'markdown-link-face)))
  :custom
  (forge-post-mode-hook '(visual-line-mode
                           fate/forge-user-reference-setup) "exclude flyspell since jinx is running."))

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
  :hook (ediff-quit . winner-undo)
  :custom-face
  (ediff-current-diff-A   ((t (:inherit 'diff-removed))))
  (ediff-fine-diff-A   ((t (:inherit 'diff-refine-removed))))
  (ediff-current-diff-B   ((t (:inherit 'diff-added))))
  (ediff-fine-diff-B   ((t (:inherit 'diff-refine-added))))
  (ediff-current-diff-C   ((t (:inherit 'diff-changed))))
  (ediff-fine-diff-C   ((t (:inherit 'diff-refine-changed)))))

(defun fate/diff-temp-buffers (&optional major-mode-name)
    "Open two temporary buffers, launch ediff to compare them, and clean up on exit."
  (interactive
    (list (let ((input (read-from-minibuffer "highlight major mode (press ENTER to skip): ")))
            (unless (string-empty-p input) input))))

  ;; Validate major mode name if provided
  (when (and major-mode-name
          (not (fboundp (intern (concat major-mode-name "-mode")))))
    (user-error "Invalid major mode: %s" major-mode-name))
  ;; Save current window configuration
  (let* ((buf1 (generate-new-buffer "*ediff-buffer-1*"))
         (buf2 (generate-new-buffer "*ediff-buffer-2*"))
         (mode-sym (and major-mode-name
                     (intern (concat major-mode-name "-mode"))))
         (original-window-config (current-window-configuration))
         (restore-fn (lambda ()
                        (set-window-configuration original-window-config)
                        (dolist (buf (list buf1 buf2))
                          (when (buffer-live-p buf) (kill-buffer buf))))))
    (unwind-protect
      (progn
        ;; Display the buffers side by side
        (delete-other-windows)
        (switch-to-buffer buf1)
        (message (concat "Buffer 1: Enter content and press "
                   (propertize "C-c C-c" 'face 'help-key-binding)
                   " to proceed to the next buffer."))
        (let ((buf1-map (make-sparse-keymap)))
          (define-key buf1-map (kbd "C-c C-c")
            (lambda  ()
              (interactive)
              (delete-other-windows)
              (switch-to-buffer buf2)
              (message (concat "Buffer 2: Enter content and press "
                         (propertize "C-c C-c" 'face 'help-key-binding)
                         " to start ediff."))
              (let ((buf2-map (make-sparse-keymap)))
                (define-key buf2-map (kbd "C-c C-c")
                  (lambda ()
                    (interactive)
                    (when mode-sym
                      (with-current-buffer buf1 (funcall mode-sym))
                      (with-current-buffer buf2 (funcall mode-sym))
                      (when (string-prefix-p "json" major-mode-name)
                        (dolist (buf (list buf1 buf2))
                          (with-current-buffer buf
                            (fate/json-pretty-print)))))
                    (ediff-buffers buf1 buf2)
                    (add-hook 'ediff-quit-hook restore-fn 'local))
                  (use-local-map buf2-map))))
            (use-local-map buf1-map))))
      ;; Cleanup form: ensure buffers are deleted if something goes wrong
      (unless (and (buffer-live-p buf1) (buffer-live-p buf2))
        (funcall restore-fn)))))

(provide 'fate-git)
;;; fate-git.el ends here
