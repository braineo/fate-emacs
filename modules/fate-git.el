;;; fate-git.el --- git related packages             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:


;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'core-load-paths))

(use-package magit
  :custom
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (magit-diff-refine-hunk t "Show fine differences for the current diff hunk only.")
  :bind
  (("C-x g" . magit-status)
   ("s-m" . magit-file-dispatch)))

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package diff-hl
  :diminish
  :hook
  ((after-init . global-diff-hl-mode)
   (after-init . diff-hl-flydiff-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode . diff-hl-dired-mode))
  :config
  (unless (display-graphic-p)
    ;; There's no fringe when Emacs is running in the console
    (diff-hl-margin-mode 1)))

;; Has some serious performance issue for diff > 2000 line changes, disable for now.
(use-package magit-delta
  :disabled
  :if (executable-find "delta")
  :hook
  ((magit-mode . (lambda () (magit-delta-mode +1))))
  :config
  ;; Cannot use line number feature of delta in magit. refer to https://github.com/dandavison/magit-delta/issues/13
  (setq magit-delta-delta-args (append magit-delta-delta-args '("--features" "magit-delta"))))

(use-package transient
  :after magit
  :custom
  (transient-history-file (concat fate-cache-directory "transient/history.el"))
  (transient-values-file (concat fate-cache-directory "transient/values.el"))
  (transient-levels-file (concat fate-cache-directory "transient/levels.el"))
  (transient-display-buffer-action '(display-buffer-below-selected)))
(provide 'fate-git)
;;; fate-git.el ends here
