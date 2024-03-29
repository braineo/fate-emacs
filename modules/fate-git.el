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

(provide 'fate-git)
;;; fate-git.el ends here
