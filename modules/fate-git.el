;;; fate-git.el --- git related packages             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:


;;; Commentary:

;;

;;; Code:

(use-package magit
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  :bind
  (("C-x g" . magit-status)
   ("s-m b" . magit-blame)))

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
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (dired-mode-hook . diff-hl-dired-mode)))

(provide 'fate-git)
;;; fate-git.el ends here
