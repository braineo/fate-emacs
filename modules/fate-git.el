;;; fate-git.el --- git related packages             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:


;;; Commentary:

;;

;;; Code:

(use-package magit
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  :bind
  (("C-x g" . magit-status)))

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(provide 'fate-git)
;;; fate-git.el ends here
