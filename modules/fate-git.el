;;; fate-git.el --- git related packages             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:


;;; Commentary:

;;

;;; Code:

(use-package magit
  :defer t
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  :bind
  (("C-x g" . magit-status)))

(provide 'fate-git)
;;; fate-git.el ends here
