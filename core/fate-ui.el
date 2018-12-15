;;; fate-ui.el --- Basic UI settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye <braineo@Binbins-iMac.local>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; relegate tooltips to echo area only
(when (boundp 'tooltip-mode)
  (tooltip-mode -1))
  
;; Minimal UI
(scroll-bar-mode -1)
(menu-bar-mode   -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight the current line
(global-hl-line-mode +1)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(provide 'fate-ui)
;;; fate-ui.el ends here
