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

;; Turn on line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight the current line
(global-hl-line-mode +1)

;; Font
(add-to-list 'default-frame-alist '(font . "Operator Mono-12:weight=light:style=book"))
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 160))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(defun set-font-size ()
    "Set font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

;; left ibuffer show full path and file names
;; (setq ibuffer-formats '((mark modified read-only " "
;;                          (name 40 40 :left :elide)
;;                          " "
;;                          (size 9 -1 :right)
;;                          " "
;;                          (mode 16 16 :left :elide)
;;                          " " filename-and-process)))

(provide 'fate-ui)
;;; fate-ui.el ends here
