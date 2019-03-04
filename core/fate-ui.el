;;; fate-ui.el --- Basic UI settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye <braineo@Binbins-iMac.local>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

(require 'fate-const)
(require 'core-load-paths)

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
(add-to-list 'default-frame-alist '(font . "Operator Mono:weight=light:style=book:pixelsize=12"))
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(alpha 95 95))

(set-frame-parameter (selected-frame) 'alpha '(95 95))

;; When it is macOS and verison number is higher than 26.0
(when (and IS-MAC (>= emacs-major-version 26))
  (setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark))))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t))

(use-package doom-snazzy-theme
  :disabled
  :after doom-themes
  :load-path "theme/")

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :init
  (setq inhibit-startup-screen t)
  :config
  (setq dashboard-banner-logo-title "Unlimited Blade Works")
  (setq dashboard-startup-banner (expand-file-name (concat fate-directory "asset/fate-banner.png")))
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))))

;; Highlight nested parentheses
(use-package highlight-parentheses
  :custom
  (hl-paren-delay 0.2 "delay of parentheses highlight")
  (hl-paren-colors '("Springgreen3"
                      "IndianRed1"
                      "IndianRed3"
                      "IndianRed4") "colors from inside to outside")
  :custom-face
  (hl-paren-face ((nil (:weight ultra-bold))))
  :hook
  (prog-mode . highlight-parentheses-mode))

;; Color delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Highlight number variables
(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode)
  (asm-mode . highlight-numbers--turn-off))

;; Highlight TODO keywords FIXME HACK DONE FAIL OKAY)
(use-package hl-todo
  :hook
  ((prog-mode text-mode) . hl-todo-mode))

;; Highlight operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package symbol-overlay
  :diminish
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-symbol-overlay (:hint nil)
      "
^Highlight^            ^Navigation^           ^Manipulate^
^^^^^^^^-------------------------------------------------
[_h_]ighlight symbol    [_n_]ext              [_s_]earch
[_t_]oogle scope        [_p_]revious          [_r_]eplace
[_c_]lean               ^ ^                   [_R_]ename
"
      ("h" symbol-overlay-put)
      ("n" symbol-overlay-jump-next)
      ("p" symbol-overlay-jump-prev)
      ("t" symbol-overlay-toggle-in-scope)
      ("s" symbol-overlay-isearch-literally)
      ("r" symbol-overlay-query-replace)
      ("R" symbol-overlay-rename)
      ("c" symbol-overlay-remove-all)
      ("q" nil :color blue))))

;; Show color of color text #FFE4C4
(use-package rainbow-mode
  :hook
  (prog-mode text-mode))

;; Show whitespaces
(use-package whitespace
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :init
  (progn
    (setq whitespace-style '(face tabs empty trailing))))

(defun set-font-size ()
    "Set font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode t))

(provide 'fate-ui)
;;; fate-ui.el ends here
