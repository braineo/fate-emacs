;;; fate-ui.el --- Basic UI settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye <braineo@Binbins-iMac.local>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

(require 'fate-const)
(eval-when-compile
 (require 'core-load-paths))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(unless (>= emacs-major-version 27)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars . 0) default-frame-alist)
  (push '(horizontal-scroll-bars. 0) default-frame-alist))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; relegate tooltips to echo area only
(when (boundp 'tooltip-mode)
  (tooltip-mode -1))

;; Minimal UI
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

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
(global-hl-line-mode t)

(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Transparent frame
(set-frame-parameter (selected-frame) 'alpha '(95 95))

;; right border takes 1 pixel on right splited buffer, make fringe visible
(setq-default window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
(window-divider-mode)

;; When it is macOS and verison number is higher than 26.0
(when (and IS-MAC (>= emacs-major-version 26))
  (setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark))))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

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
  :defer t
  :init
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
  :hook
  (after-init-hook . which-key-mode)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

(provide 'fate-ui)
;;; fate-ui.el ends here
