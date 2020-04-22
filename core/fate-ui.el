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

;; Minimal UI
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(unless (>= emacs-major-version 27)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  ;; relegate tooltips to echo area only
  (when (boundp 'tooltip-mode)
    (tooltip-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

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
  :config
  (doom-modeline-def-modeline 'doom-fate
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name grip gnus debug lsp minor-modes input-method indent-info buffer-encoding major-mode vcs checker))
  (defun fate-doom-modeline ()
    "Setup custom doom modeline."
    (doom-modeline-set-modeline 'doom-fate 'default))
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . fate-doom-modeline)))

(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :init
  (setq inhibit-startup-screen t)
  :custom
  (dashboard-banner-logo-title "Unlimited Blade Works")
  (dashboard-startup-banner (expand-file-name (concat fate-directory "asset/fate-banner.png")))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  :config
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))
        ;; Format: "(icon title help action face prefix suffix)"
        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
             "Homepage" "Browse homepage"
             (lambda (&rest _) (browse-url FATE-HOME)))
           (,(when (display-graphic-p)
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
             "Update" "Update Fate Emacs"
              (lambda (&rest _) (fate-update)))))))

(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (member "all-the-icons" (font-family-list))
          (all-the-icons-install-fonts t)))

;; Highlight nested parentheses
(use-package highlight-parentheses
  :custom
  (hl-paren-highlight-adjacent t "like show-paren-mode")
  (hl-paren-delay 0.2 "delay of parentheses highlight")
  (hl-paren-colors '("IndianRed3"
                     "goldenrod3"
                     "Springgreen3"
                     "DeepSkyBlue3"
                     "RoyalBlue3"
                     "DarkViolet") "colors from inside to outside")
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
  (after-init . which-key-mode)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

(provide 'fate-ui)
;;; fate-ui.el ends here
