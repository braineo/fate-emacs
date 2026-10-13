;;; fate-ui.el --- Basic UI settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye <braineo@Binbins-iMac.local>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

(require 'fate-const)
(require 'fate-hints)
(require 'fate-custom)
(eval-when-compile
 (require 'core-load-paths))

;; Minimal UI: menu/tool/scroll bars are disabled in early-init.el

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

;; Turn on line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; disable the annoying bell ring
;; disable startup screen
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(setopt ring-bell-function 'ignore
        inhibit-startup-screen t
        use-short-answers t)

;; highlight the current line
(global-hl-line-mode t)

;; right border takes 1 pixel on right split buffer, make fringe visible
(setopt window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        window-combination-resize t)
(window-divider-mode)

(when IS-MAC
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(add-to-list 'default-frame-alist '(alpha-background . 97))
(set-frame-parameter nil 'alpha-background 97)

;; Theme
(use-package doom-themes
  :if fate/theme
  :config
  (load-theme fate/theme t)
  (custom-set-faces
   `(color-rg-font-lock-header-line-text ((t (:foreground ,(doom-color 'base7)))))
   `(color-rg-font-lock-header-line-keyword ((t (:foreground ,(doom-color 'red)))))
   `(color-rg-font-lock-header-line-directory ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-header-line-edit-mode ((t (:foreground ,(doom-color 'magenta)))))
   `(color-rg-font-lock-command ((t (:background ,(doom-color 'modeline-bg) :foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-file ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-line-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-column-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-match ((t (:foreground ,(doom-color 'red))))))
  ;; new font-lock since 29.1 for tree sitter
  (custom-theme-set-faces
    fate/theme
    ;; '(font-lock-bracket-face) ;; punctuation
    ;; '(font-lock-delimiter-face) ;; punctuation
    '(font-lock-escape-face ((t (:inherit font-lock-keyword-face))))
    ;; '(font-lock-function-call-face) ;; function-name-face
    '(font-lock-misc-punctuation-face ((t (:inherit font-lock-keyword-face))))
    '(font-lock-number-face ((t (:inherit font-lock-constant-face))))
    '(font-lock-operator-face ((t (:inherit font-lock-keyword-face))))
    ;; '(font-lock-property-name-face) ;; defined
    '(font-lock-property-use-face ((t (:inherit font-lock-property-name-face :slant italic)))))
    ;; '(font-lock-punctuation-face) ;; dimmed
    ;; '(font-lock-regexp-face) ;; defined
    ;; '(font-lock-variable-use-face)) ;; variable-name

  (enable-theme fate/theme))

(use-package modus-themes
  :if (not fate/theme)
  :config
  ;; Your customizations here:
  (setopt modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold))

  (setopt modus-themes-common-palette-overrides
    '((string green-faint)
      (type yellow-cooler)
      (border-mode-line-active unspecified)
      (border-mode-line-inactive unspecified)
      (modeline-err err)
      (modeline-warning warning)
      (modeline-info info)
      (underline-warning warning)
      (fg-heading-1 blue-cooler)
      (fg-heading-2 cyan-intense)
      (fg-heading-3 yellow-warmer)))

  (defun fate/modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
        ;; font-lock
        `(font-lock-escape-face ((,c :foreground ,green)))
        `(font-lock-regexp-face ((,c :foreground ,blue-faint)))
        `(font-lock-property-use-face ((,c :foreground ,slate)))
        ;; forge
        `(forge-pullreq-merged ((,c :inherit default)))
        `(forge-pullreq-open ((,c :inherit bold)))
        `(forge-pullreq-rejected ((,c :inherit default)))
        ;; multiple-cursors.el
        `(mc/cursor-bar-face ((,c :background ,cursor)))
        `(mc/cursor-face ((,c :background ,cursor :foreground ,bg-main)))
        ;; color-rg
        `(color-rg-font-lock-header-line-text ((,c :foreground ,fg-main)))
        `(color-rg-font-lock-header-line-keyword ((,c :foreground ,red)))
        `(color-rg-font-lock-header-line-directory ((,c :foreground ,blue)))
        `(color-rg-font-lock-header-line-edit-mode ((,c :foreground ,magenta)))
        `(color-rg-font-lock-command ((,c :background ,bg-dim :foreground ,comment)))
        `(color-rg-font-lock-file ((,c :foreground ,blue)))
        `(color-rg-font-lock-line-number ((,c :foreground ,comment)))
        `(color-rg-font-lock-column-number ((,c :foreground ,comment)))
        `(color-rg-font-lock-match ((,c :foreground ,red))))))

  (add-hook 'modus-themes-after-load-theme-hook #'fate/modus-themes-custom-faces)
  (modus-themes-load-theme 'modus-vivendi))

(use-package solaire-mode
  :if fate/theme
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Unlimited Blade Works")
  (dashboard-startup-banner (expand-file-name (concat fate-directory "asset/fate-banner.png")))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-projects-switch-function 'find-file)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-projects-backend 'projectile)
  (dashboard-footer-messages fate/emacs-hints)
  (dashboard-items '((recents  . 10)
                     (projects . 10)))
  :config
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)

        ;; Format: "(icon title help action face prefix suffix)"
        dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (nerd-icons-codicon "nf-cod-github"))
             "Homepage" "Browse homepage"
             (lambda (&rest _) (browse-url FATE-HOME)))
           (,(when (display-graphic-p)
               (nerd-icons-mdicon "nf-md-update"))
             "Update" "Update Fate Emacs"
             (lambda (&rest _) (fate/update-configuration))))))
  (dashboard-setup-startup-hook))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Highlight nested parentheses
(use-package highlight-parentheses
  :custom
  (highlight-parentheses-highlight-adjacent t "like show-paren-mode")
  (highlight-parentheses-delay 0.2 "delay of parentheses highlight")
  (highlight-parentheses-colors '("IndianRed3"
                                  "goldenrod3"
                                  "Springgreen3"
                                  "DeepSkyBlue3"
                                  "RoyalBlue3"
                                  "DarkViolet") "colors from inside to outside")
  :custom-face
  (highlight-parentheses-highlight ((nil (:weight ultra-bold))))
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

;; Highlight TODO keywords FIXME HACK DONE FAIL OKAY CR)
(use-package hl-todo
  :config
  (add-to-list 'hl-todo-keyword-faces '("CR"  . "#e57b8b"))
  (global-hl-todo-mode))

;; Highlight operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package symbol-overlay
  :diminish
  :defer t
  :init
  (with-eval-after-load 'transient
    (transient-define-prefix symbol-overlay-transient ()
      "Symbol overlay"
      :transient-suffix 'transient--do-stay
      [["Highlight"
        ("h" "Highlight symbol" symbol-overlay-put)
        ("t" "Toggle scope" symbol-overlay-toggle-in-scope)
        ("c" "Clear" symbol-overlay-remove-all)]

       ["Navigation"
        ("n" "Next" symbol-overlay-jump-next)
        ("p" "Previous" symbol-overlay-jump-prev)]

       ["Manipulate"
        ("s" "Search" symbol-overlay-isearch-literally :transient nil)
        ("r" "Replace" symbol-overlay-query-replace :transient nil)
        ("R" "Rename" symbol-overlay-rename :transient nil)]])))

;; Show color of color text #FFE4C4
(use-package colorful-mode
  :hook
  (prog-mode text-mode))

;; Show the line of an offscreen opening paren
(setopt show-paren-context-when-offscreen 'overlay)

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (setq-local show-trailing-whitespace t))))

(defun set-font-size ()
    "Set font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

;; Which Key
(use-package which-key
  :ensure nil
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

(provide 'fate-ui)
;;; fate-ui.el ends here
