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
(set-frame-parameter (window-frame) 'cursor-type 'bar)

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

;; Initial size to be full screen height, half screen width and in the middle
(when (display-graphic-p)
  (let ((frame (selected-frame)))
    (set-frame-width frame (/ (display-pixel-width) 2) nil 'pixelwise)
    (set-frame-height frame (display-pixel-height) nil 'pixelwise)
    (set-frame-position frame (/ (display-pixel-width) 4) 0)))


;; right border takes 1 pixel on right splited buffer, make fringe visible
(setq-default window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
(window-divider-mode)

;; When it is macOS and verison number is higher than 26.0
(when (and IS-MAC (>= emacs-major-version 26))
  (setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark))))

;; Tree sitter for better syntax highlight
(use-package treesit
  :if (treesit-available-p)
  :ensure nil
  :init
  (setq treesit-language-source-alist
    '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
      (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
      (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
      (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
      (c-sharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp" nil "src"))
      (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
      (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
      (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil))
      (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
      (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
      (graphql    . ("https://github.com/bkegley/tree-sitter-graphql"))
      (haskell    . ("https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil))
      (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
      (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
      (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
      (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
      (make       . ("https://github.com/alemuller/tree-sitter-make"))
      (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
      (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
      (org        . ("https://github.com/milisims/tree-sitter-org"))
      (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
      (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
      (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
      (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
      (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
      (scala      . ("https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil))
      (toml       . ("https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil))
      (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
      (kotlin     . ("https://github.com/fwcd/tree-sitter-kotlin"))
      (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
      (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))
      (mojo       . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo"))))

  (setq major-mode-remap-alist
    '((c-mode         . c-ts-mode)
      (c++-mode       . c++-ts-mode)
      (cmake-mode     . cmake-ts-mode)
      (conf-toml-mode . toml-ts-mode)
      (css-mode       . css-ts-mode)
      (graphql-mode   . graphql-ts-mode)
      (js-mode        . js-ts-mode)
      (js-json-mode   . json-ts-mode)
      (python-mode    . python-ts-mode)
      (sh-mode        . bash-ts-mode)
      (yaml-mode      . yaml-ts-mode)))

  :custom
  (treesit-font-lock-level 4))

(add-to-list 'default-frame-alist '(alpha-background . 97))
(set-frame-parameter nil 'alpha-background 97)

;; Theme
(use-package doom-themes
  :config
  (load-theme fate/theme t)
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

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :config
  (doom-modeline-def-modeline 'doom-fate
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name grip gnus debug lsp minor-modes input-method indent-info buffer-encoding major-mode vcs check))
  (defun fate-doom-modeline ()
    "Setup custom doom modeline."
    (doom-modeline-set-modeline 'doom-fate 'default))
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . fate-doom-modeline))
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
  :config
  (progn
    (setq dashboard-items '((recents  . 10)
                            (projects . 10))

          dashboard-startupify-list '(dashboard-insert-banner
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
    (dashboard-setup-startup-hook)))

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
  :ensure nil
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

(use-package gdb-mi
  :ensure nil
  :defer t
  :init
  (setq
    ;; use gdb-many-windows by default when `M-x gdb'
    gdb-many-windows t
    ;; Non-nil means display source file containing the main routine at startup
    gdb-show-main t))

(provide 'fate-ui)
;;; fate-ui.el ends here
