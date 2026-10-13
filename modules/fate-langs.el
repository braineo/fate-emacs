;;; fate-langs.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; collection of simple language package settings

;;; Code:

(eval-when-compile
  (require 'fate-misc)
  (require 'treesit))

;; Tree sitter for better syntax highlight
(use-package treesit
  :if (treesit-available-p)
  :ensure nil
  :config
  ;; Built-in ts modes (c, cpp, cmake, css, bash, go, html, java, js, json,
  ;; lua, markdown, python, ruby, rust, toml, tsx, typescript, yaml, ...)
  ;; register their own pinned grammar recipes since Emacs 31.
  ;; Only list grammars for modes that are not built in.
  (dolist (source
           '((elisp      "https://github.com/Wilfred/tree-sitter-elisp")
             (graphql    "https://github.com/bkegley/tree-sitter-graphql")
             (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
             (kotlin     "https://github.com/fwcd/tree-sitter-kotlin")
             (make       "https://github.com/alemuller/tree-sitter-make")
             (mojo       "https://github.com/HerringtonDarkholme/tree-sitter-mojo")
             (org        "https://github.com/milisims/tree-sitter-org")
             (scala      "https://github.com/tree-sitter/tree-sitter-scala" "master" "src")
             (sql        "https://github.com/m-novikov/tree-sitter-sql")
             (typst      "https://github.com/uben0/tree-sitter-typst" "master" "src")
             (vue        "https://github.com/merico-dev/tree-sitter-vue")
             (zig        "https://github.com/GrayJack/tree-sitter-zig")))
    (add-to-list 'treesit-language-source-alist source))
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install-grammar 'always)
  ;; Replaces hand-written `major-mode-remap-alist'.  Not t: go-mode,
  ;; rust-mode, lua-mode etc. are intentionally kept.
  (treesit-enabled-modes '(c-ts-mode c++-ts-mode c-or-c++-ts-mode cmake-ts-mode
                           toml-ts-mode css-ts-mode js-ts-mode json-ts-mode
                           python-ts-mode bash-ts-mode yaml-ts-mode)))

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package gdb-mi
  :ensure nil
  :defer t
  :custom
  ;; use gdb-many-windows by default when `M-x gdb'
  (gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (gdb-show-main t))

(use-package reformatter
  :defer t)

;; Lisp

(defun fate-init-parinfer-mode ()
  "Disable some minor modes having troubles with parinfer-rus-mode."
  (electric-pair-local-mode -1)
  (parinfer-rust-mode))

(use-package elisp-mode
  :ensure nil
  :custom
  (elisp-fontify-semantically t))

(use-package parinfer-rust-mode
  :if (string-match-p "MODULES" system-configuration-features)
  :hook
  ((emacs-lisp-mode scheme-mode) . fate-init-parinfer-mode)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-library-directory (concat fate-cache-directory "parinfer-rust/")))

(defun fate/treesit-install-language-grammars (&optional force)
  "Install treesitter so for configured major modes.
When FORCE is set, clone and reinstall grammar."
  (interactive "P")
  (dolist (lang (mapcar 'car treesit-language-source-alist))
    (when (or force
            (not (treesit-language-available-p lang)))
      (message (format "installing %s grammar" lang))
      (treesit-install-language-grammar lang))))

(reformatter-define taplo
  :program "taplo"
  :args '("format" "-")
  :lighter "TAPLO")

(use-package toml-ts-mode
  :ensure nil
  :bind (:map toml-ts-mode-map
          ("C-c C-l" . taplo-buffer)))

(use-package swift-mode
  :mode (("\\.swift\\'" . swift-mode)))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :config
  (defconst fate/go-tools
    '("golang.org/x/tools/gopls"
      "golang.org/x/tools/cmd/goimports"
      "github.com/cweill/gotests/gotests"
      "github.com/fatih/gomodifytags"
      "github.com/josharian/impl"
      "github.com/haya14busa/goplay/cmd/goplay"
      "github.com/go-delve/delve/cmd/dlv"
      "honnef.co/go/tools/cmd/staticcheck")
    "go cli tools")
  (with-eval-after-load 'core-packages
    (fate/create-install-tools!
      "go" ("go" "install" "-v" "-x") fate/go-tools (lambda (tool) (concat tool "@latest"))))
  :custom
  (gofmt-command (if (executable-find "goimports") "goimports" "gofmt"))
  :hook
  (before-save . gofmt-before-save)
  (go-mode . (lambda () (treesit-parser-create 'go))))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (defconst fate/rust-tools
    '("ripgrep"
      "fd-find"
      "typos-cli"
      "difftastic"
      "tealdeer"
      "git-delta"
      "ast-grep"
      "stylua"
      "taplo-cli")
    "rust cli tools")
  (with-eval-after-load 'core-packages
    (fate/create-install-tools!
      "rust" ("cargo" "install") fate/rust-tools))
  :mode (("\\.rs\\'" . rust-mode))
  :custom (rust-format-on-save t))

(use-package cargo-mode
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;; QML

(reformatter-define qmlformat
  :program "qmlformat"
  :stdin nil
  :args (list input-file "--no-sort")
  :lighter "QF")

(use-package qml-mode
  :mode (("\\.qml\\'" . qml-mode))
  :bind (:map qml-mode-map
         ("C-c C-l" . qmlformat-buffer)))

;; i18n
(use-package po-mode
  :mode (("\\.po\\'" . po-mode)))

(use-package conf-mode
  :ensure nil
  :mode (("/\\.env" . conf-mode)))

;; shaders
(use-package glsl-mode
  :mode (("/\\.glsl" . glsl-mode)))

;; lua

(reformatter-define stylua
  :program "stylua"
  :args '("-" "--indent-type=Spaces" "--indent-width=2")
  :lighter "QF")

(use-package lua-mode
  :mode (("/\\.lua" . lua-mode))
  :bind (:map lua-mode-map
         ("C-c C-l" . stylua-buffer)))

(reformatter-define shfmt
  :program "shfmt"
  :args '("--indent=4" "--case-indent" "--simplify")
  :lighter "SHF")

(use-package sh-script
  :ensure nil
  :bind (:map sh-mode-map
          ("C-c C-l" . shfmt-buffer)
          :map bash-ts-mode-map
          ("C-c C-l" . shfmt-buffer)))

(provide 'fate-langs)
;;; fate-langs.el ends here
