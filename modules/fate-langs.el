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


;; Lisp

(defun fate-init-parinfer-mode ()
  "Disable some minor modes having troubles with parinfer-rus-mode."
  (progn
    (electric-pair-local-mode -1)
    (parinfer-rust-mode)))

(use-package parinfer-rust-mode
  :if (string-match-p "MODULES" system-configuration-features)
  :hook
  ((emacs-lisp-mode scheme-mode) . fate-init-parinfer-mode)
  :init
  (setq parinfer-rust-auto-download t)
  :custom
  (parinfer-rust-library-directory (concat fate-cache-directory "parinfer-rust/")))


(use-package treesit
  :ensure nil
  :defer t
  :custom
  (treesit-font-lock-level 4))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . (lambda()
                           (setq-local prettier-js-args
                            '("--parser" "markdown")))))

(use-package markdown-toc
 :defer)

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package jinx
  :hook ((markdown-mode text-mode) . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package swift-mode
  :mode (("\\.swift\\'" . swift-mode)))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :config
  (defconst fate/go-tools
    '("golang.org/x/tools/gopls"
      "github.com/cweill/gotests/gotests"
      "github.com/fatih/gomodifytags"
      "github.com/josharian/impl"
      "github.com/haya14busa/goplay/cmd/goplay"
      "github.com/go-delve/delve/cmd/dlv"
       "honnef.co/go/tools/cmd/staticcheck")
    "go cli tools")

  (defun fate/go-install-tools ()
    "Install necessary tools for go development."
    (interactive)
    (unless (executable-find "go")
      (message "cannot find `go' executable"))

    (message "installing %d tools" (length fate/go-tools))
    (dolist (tool fate/go-tools)
      (set-process-sentinel
        (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat tool "@latest"))
        (lambda (proc _)
          (let ((status (process-exit-status proc)))
            (if (= 0 status)
              (message "Installed %s" tool)
             (message "Failed to install %s" tool))))))))

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :custom (rust-format-on-save t))

(use-package cargo-mode
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; QML

(defgroup qmlformat nil
  "Reformat QML using qmlformat."
  :group 'languages)

(defcustom qmlformat-command "qmlformat"
  "Command used for reformatting."
  :group 'qmlformat
  :type 'string)

(reformatter-define qmlformat
  :program qmlformat-command
  :stdin nil
  :args (list input-file "--no-sort")
  :lighter "QF"
  :group 'qmlformat)

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

(defgroup stylua nil
  "Reformat lua using StyLua."
  :group 'languages)

(defcustom stylua-command "stylua"
  "Command used for reformatting."
  :group 'stylua
  :type 'string)

(reformatter-define stylua
  :program stylua-command
  :args '("-" "--indent-type=Spaces" "--indent-width=2")
  :lighter "QF"
  :group 'stylua)

(use-package lua-mode
  :mode (("/\\.lua" . lua-mode))
  :bind (:map lua-mode-map
         ("C-c C-l" . stylua-buffer)))

(defgroup shfmt nil
  "Reformat JSON using shfmt."
  :group 'languages)

(defcustom shfmt-command "shfmt"
  "Command used for reformatting."
  :group 'shfmt
  :type 'string)

(reformatter-define shfmt
  :program shfmt-command
  :args '("--indent=4" "--case-indent" "--simplify")
  :lighter "SHF"
  :group 'shfmt)


(use-package sh-mode
  :ensure nil
  :bind (:map sh-mode-map
          ("C-c C-l" . shfmt-buffer)))

(use-package taskjuggler-mode
  :straight (:host gitlab
              :repo "bricka/emacs-taskjuggler-mode")
    :config (add-to-list 'tree-sitter-load-path (straight--repos-dir "tree-sitter-taskjuggler")))

(use-package tree-sitter-taskjuggler
  :defer t
  :straight (tree-sitter-taskjuggler
              :host gitlab
              :repo "braineo/tree-sitter-taskjuggler"
              :branch "block-comment"
              :pre-build (("npm" "ci") ("make"))
              :build (:not compile)
              :files ("taskjuggler.so")))


(provide 'fate-langs)
;;; fate-langs.el ends here
