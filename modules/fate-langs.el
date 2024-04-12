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

(eval-when-compile
  (require 'core-packages))

(use-package reformatter
  :defer t)

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
  (gofmt-command (if (executable-find "goimports") "goimports" "gofmt")))

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

(use-package treesit-auto
  :commands (treesit-auto-add-to-auto-mode-alist global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(python rust yaml org json glsl dockerfile c cmake bash))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'fate-langs)
;;; fate-langs.el ends here
