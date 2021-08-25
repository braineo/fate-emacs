;;; fate-lsp.el ---                                  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(eval-when-compile
  (require 'core-load-paths)
  (require 'flycheck)
  (require 'fate-lsp-gql))

(defun fate-lsp-setup-python ()
  "Microsoft Python Language Server does not have a syntax checker, setup one for it."
  (progn
    (require 'lsp-python-ms)
    (lsp)
    ;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-626210720
    ;; Do not let lsp hijack flycheck
    (setq-local lsp-diagnostics-provider :none)
    (setq-local flycheck-checker 'python-flake8)))

(defun fate-lsp-setup-js ()
  "Do not start lsp when major mode is qml which derives from `js-mode'."
  (unless (member major-mode '(qml-mode))
    (lsp-deferred)))

(use-package lsp-python-ms
  :defer t
  :custom
  (lsp-python-ms-cache-dir (concat fate-cache-directory ".lsp-python")))

(use-package lsp-mode
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((python-mode . fate-lsp-setup-python)
   (js-mode . fate-lsp-setup-js)
   ((sh-mode c-mode c++-mode
      html-mode web-mode json-mode
      css-mode less-mode sass-mode scss-mode
      js2-mode typescript-mode go-mode
      groovy-mode graphql-mode) . lsp-deferred)
   (lsp-mode . lsp-headerline-breadcrumb-mode))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  ;; Increase the amount of data which Emacs reads from the process 1mb
  ;; default is 4k while some of the language server responses are in 800k - 3M range
  (setq read-process-output-max (* 1024 1024))
  :custom
  (lsp-enable-snippet nil "not yet configured")
  (lsp-session-file (concat fate-cache-directory "lsp-session-v1"))
  (lsp-headerline-breadcrumb-segments '(file symbols)))


(use-package lsp-ui
  :defer t
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-sideline-enable nil "Hide sideline")
  (lsp-ui-doc-enable nil "Disable lsp doc for now as size is not property handled ")
  (lsp-ui-peek-always-show t "Show peek even only one matching"))

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(provide 'fate-lsp)
;;; fate-lsp.el ends here
