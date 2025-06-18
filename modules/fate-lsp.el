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
  (require 'fate-flycheck))


(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge")
  :hook (after-init . global-lsp-bridge-mode)
  :config
  (add-to-list
    'lsp-bridge-single-lang-server-extension-list
    '(("less") . "vscode-css-language-server"))
  (with-eval-after-load 'fate-flycheck-bridge
    (add-hook 'lsp-bridge-mode-hook #'flycheck-lsp-bridge-setup))
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (acm-markdown-render-font-height 80)
  (acm-enable-tabnine nil)
  (acm-backend-lsp-match-mode "fuzzy")
  (lsp-bridge-user-langserver-dir (expand-file-name "langserver" fate-directory))
  (lsp-bridge-enable-completion-in-string t)
  (lsp-bridge-markdown-lsp-server "harper-ls")
  :bind
  (:map lsp-bridge-mode-map
    ([remap xref-find-definitions] . lsp-bridge-find-def)
    ([remap xref-go-back] . lsp-bridge-find-def-return)
    ([remap xref-find-references] . lsp-bridge-find-references)
    ([remap view-hello-file] . lsp-bridge-popup-documentation))
  :init
  (with-eval-after-load 'transient
    (transient-define-prefix lsp-transient ()
      "LSP Transient"

      [["Action"
        ("f" "Format" lsp-bridge-code-format)
        ("r" "Rename" lsp-bridge-rename)
        ("x" "Action" lsp-bridge-code-action)]
       ["Code"
        ("i" "Implementation" lsp-bridge-find-impl)
        ("D" "Definition" lsp-bridge-find-def)
        ("R" "References" lsp-bridge-find-references)]
       ["Debug"
        ("o" "Documentation" lsp-bridge-popup-documentation)
        ("d" "Diagnostic" lsp-bridge-diagnostic-list)
        ("M-r" "Restrat" lsp-bridge-restart-process)]])))



(provide 'fate-lsp)
;;; fate-lsp.el ends here
