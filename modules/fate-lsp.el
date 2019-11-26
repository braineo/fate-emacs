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
  (require 'flycheck))

(defun fate-lsp-setup-python ()
  "Microsoft Python Language Server does not have a syntax checker, setup one for it."
  (progn
    (require 'lsp-python-ms)
    (lsp)
    (setq-local flycheck-checker 'python-flake8)))

(use-package lsp-python-ms
  :hook (python-mode . fate-lsp-setup-python))

(use-package lsp-mode
  :defer t
  :diminish lsp-mode
  :hook
  ((sh-mode c-mode c++-mode
    html-mode web-mode json-mode
    css-mode less-mode sass-mode scss-mode
    js-mode js2-mode typescript-mode
    groovy-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  :config
  (require 'lsp-clients)
  :custom
  (lsp-prefer-flymake nil "prefer using flycheck")
  (lsp-enable-snippet nil "not yet configured"))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-sideline-enable nil "Hide sideline")
  (lsp-ui-peek-always-show t "Show peek even only one matching")
  (lsp-session-file (concat fate-cache-directory "lsp-session-v1")))

(use-package company-lsp
  :after company
  :init
  (setq company-lsp-cache-candidates 'auto)
  (cl-pushnew 'company-lsp company-backends))

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(provide 'fate-lsp)
;;; fate-lsp.el ends here
