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



(defun fate-lsp-setup-python ()
  "Microsoft Python Language Server does not have a syntax checker, setup one for it."
  (progn
    (require 'lsp-python-ms)
    (lsp)
    ;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-626210720
    ;; Do not let lsp hijack flycheck
    (setq-local lsp-diagnostics-provider :none)
    (setq-local flycheck-checker 'python-flake8)))

(defun fate-lsp-setup-go ()
  "Use gopls for format and import sorting."
  (progn
    (require 'lsp)
    (lsp)
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

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
   (go-mode . fate-lsp-setup-go)
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
  (with-eval-after-load 'lsp
     (require 'fate-lsp-gql))
  (with-eval-after-load 'hydra
   (defhydra hydra-lsp (:exit t :hint nil)
     "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
     ("d" lsp-find-declaration)
     ("D" lsp-ui-peek-find-definitions)
     ("R" lsp-ui-peek-find-references)
     ("i" lsp-ui-peek-find-implementation)
     ("t" lsp-find-type-definition)
     ("s" lsp-signature-help)
     ("o" lsp-describe-thing-at-point)
     ("r" lsp-rename)

     ("f" lsp-format-buffer)
     ("m" lsp-ui-imenu)
     ("x" lsp-execute-code-action)

     ("M-s" lsp-describe-session)
     ("M-r" lsp-restart-workspace)
     ("S" lsp-shutdown-workspace)))
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
