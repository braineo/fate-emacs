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
  :straight (:host github
             :repo "manateelazycat/lsp-bridge"
             :files ("*" (:exclude ".git")))
  :hook (after-init . global-lsp-bridge-mode)
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (acm-markdown-render-font-height 80)
  :bind
  (:map lsp-bridge-mode-map
    ([remap xref-find-definitions] . lsp-bridge-find-def)
    ([remap xref-find-references] . lsp-bridge-find-references)
    ([remap view-hello-file] . lsp-bridge-popup-documentation))
  :init
  (with-eval-after-load 'hydra
   (defhydra hydra-lsp (:exit t :hint nil)
     "
 Action^^               Documentation^^            Server
-------------------------------------------------------------------------------------
 [_f_] format           [_i_] implementation       [_M-r_] restart
 [_x_] execute action   [_D_] definition           [_o_] documentation
 [_r_] rename           [_R_] references           [_d_] diagnostic"
     ("D" lsp-bridge-find-def)
     ("R" lsp-bridge-find-references)
     ("i" lsp-bridge-find-impl)
     ("o" lsp-bridge-lookup-documentation)
     ("r" lsp-bridge-rename)
     ("f" lsp-bridge-code-format)
     ("x" lsp-bridge-code-action)
     ("d" lsp-bridge-diagnostic-list)
     ("M-r" lsp-bridge-restart-process))))

(provide 'fate-lsp)
;;; fate-lsp.el ends here
