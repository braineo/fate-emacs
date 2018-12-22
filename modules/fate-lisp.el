;;; fate-lisp.el ---                                 -*- lexical-binding: t; -*-

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

(use-package parinfer
  :hook
  ((emacs-lisp-mode scheme-mode) . parinfer-mode)
  :defer t
  :init
  (setq parinfer-extensions
        '(defaults       ; should be included.
          pretty-parens  ; different paren styles for different modes.
          evil           ; If you use Evil.
          lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
          paredit        ; Introduce some paredit commands.
          smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
          smart-yank)))   ; Yank behavior depend on mode.

(provide 'fate-lisp)
;;; fate-lisp.el ends here
