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

(defun fate-init-parinfer-mode ()
  "Disable some minor modes having troubles with parinfer-rus-mode."
  (progn
    (electric-pair-local-mode -1)
    (parinfer-rust-mode)))

(use-package parinfer-rust-mode
  :hook
  ((emacs-lisp-mode scheme-mode) . fate-init-parinfer-mode)
  :init
  (setq parinfer-rust-auto-download t))


(provide 'fate-lisp)
;;; fate-lisp.el ends here
