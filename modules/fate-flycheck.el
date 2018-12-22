;;; fate-flycheck.el ---                             -*- lexical-binding: t; -*-

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

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled) "Only check while saving and opening files")
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (global-flycheck-mode . flycheck-pos-tip-mode))

(provide 'fate-flycheck)
;;; fate-flycheck.el ends here
