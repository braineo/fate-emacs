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

(use-package lsp-mode
  :defer t
  :diminish lsp-mode
  :hook
  ((python-mode sh-mode
    html-mode web-mode json-mode
    css-mode less-mode sass-mode scss-mode
    js-mode js2-mode typescript-mode
    groovy-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (require 'lsp-clients))

;; (use-package company-lsp
;;   :defer t
;;   :commands company-lsp)

(provide 'fate-lsp)
;;; fate-lsp.el ends here
