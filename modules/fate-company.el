;;; fate-company.el ---                              -*- lexical-binding: t; -*-

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

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (("C-M-/" . company-complete)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("TAB" . company-complete-common-or-cycle)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .5               ; decrease delay before autocompletion popup shows
        company-minimum-prefix-length 2
        company-require-match nil))

(provide 'fate-company)
;;; fate-company.el ends here
