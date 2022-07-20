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
  :disabled
  :hook
  (after-init . global-company-mode)
  :bind
  (("C-M-/" . company-complete)
   ([remap indent-for-tab-command] . company-indent-or-complete-common)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-tooltip-align-annotations t "aligns annotation to the right")
  (company-tooltip-limit 12 "bigger popup window")
  (company-idle-delay .5 "decrease delay before autocompletion popup shows")
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

(use-package company-prescient
  :hook (company . company-prescient-mode))

(provide 'fate-company)
;;; fate-company.el ends here
