;;; fate-term.el --- configuration for term        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Binbin Ye

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

(use-package ghostel)

(defun fate/vterm-split-right ()
  "Split windwow and create a new term horizontally."
  (interactive)
  (let* ((ignore-window-parameters t))
    (select-window (split-window-horizontally))
    (ghostel)))

(defun fate/vterm-split-below ()
  "Split windwow and create a new term horizontally."
  (interactive)
  (let* ((ignore-window-parameters t))
    (select-window (split-window-vertically))
    (ghostel)))

(use-package vterm
  :hook (vterm-mode . (lambda()
                        (setq-local global-hl-line-mode nil)))
  :bind (:map vterm-mode-map
          ("C-x 3" . fate/vterm-split-right)
          ("C-x 2" . fate/vterm-split-below))
  :custom (vterm-buffer-name-string "vterm %s"))

(use-package vterm-toggle
  :bind
  (("<f3>" . vterm-toggle-cd)
   (:map vterm-mode-map
         ("<f3>" . vterm-toggle-cd))))



(provide 'fate-term)
;;; fate-term.el ends here
