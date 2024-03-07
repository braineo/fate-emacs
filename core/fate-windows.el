;;; fate-windows.el --- window management packages   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Binbin Ye

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

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode)
  :custom
  (winner-boring-buffers '("*Completions*"
                            "*Compile-Log*"
                            "*inferior-lisp*"
                            "*Fuzzy Completions*"
                            "*Apropos*"
                            "*Help*"
                            "*cvs*"
                            "*Buffer List*"
                            "*Ibuffer*"
                            "*esh command on file*")))

(use-package shackle
  :commands shackle-mode
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-rule '(:select t))
  (shackle-rules
    ;;   :regexp     :select  :custom  :inhibit-window-quit :ignore :other :popup :align :size :frame
    '((flycheck-error-list-mode  :select nil :size 0.2   :align bottom)
      (calendar-mode :select t :size 0.2 :align bottom)
      ((help-mode helpful-mode)  :select t :size 80 :align right))))


(use-package popper
  :hook (after-init . popper-mode)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode  helpful-mode
                              compilation-mode)))

(provide 'fate-windows)
;;; fate-windows.el ends here
