;;; fate-align-text.el ---                           -*- lexical-binding: t; -*-

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

;; align text method copy from spacemacs

;;; Code:

 ;; BEGIN align functions

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun fate/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun fate/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro fate|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "fate/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (fate/align-repeat start end ,regexp ,justify-right after)))))

(fate|create-align-repeat-x "comma" "," nil t)
(fate|create-align-repeat-x "semicolon" ";" nil t)
(fate|create-align-repeat-x "colon" ":" nil t)
(fate|create-align-repeat-x "equal" "=")
(fate|create-align-repeat-x "math-oper" "[+\\-*/]")
(fate|create-align-repeat-x "ampersand" "&")
(fate|create-align-repeat-x "bar" "|")
(fate|create-align-repeat-x "left-paren" "(")
(fate|create-align-repeat-x "right-paren" ")" t)
(fate|create-align-repeat-x "backslash" "\\\\")
(fate|create-align-repeat-x "period" "\\\.")

;; END align functions

(defhydra hydra-align-text (:hint nil)
    "
Align text
^^^^^^^^-------------------------------------------------
[_x_] regex         [_,_] comma       [_=_] equal
[_;_] semicolon     [_._] period      [_o_] math-oper
[_:_] colon         [_&_] ampersand
"
  ("x" fate/align-repeat)
  ("," fate/align-repeat-comma)
  (";" fate/align-repeat-semicolon)
  (":" fate/align-repeat-colon)
  ("=" fate/align-repeat-equal)
  ("o" fate/align-repeat-math-oper)
  ("&" fate/align-repeat-ampersand)
  ("|" fate/align-repeat-bar)
  ("(" fate/align-repeat-left-paren)
  (")" fate/align-repeat-right-paren)
  ("\\" fate/align-repeat-backslash)
  ("\." fate/align-repeat-period))

(provide 'fate-align-text)
;;; fate-align-text.el ends here
