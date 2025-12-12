;;; fate-misc.el --- misc functions                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  binbin

;; Author: Binbin
;; Keywords: helper

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


;;; Code:

(defun fate/no-caps (&optional restore)
  "Set capslock as control.  When RESTORE is set, restore the capslock position."
  (interactive "P")
  (let* ((option (if restore
                   "" ;; `setxkbmap -option` only adds options, need to replace the entire options
                   "caps:ctrl_modifier"))
         (command (format "setxkbmap -option -option %s" option)))
    (message command)
    (shell-command command)))

(defun font-installed-p (font-name)
  "Check if font FONT-NAME is installed."
  (if (find-font (font-spec :name font-name)) t nil))

(defun fate/draw-file-tree (tree &optional prefix is-last)
  "Draw a file tree from TREE structure.
TREE is an S-expression: (name child1 child2 ...)
PREFIX is the accumulated prefix string for indentation.
IS-LAST indicates if this is the last child at current level."
  (let* ((name (car tree))
         (children (cdr tree))
         (branch (cond ((null prefix) "")
                       (is-last "└── ")
                       (t "├── ")))
         (result (concat prefix branch name "\n"))
         (child-prefix (cond ((null prefix) "")
                            (is-last (concat prefix "    "))
                            (t (concat prefix "│   ")))))
    (concat result
            (mapconcat
              (lambda (child)
                (let* ((is-last-child (eq child (car (last children)))))
                  (if (stringp child)
                    ;; Leaf node
                    (concat child-prefix (if is-last-child "└── " "├── ") child "\n")
                    ;; Subtree
                    (fate/draw-file-tree child child-prefix is-last-child))))
             children
             ""))))

(provide 'fate-misc)
;;; fate-misc.el ends here
