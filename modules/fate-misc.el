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

(defun fate/draw-file-tree (file-tree &optional level branch prefix)
  "FILE-TREE is an S-expression of file system structure.
LEVEL is indent level of current recurssion.
BRANCH is either ├ or └.   PREFIX is the prefix before branch and bar.
\=(foo (bar a) b) stands for folder foo has folder bar and file b,
folder bar has file a."
  (let* ((text-tree "")
         (indent-level (or level 0))
         (prefix (or prefix ""))
         (bar "── ")
         (index 1)
         (dir-name (car file-tree))
         (sub-tree (cdr file-tree))
         (branch (or branch "├"))
         (is-last (string= branch "└")))

    ;; (print (format "%d %s prefix %s is last %s" indent-level dir-name prefix is-last))
    (setq text-tree (concat
                     text-tree
                     prefix (if (> indent-level 0) (concat branch bar ) "") dir-name "\n"))

    (setq indent-level (+ 1 indent-level))
    (dolist (entry sub-tree)
      (let* ((tree-length (length sub-tree))
             (branch (if (eq index tree-length) "└" "├"))
             (prefix (if (> indent-level 1) (concat prefix (if is-last "    " "│   ")) "")))
        (cond
         ((stringp entry)
          ;; (print (format "%d %s prefix %s index %d tree-length %d" indent-level entry prefix index tree-length))
          (setq text-tree (concat
                           text-tree prefix branch bar entry "\n")))
         ((listp entry)
          (setq text-tree (concat
                           text-tree
                           (fate/draw-file-tree entry indent-level branch prefix))))))
      (setq index (+ index 1)))
    text-tree))

(provide 'fate-misc)
;;; fate-misc.el ends here
