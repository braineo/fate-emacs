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

(provide 'fate-misc)
;;; fate-misc.el ends here
