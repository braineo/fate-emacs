;;; fate-sg.el --- integration of ast-grep           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Binbin Ye

;; Author: Binbin Ye
;; Keywords: ast-grep

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


(require 'transient)


;;; Config

(defgroup fate-sg nil
  "Search and refactor code based on ast-grep."
  :group 'fate-sg)


(defcustom fate-sg-buffer "*fate-sg*"
  "The buffer name of search result."
  :type 'string
  :group 'fate-sg)




(defvar fate-sg-pattern nil
  "History of patterns for sg command.")

(defvar fate-sg-replacement nil
  "History of replacements for sg command.")

(defvar fate-sg-path nil
  "History of paths for sg command.")

(transient-define-infix fate-sg--pattern ()
  :transient t
  :argument "--pattern "
  :class 'transient-option
  :description "Search pattern"
  ;; :variable fate-sg-pattern
  :key "p")

(transient-define-infix fate-sg--rewrite ()
  :transient t
  :argument "--rewrite "
  :class 'transient-option
  :description "Search rewrite"
  :key "r")

(transient-define-infix fate-sg--path ()
  :transient t
  :class 'transient-files
  :description "Path to execute sg"
  :argument "-- "
  :key "f")

(transient-define-suffix fate-sg--run ()
  :transient nil
  :description "Execute search"
  :key "x"
  (interactive)
  (message "run")
  (fate/sg-run (transient-args 'sg-transient)))


;; TODO: add arguments: pattern, replace, dir / file / project, hidden.

(defun fate/sg-run (args)
  "Run sg can get result."
  (interactive)
  (if (get-buffer fate-sg-buffer)
      (with-current-buffer fate-sg-buffer
        (progn
          (text-mode)
          (read-only-mode -1)
          (erase-buffer)))
    (generate-new-buffer fate-sg-buffer))
  ;;"sg -p 'export default $C;' /Users/binbinye/Development/genshin-wish/client"
  (with-current-buffer fate-sg-buffer
    (compilation-start (mapconcat 'identity (cons (executable-find "sg") args) " ") 'text-mode)))



(provide 'fate-sg)
;;; fate-sg.el ends here
