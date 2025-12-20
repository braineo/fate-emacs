;;; fate-json.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Binbin Ye

;; Author: Binbin Ye
;; Keywords: JSON

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

(require 'treesit)

(defun fate/json-get-path (node)
  "Get a path list from root of JSON to NODE.  NODE is a tree-sitter-node.
OUTPUT is parsed path list."
  (let* ((path '())
         (current node))
    (while current
      (let ((parent (treesit-node-parent current)))
        (cond
          ((string= (treesit-node-type parent) "array")
           (let ((cursor (treesit-node-child parent 0))
                 (index -1))
             (while (not (treesit-node-eq current cursor))
               (setq cursor (treesit-node-next-sibling cursor t))
               (when cursor (setq index (1+ index))))
             (push index path)))
          ((string= (treesit-node-type current) "pair")
           (push (treesit-node-text (treesit-node-child current 0) t) path)))
        (setq current parent)))
    path))

;;;###autoload
(defun fate/json-print-path-js ()
  "Show JSON path at point in JavaScript/jq format in minibuffer."
  (interactive)
  (message (fate/json-get-path-string-js
             (fate/json-get-path (treesit-node-at (point))))))

(defun fate/json-get-path-string-js (path)
  "Convert PATH list to JavaScript/jq format."
  (mapconcat
    (lambda (elt)
      (cond
        ((numberp elt) (format "[%d]" elt))
        ((stringp elt)
         (let ((trimmed (string-trim elt "\"" "\"")))
           (if (or (string-match-p "[^[:word:]]" trimmed) (string-match-p "^[[:digit:]]+$" trimmed))
             (format "[%s]" elt)
             (format ".%s" trimmed))))))
    path ""))

(defun fate/json-print-path-python ()
  "Show JSON path at point in Python format in minibuffer."
  (interactive)
  (message
    (mapconcat
      (lambda (elt)
        (cond
          ((numberp elt) (format "[%d]" elt))
          ((stringp elt) (format "[%s]" elt))))
      (fate/json-get-path (treesit-node-at (point))) "")))

;;;###autoload
(defun fate/json-kill-path-js ()
  "Save json path to kill ring."
  (interactive)
  (kill-new (fate/json-print-path-js)))

(provide 'fate-json)
;;; fate-json.el ends here


