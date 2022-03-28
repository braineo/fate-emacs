;;; fate-web.el --- Web development setup            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Binbin Ye

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

(require 'tree-sitter)

(use-package prettier-js
  :defer t
  :defines prettier-js-args prettier-js)

(defun fate/prettier-minor-mode ()
  "Enable minor mdoe for certain extensions."
  (when (buffer-file-name)
    (cl-dolist (extension '("\\.js[x]?\\'" "\\.ts[x]?\\'" "\\.[s]?css\\'" "\\.less\\'"
                             "\\.vue\\'" "\\.json\\'" "\\.gql\\'" "\\.graphql\\'" "\\.md\\'"))
      (when (and (string-match-p extension buffer-file-name)
              ;; Do not auto format anything inside node_modules
              (not (string-match-p "node_modules" buffer-file-name)))
        (prettier-js-mode)
        (cl-return)))))

(defun fate/json-get-path (current-node output)
  "Get path to json value at cursor position.  CURRENT-NODE is a tree-sitter-node.
OUTPUT is parsed path list."
  (let* ((parent-node (tsc-get-parent current-node)))
    (if parent-node
      (progn
        (when (eq (tsc-node-type parent-node) 'array)
          (let ((index -1)
                (cursor (tsc-make-cursor parent-node)))
            (tsc-goto-first-child cursor)
            (while (not (tsc-node-eq current-node (tsc-current-node cursor)))
                (progn
                  (tsc-goto-next-sibling cursor)
                  (if (tsc-node-named-p (tsc-current-node cursor))
                    (progn
                      (setq index (+ index 1))))))
            (setq output (push index output))))
        (when (eq (tsc-node-type current-node) 'pair)
            (setq output (push (tsc-node-text (tsc-get-nth-child current-node 0)) output)))
        (fate/json-get-path parent-node output))
      output)))

(defun fate/json-print-path-js ()
  "Copy json path in JavaScript format."
  (interactive)
  (let (json-path)
    (dolist (elt (fate/json-get-path (tree-sitter-node-at-pos) '()) json-path)
      (when (stringp elt)
        (let* ((trimmed-elt (string-trim elt "\"" "\"")))
          (if (string-match-p "-" trimmed-elt)
            (setq json-path (concat json-path "[" trimmed-elt "]"))
            (setq json-path (concat json-path "." trimmed-elt)))))
      (when (numberp elt)
        (setq json-path (concat json-path "[" (number-to-string elt) "]"))))
    (message json-path)
    (kill-new json-path)))

(use-package json-mode
  :mode "\\.json?\\'"
  :config
  (defun fate/json-prettier ()
    "Tell prettier the content is to be parsed as JSON regardless of any file extensions."
    (interactive)
    (setq-local prettier-js-args '("--parser=json"))
    (prettier-js))
  :hook
  (json-mode . fate/prettier-minor-mode)
  :bind
  (:map json-mode-map
    ("C-c C-l" . fate/json-prettier)
    ("C-c C-p" . fate/json-print-path-js)))

(use-package less-css-mode
  :hook (less-css-mode . fate/prettier-minor-mode))

(use-package js-mode
  :ensure nil
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :hook (js-mode . fate/prettier-minor-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'")
  :hook (typescript-mode . fate/prettier-minor-mode))

(define-derived-mode typescript-tsx-mode typescript-mode "TSX"
  "Derived mode for better syntax highlight and linter config.")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

(use-package tide
  :after typescript-mode
  :init (tide-start-server-if-nonexistent)
  :commands (tide-current-server tide-start-server-if-nonexistent tide-jsdoc-template))

(use-package web-mode
  :mode ("\\.ejs\\'"
          "\\.html\\'")
  :custom
  (web-mode-enable-auto-indentation nil "Use prettier on save instead")
  (web-mode-enable-auto-quoting nil "annoying when writting arrow function in a tag")
  :hook (web-mode . fate/prettier-minor-mode))

(use-package graphql-mode
  :mode ("\\.graphql\\'")
  :hook (graphql-mode . prettier-js-mode))

(use-package jest
  :defer t)

(provide 'fate-web)
;;; fate-web.el ends here
