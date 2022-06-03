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


(use-package json-mode
  :load-path "modules/fate-json-mode")

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
