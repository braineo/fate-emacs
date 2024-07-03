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

(require 'treesit)
(eval-when-compile
  (require 'core-packages))

(defconst fate/js-tools
   '("prettier"
     "vscode-langservers-extracted"
     "typescript-language-server")
    "Web development tools.")

(fate/create-install-tools!
  "js" ("npm" "install" "-g") fate/js-tools)

(use-package prettier-js
  :defer t
  :defines prettier-js-args prettier-js)

(defun fate/prettier-minor-mode ()
  "Enable minor mdoe for certain extensions."
  (when (buffer-file-name)
    (cl-dolist (extension '("\\.[m]?js[x]?\\'" "\\.ts[x]?\\'" "\\.[s]?css\\'" "\\.less\\'"
                             "\\.vue\\'" "\\.json\\'" "\\.gql\\'" "\\.graphql\\'" "\\.md\\'"))
      (when (and (string-match-p extension buffer-file-name)
              ;; Do not auto format anything inside node_modules
              (or (not (string-match-p "node_modules" buffer-file-name))
                (not string-match-p "^/run/" buffer-file-name)
                (not string-match-p "^/te?mp/" buffer-file-name)))
        (prettier-js-mode)
        (cl-return)))))

(defvar-local jq-foramt-args '())

(defun fate/json-get-path (current-node output)
  "Get path to json value at cursor position.  CURRENT-NODE is a tree-sitter-node.
OUTPUT is parsed path list."
  (let* ((parent-node (treesit-node-parent current-node)))
    (if parent-node
      (progn
        (when (equal (treesit-node-type parent-node) "array")
          (let ((index -1)
                (cursor (treesit-node-child parent-node 0)))
            (while (not (treesit-node-eq current-node cursor))
                (progn
                  (setq cursor (treesit-node-next-sibling cursor t))
                  (if cursor
                    (progn
                      (setq index (+ index 1))))))
            (setq output (push index output))))
        (when (equal (treesit-node-type current-node) "pair")
            (setq output (push (substring-no-properties (treesit-node-text (treesit-node-child current-node 0))) output)))
        (fate/json-get-path parent-node output))
      output)))

;;;###autoload
(defun fate/json-print-path-js ()
  "Show json path in minibuffer in JavaScript, jq format."
  (interactive)
  (let (json-path)
    (dolist (elt (fate/json-get-path (treesit-node-at (point)) '()) json-path)
      (when (stringp elt)
        (let* ((trimmed-elt (string-trim elt "\"" "\"")))
          (if (string-match-p "[^[:word:]]" trimmed-elt)
            (setq json-path (concat json-path "[\"" trimmed-elt "\"]"))
            (setq json-path (concat json-path "." trimmed-elt)))))
      (when (numberp elt)
        (setq json-path (concat json-path "[" (number-to-string elt) "]"))))
    (message json-path)))

;;;###autoload
(defun fate/json-kill-path-js ()
  "Save json path to kill ring."
  (interactive)
  (kill-new (fate/json-print-path-js)))

;;;###autoload
(defun fate/json-pretty-print (&optional minimize)
  "Pretty-print current buffer.  when MINIMIZE is set, minimize JSON document."
  (interactive "P")
  (when minimize
    (setq-local jq-foramt-args '("-c")))
  (when (use-region-p)
    (cond ((executable-find "jq") (json-jq-region (region-beginning) (region-end) nil))
          (t (json-pretty-print (region-beginning) (region-end) minimize))))
  (cond ((executable-find "jq") (json-jq-buffer nil))
        (t (json-pretty-print (point-min) (point-max) minimize)))

  (setq-local jq-foramt-args '()))


(defgroup jqfmt nil
  "Reformat JSON using jq."
  :group 'languages)

(defcustom jqfmt-command "jq"
  "Command used for reformatting."
  :group 'jqfmt
  :type 'string)

(reformatter-define json-jq
  :program jqfmt-command
  :args jq-foramt-args
  :lighter "JQ"
  :group 'jqfmt)

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'")
  :bind (:map json-ts-mode-map
          ("C-c P" . fate/json-print-path-js)
          ("C-c C-p" . fate/json-kill-path-js)
          ("C-c C-l" . fate/json-pretty-print)))

(use-package css-ts-mode
  :ensure nil
  :mode ("\\.less\\'")
  :hook (css-ts-mode . fate/prettier-minor-mode)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.module.less\\'" "\\1.tsx")))

(use-package jtsx
  :mode
  (("\\.[cm]?jsx?\\'" . jtsx-jsx-mode)
   ("\\.tsx\\'" . jtsx-tsx-mode)
   ("\\.ts\\'" . jtsx-typescript-mode))
  :hook ((javascript-ts-mode typescript-ts-mode tsx-ts-mode) . fate/prettier-minor-mode)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.ts\\'" "\\1.spec.ts"))
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.tsx\\'" "\\1.spec.tsx" "\\1.scss" "\\1.module.less" "\\1.sass" "\\1.css")))

(use-package tide
  :after typescript-ts-mode
  :init (tide-start-server-if-nonexistent)
  :commands (tide-current-server tide-start-server-if-nonexistent tide-jsdoc-template))

(use-package web-mode
  :mode ("\\.ejs\\'"
          "\\.html\\'")
  :custom
  (web-mode-enable-auto-indentation nil "Use prettier on save instead")
  (web-mode-enable-auto-quoting nil "annoying when writting arrow function in a tag")
  :hook (web-mode . fate/prettier-minor-mode))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :hook (graphql-ts-mode . fate/prettier-minor-mode)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql")))
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.gql.ts\\'" "\\1.graphql")))

(use-package jest
  :defer t)

(provide 'fate-web)
;;; fate-web.el ends here
