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

(eval-when-compile
  (require 'fate-misc))

(defconst fate/js-tools
   '("prettier"
     "vscode-langservers-extracted"
     "@typescript/native-preview")
    "Web development tools.")

(fate/create-install-tools!
  "js" ("npm" "install" "-g") fate/js-tools)

(use-package prettier-js
  :defer t
  :defines prettier-js-args prettier-js)

(defun fate/prettier-minor-mode ()
  "Enable minor mdoe for certain extensions."
  (when (and buffer-file-name
             (string-match-p "\\.\\(m?jsx?\\|tsx?\\|s?css\\|less\\|vue\\|json\\|gql\\|graphql\\|md\\)\\'"
                             buffer-file-name)
             ;; Do not auto format anything inside node_modules or temp dirs
             (not (string-match-p "node_modules\\|\\`/run/\\|\\`/te?mp/" buffer-file-name)))
    (prettier-js-mode)))

(defvar-local jq-format-args '())


;;;###autoload
(defun fate/json-pretty-print (&optional minimize)
  "Pretty-print current buffer.  when MINIMIZE is set, minimize JSON document."
  (interactive "P")
  (when minimize
    (setq-local jq-format-args '("-c")))
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (if (executable-find "jq")
        (json-jq-region beg end nil)
      (json-pretty-print beg end minimize)))

  (setq-local jq-format-args '()))


(reformatter-define json-jq
  :program "jq"
  :args jq-format-args
  :lighter "JQ")

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'")
  :bind (:map json-ts-mode-map
          ("C-c C-p" . json-ts-jq-path-at-point)
          ("C-c C-l" . fate/json-pretty-print)))

;; The feature is `css-mode'; `:config' of `use-package css-ts-mode' never ran.
(use-package css-mode
  :ensure nil
  :mode ("\\.less\\'" . css-ts-mode)
  :hook (css-ts-mode . fate/prettier-minor-mode)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.module.less\\'" "\\1.tsx")))

(use-package jtsx
  :mode
  (("\\.[cm]?jsx?\\'" . jtsx-jsx-mode)
   ("\\.m?tsx\\'" . jtsx-tsx-mode)
   ("\\.m?ts\\'" . jtsx-typescript-mode))
  :hook ((javascript-ts-mode typescript-ts-mode tsx-ts-mode) . fate/prettier-minor-mode)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.ts\\'" "\\1.spec.ts"))
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.tsx\\'" "\\1.spec.tsx" "\\1.scss" "\\1.module.less" "\\1.sass" "\\1.css"))
  :bind (:map jtsx-tsx-mode-map
              ("C-c C-j" . jtsx-jump-jsx-element-tag-dwim)
              ("C-c C-w" . jtsx-wrap-in-jsx-element)
              ("C-c C-u" . jtsx-unwrap-jsx)
              ("C-c C-r" . jtsx-rename-jsx-element))
  :custom
  (jtsx-enable-jsx-element-tags-auto-sync nil "disabled because sometimes it is buggy due to node parsing"))


(use-package web-mode
  :mode ("\\.ejs\\'"
          "\\.html\\'")
  :custom
  (web-mode-enable-auto-indentation nil "Use prettier on save instead")
  (web-mode-enable-auto-quoting nil "annoying when writing arrow function in a tag")
  :hook (web-mode . fate/prettier-minor-mode))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :hook (graphql-ts-mode . fate/prettier-minor-mode)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.gql.ts\\'" "\\1.graphql")))


(provide 'fate-web)
;;; fate-web.el ends here
