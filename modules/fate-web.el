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

(defvar apheleia-formatter)
(declare-function apheleia-mode "apheleia")
(declare-function apheleia-format-buffer "apheleia")

(defun fate/oxfmt-project-p ()
  "Non-nil if the project root has an oxfmt config."
  (when-let* ((project (project-current)))
    (directory-files (project-root project) nil
                     "\\`\\(\\.oxfmtrc\\.jsonc?\\|oxfmt\\.config\\.[cm]?[jt]s\\)\\'")))

(defun fate/web-formatter ()
  "Return the apheleia formatter for the current project."
  (if (fate/oxfmt-project-p) 'oxfmt 'prettier))

(defun fate/web-format-on-save ()
  "Enable format on save for certain extensions.
Use oxfmt when the project root configures it, otherwise prettier."
  (when (and buffer-file-name
             (string-match-p "\\.\\(m?jsx?\\|tsx?\\|s?css\\|less\\|vue\\|json\\|gql\\|graphql\\|md\\)\\'"
                             buffer-file-name)
             ;; Do not auto format anything inside node_modules or temp dirs
             (not (string-match-p "node_modules\\|\\`/run/\\|\\`/te?mp/" buffer-file-name)))
    (setq-local apheleia-formatter (fate/web-formatter))
    (apheleia-mode)))

;;;###autoload
(defun fate/json-pretty-print (&optional minimize)
  "Format current buffer with oxfmt or prettier.
When MINIMIZE is set, minimize with jq.  When a region is active or
the buffer has no file, use `json-pretty-print'."
  (interactive "P")
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (cond
     (minimize
      (let ((out (generate-new-buffer " *jq*")))
        (unwind-protect
            (if (zerop (call-process-region beg end "jq" nil out nil "-c" "."))
                (progn (delete-region beg end)
                       (goto-char beg)
                       (insert-buffer-substring out))
              (user-error "jq: %s" (with-current-buffer out (string-trim (buffer-string)))))
          (kill-buffer out))))
     ((or (use-region-p) (not buffer-file-name))
      (json-pretty-print beg end))
     (t (apheleia-format-buffer (fate/web-formatter))))))

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
  :hook (css-ts-mode . fate/web-format-on-save)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.module.less\\'" "\\1.tsx")))

(use-package jtsx
  :mode
  (("\\.[cm]?jsx?\\'" . jtsx-jsx-mode)
   ("\\.m?tsx\\'" . jtsx-tsx-mode)
   ("\\.m?ts\\'" . jtsx-typescript-mode))
  :hook ((javascript-ts-mode typescript-ts-mode tsx-ts-mode) . fate/web-format-on-save)
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
  :hook (web-mode . fate/web-format-on-save))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :hook (graphql-ts-mode . fate/web-format-on-save)
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.gql.ts\\'" "\\1.graphql")))


(provide 'fate-web)
;;; fate-web.el ends here
