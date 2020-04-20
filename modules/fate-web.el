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


(use-package prettier-js
  :defer t
  :defines prettier-js-args prettier-js)

(defun fate/prettier-minor-mode ()
  "Enable minor mdoe for certain extensions."
  (when (buffer-file-name)
    (cl-dolist (extension '(".js[x]?" ".ts[x]?" ".[s]?css" ".less" ".vue" ".json" ".gql" ".md"))
      (when (string-match-p extension buffer-file-name)
        (prettier-js-mode)
        (cl-return)))))

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
    ("C-c C-l" . fate/json-prettier)))

(use-package js-mode
  :ensure nil
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :hook (js-mode . fate/prettier-minor-mode))

(use-package typescript-mode
  :mode ("\\.ts[x]?\\'")
  :hook (typescript-mode . fate/prettier-minor-mode))

(use-package web-mode
  :mode ("\\.ejs\\'"
         "\\.html\\'"))

(provide 'fate-web)
;;; fate-web.el ends here
