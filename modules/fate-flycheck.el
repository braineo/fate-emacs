;;; fate-flycheck.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

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

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (progn
    ;; Fix error list to bottom of window
    (add-to-list 'display-buffer-alist
      `(,(rx bos "*Flycheck errors*" eos)
         (display-buffer-reuse-window
           display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.2)))
    (flycheck-define-checker fate-json-jq
      "JSON checker using the jq tool.
This checker accepts multiple consecutive JSON values in a
single input, which is useful for jsonlines data.
See URL `https://stedolan.github.io/jq/'."
      :command ("jq" "." source null-device)
      ;; Example error message:
      ;;   parse error: Expected another key-value pair at line 3, column 1
      :error-patterns
      ((error line-start
              (optional "parse error: ")
              (message) "at line " line ", column " column
              (zero-or-more not-newline) line-end))
      :modes fate-json-mode)
    (add-to-list 'flycheck-checkers 'fate-json-jq))
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (global-flycheck-mode . flycheck-pos-tip-mode))

(provide 'fate-flycheck)
;;; fate-flycheck.el ends here
