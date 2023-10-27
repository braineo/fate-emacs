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
    (add-to-list 'flycheck-checkers 'fate-json-jq)

    (flycheck-define-checker python-ruff
      "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
        :command ("ruff"
                  "--format=text"
                  (eval (when buffer-file-name
                          (concat "--stdin-filename=" buffer-file-name)))
                  "-")
     :standard-input t
     :error-filter (lambda (errors)
                     (let ((errors (flycheck-sanitize-errors errors)))
                       (seq-map #'flycheck-flake8-fix-error-level errors)))
     :error-patterns
     ((warning line-start
               (file-name) ":" line ":" (optional column ":") " "
               (id (one-or-more (any alpha)) (one-or-more digit)) " "
               (message (one-or-more not-newline))
               line-end))
     :modes python-mode)
    (add-to-list 'flycheck-checkers 'python-ruff))

  (flycheck-def-executable-var qt-qmllint "qmllint")
  (flycheck-define-checker qt-qmllint
      "A QML syntatic validity checker provided by QT
`flycheck-qt-qmllint-executable'.
See URL `https://doc.qt.io/qt-6/qtquick-tool-qmllint.html'."
     :command ("qmllint" source)
     :error-patterns
     ((error line-start
               (file-name) ":" line " : "
               (message (one-or-more not-newline))
               line-end))
     :modes qml-mode)
  (add-to-list 'flycheck-checkers 'qt-qmllint)

  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (global-flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . (lambda ()
                           (setq-local flycheck-checker 'rust-clippy)
                           (flycheck-rust-setup))))

(provide 'fate-flycheck)
;;; fate-flycheck.el ends here
