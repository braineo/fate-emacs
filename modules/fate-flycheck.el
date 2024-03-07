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
  :commands (flycheck-add-mode)
  :config
  (flycheck-add-mode 'json-jq 'fate-json-mode)

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

(use-package flycheck-posframe
  :commands (flycheck-posframe--pad-icon)
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (defun flycheck-posframe--pad-icon (icon)
    (concat icon " "))
  (with-eval-after-load 'nerd-icons
    (setq
      flycheck-posframe-info-prefix (flycheck-posframe--pad-icon (nerd-icons-codicon "nf-cod-info"))
      flycheck-posframe-warning-prefix (flycheck-posframe--pad-icon (nerd-icons-codicon "nf-cod-warning"))
      flycheck-posframe-error-prefix (flycheck-posframe--pad-icon (nerd-icons-codicon "nf-cod-error"))))
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'success :foreground (face-attribute 'success :foreground))
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'fate-flycheck)
;;; fate-flycheck.el ends here
