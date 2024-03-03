;;; fate-flycheck-bridge.el --- integration between flycheck and lsp-bridge  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Binbin Ye

;; Author: Binbin Ye
;; Keywords: lsp

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


(require 'flycheck)
(require 'lsp-bridge)


(defun flycheck-lsp-bridge-parse-error (diagnostic checker)
  "Parse a error DIAGNOSTIC from CHECKER into a `flycheck-error'.
Return the corresponding `flycheck-error'."
  (let* ((diagnostic-start (plist-get (plist-get diagnostic :range) :start))
         (diagnostic-end (plist-get (plist-get diagnostic :range) :end))
         (message (plist-get diagnostic :message))
         (severity (plist-get diagnostic :severity)))
    (flycheck-error-new-at
      (+ 1 (or (plist-get diagnostic-start :line) 0))
      (+ 1 (or (plist-get diagnostic-start :character) 0))
      (pcase severity
        (1 'error)
        (2 'warn)
        (3 'info)
        (4 'info)
        (_ 'error))
      message
      :end-line (+ 1 (or (plist-get diagnostic-end :line) 0))
      :end-column (+ 1 (or (plist-get diagnostic-end :character) 0))
      :checker checker)))

(defun flycheck-lsp-bridge-start (checker callback)
  "Query lsp bridge diagosis with CHECKER.
CALLBACK is the status callback passed by flycheck."
  (let* ((errors (delq nil
                   (mapcar (lambda (diagnostic)
                             (flycheck-lsp-bridge-parse-error diagnostic checker))
                     lsp-bridge-diagnostic-records))))
    (funcall callback 'finished errors)))


(defun flycheck-lsp-bridge-report ()
  "Report once on receiving update from lsp-bridge."
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(flycheck-define-generic-checker 'lsp-bridge
  "Integration between lsp bridge and flycheck."
  :start #'flycheck-lsp-bridge-start
  :modes '(lsp-placeholder-mode)  ;; must be major mode. add a place holder here
  :predicate (lambda() lsp-bridge-mode))

(defvar flycheck-lsp-bridge-enabled-p nil "Is integration enabled in current buffer.")

(defun flycheck-lsp-bridge-enable ()
  "Enable flycheck lsp-bridge checker for current major mode."
  (unless flycheck-lsp-bridge-enabled-p
    (setq-local flycheck-lsp-bridge-enabled-p t)
    (setq-local lsp-bridge-diagnostic-enable-overlays nil)
    (flycheck-stop)

    (unless (flycheck-checker-supports-major-mode-p 'lsp major-mode)
      (flycheck-add-mode 'lsp-bridge major-mode))

    ;; report once for initial update
    (flycheck-lsp-bridge-report)

    (add-hook 'lsp-bridge-diagnostic-update-hook #'flycheck-lsp-bridge-report nil t)
    (remove-hook 'lsp-bridge-diagnostic-update-hook #'flycheck-lsp-bridge-enable t)))

;;;###autoload
(defun flycheck-lsp-bridge-setup ()
  "Setup flycheck lsp bridge."
  (interactive)
  (add-to-list 'flycheck-checkers 'lsp-bridge)
  (add-hook 'lsp-bridge-diagnostic-update-hook #'flycheck-lsp-bridge-enable t))


(provide 'fate-flycheck-bridge)
;;; fate-flycheck-bridge.el ends here
