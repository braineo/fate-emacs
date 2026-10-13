;;; fate-auto-complete.el ---                        -*- lexical-binding: t; -*-

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

(use-package yasnippet
  :commands (yas-reload-all)
  :config (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(keymap-global-set "M-/" #'hippie-expand)

(setopt hippie-expand-try-functions-list
      '(
        ;; Try to expand word "dynamically", searching the current buffer.
        try-expand-dabbrev
        ;; Try to expand word "dynamically", searching all other buffers.
        try-expand-dabbrev-all-buffers
        ;; Try to expand word "dynamically", searching the kill ring.
        try-expand-dabbrev-from-kill
        ;; Try to complete text as a file name, as many characters as unique.
        try-complete-file-name-partially
        ;; Try to complete text as a file name.
        try-complete-file-name
        ;; Try to expand word before point according to all abbrev tables.
        try-expand-all-abbrevs
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-list
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-line
        ;; Try to complete as an Emacs Lisp symbol, as many characters as
        ;; unique.
        try-complete-lisp-symbol-partially
        ;; Try to complete word as an Emacs Lisp symbol.
        try-complete-lisp-symbol))

;; From https://code.tecosaur.net/tec/emacs-config/commit/1e6e64991e via karthink
;; Stop hippie-expand duplicating text after point, e.g. `foo' in `foo)'.
(defun fate/he-subst-suffix-overlap (ins rem)
  "The longest suffix of the string INS that is a prefix of REM.
This is intended to be used when INS is a newly inserted string and REM is the
remainder of the line, to allow for handling potentially duplicated content."
  (let ((len (min (length ins) (length rem))))
    (while (and (> len 0)
                (not (eq t (compare-strings ins (- len) nil rem 0 len))))
      (setq len (1- len)))
    len))

(with-eval-after-load 'hippie-exp
  (define-advice he-substitute-string (:filter-args (args) suffix-strip)
    "Filter ARG list for `he-substitute-string', truncating duplicated suffix.
ARGS is the raw argument list (STRING &optional TRANS-CASE)."
    (pcase-let* ((`(,ins &optional ,trans-case) args)
                 (rem (save-excursion
                        (goto-char (marker-position he-string-end))
                        (buffer-substring-no-properties
                         (point) (line-end-position))))
                 (ov (fate/he-subst-suffix-overlap ins rem)))
      (when (>= ov 0)
        (setq ins (substring ins 0 (- (length ins) ov))))
      (list ins trans-case))))

(provide 'fate-auto-complete)
;;; fate-auto-complete.el ends here
