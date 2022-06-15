;;; fate-json-mode.el --- major mode for json using tree sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Binbin Ye

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
(require 'reformatter)
(require 'tree-sitter)
(require 'tree-sitter-hl)

(defvar-local jq-foramt-args '())

;;;###autoload
(define-derived-mode fate-json-mode prog-mode "JSON"
  ;; It's up to the major mode to set this. It plays a role similar to that of
  ;; `font-lock-defaults'.
  (setq tree-sitter-hl-default-patterns
        [(pair key: (_) @keyword)
         (number) @number
         (string) @string
         [(true) (false) (null)] @variable.builtin
         (escape_sequence) @escape])
  (tree-sitter-hl-mode))


(defun fate/json-get-path (current-node output)
  "Get path to json value at cursor position.  CURRENT-NODE is a tree-sitter-node.
OUTPUT is parsed path list."
  (let* ((parent-node (tsc-get-parent current-node)))
    (if parent-node
      (progn
        (when (eq (tsc-node-type parent-node) 'array)
          (let ((index -1)
                (cursor (tsc-make-cursor parent-node)))
            (tsc-goto-first-child cursor)
            (while (not (tsc-node-eq current-node (tsc-current-node cursor)))
                (progn
                  (tsc-goto-next-sibling cursor)
                  (if (tsc-node-named-p (tsc-current-node cursor))
                    (progn
                      (setq index (+ index 1))))))
            (setq output (push index output))))
        (when (eq (tsc-node-type current-node) 'pair)
            (setq output (push (tsc-node-text (tsc-get-nth-child current-node 0)) output)))
        (fate/json-get-path parent-node output))
      output)))

;;;###autoload
(defun fate/json-print-path-js ()
  "Show json path in minibuffer in JavaScript, jq format."
  (interactive)
  (let (json-path)
    (dolist (elt (fate/json-get-path (tree-sitter-node-at-pos) '()) json-path)
      (when (stringp elt)
        (let* ((trimmed-elt (string-trim elt "\"" "\"")))
          (if (string-match-p "-" trimmed-elt)
            (setq json-path (concat json-path "[" trimmed-elt "]"))
            (setq json-path (concat json-path "." trimmed-elt)))))
      (when (numberp elt)
        (setq json-path (concat json-path "[" (number-to-string elt) "]"))))
    (message json-path)
    (kill-new json-path)))

(define-key fate-json-mode-map (kbd "C-c P") 'fate/json-print-path-js)

;;;###autoload
(defun fate/json-kill-path-js ()
  "Save json path to kill ring."
  (interactive)
  (kill-new (fate/json-print-path-js)))

(define-key fate-json-mode-map (kbd "C-c C-p") 'fate/json-kill-path-js)

;;;###autoload
(defun fate/json-pretty-print (&optional minimize)
  "Pretty-print current buffer.  when MINIMIZE is set, minimize JSON document."
  (interactive "P")
  (if minimize
    (setq-local jq-foramt-args '("-c")))
  (when (use-region-p)
    (cond ((executable-find "jq") (json-jq-region (region-beginning) (region-end) nil))
          (t (json-pretty-print (region-beginning) (region-end) minimize))))
  (cond ((executable-find "jq") (json-jq-buffer nil))
        (t (json-pretty-print (point-min) (point-max) minimize)))

  (setq-local jq-foramt-args '()))

(define-key fate-json-mode-map (kbd "C-c C-l") 'fate/json-pretty-print)

(reformatter-define json-jq
  :program (executable-find "jq")
  :args jq-foramt-args
  :lighter " jq")

;;;###autoload
(progn
  ;; Register the association with `tree-sitter-mode'.
  (add-to-list 'tree-sitter-major-mode-language-alist '(fate-json-mode . json))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . fate-json-mode)))

(provide 'fate-json-mode)
;;; fate-json-mode.el ends here
