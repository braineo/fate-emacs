;;; fate-taskjuggler.el --- taskjuggler              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  binbin

;; Author: binbin
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


(use-package taskjuggler-mode
  :straight (:host gitlab
              :repo "bricka/emacs-taskjuggler-mode")
  :config
  (add-to-list 'tree-sitter-load-path (straight--repos-dir "tree-sitter-taskjuggler"))
  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-indentation-alist '(taskjuggler-mode taskjuggler-indent-offset tree-sitter-indent-offset)))
  (with-eval-after-load 'flycheck
    (flycheck-define-checker taskjuggler
      "A syntax checker for TaskJuggler files."
      :command ("tj3" "--check-syntax" source)
      :modes taskjuggler-mode
      :error-patterns
      ((error line-start (file-name) ":" line ": " "Error: " (message))))
    (add-to-list 'flycheck-checkers 'taskjuggler)))

(use-package tree-sitter-taskjuggler
  :defer t
  :straight (tree-sitter-taskjuggler
              :host gitlab
              :repo "braineo/tree-sitter-taskjuggler"
              :branch "block-comment"
              :pre-build (("npm" "ci") ("make"))
              :build (:not compile)
              :files ("taskjuggler.so")))


(provide 'fate-taskjuggler)
;;; fate-taskjuggler.el ends here
