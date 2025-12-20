;;; fate-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye
;; Keywords: python

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

(require 'hideshow)
(eval-when-compile
  (require 'fate-misc))

(defun fate/python-hideshow-forward-sexp-function (arg)
  "Python specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  arg  ; Shut up, byte compiler.
  (python-nav-end-of-block))

(defun fate/python-setup-hs-mode ()
  "Replace `hs-special-modes-alist' for `python-mode'."
  (let
    ((python-mode-hs-info
       '(python-mode
          "\\s-*\\_<\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\|with\\)\\_>" "" "#"
          fate/python-hideshow-forward-sexp-function
          nil)))
    (setq hs-special-modes-alist (cl-remove-if #'(lambda (x) (eq (car x) 'python-mode)) hs-special-modes-alist))
    (add-to-list 'hs-special-modes-alist python-mode-hs-info)
    (hs-grok-mode-type)))


(defun fate/ruff-make-args ()
  "Construct args with BEG and END position for ruff."
  (append
   '("format")
   (when (use-region-p)
       `("--range"
         ,(format "%d-%d"
                  (1- (line-number-at-pos (region-beginning)))
                  (1+ (line-number-at-pos (region-end))))))
   `(,(buffer-file-name))))

(defun fate/ruff-fmt ()
  "Format buffer using ruff."
  (interactive)
  (unless (executable-find "ruff")
    (error "Cannot find ruff executable"))
  (apply #'call-process "ruff" nil (get-buffer-create "*ruff-output*") nil (fate/ruff-make-args)))

(use-package python
  :defines gud-pdb-command-name pdb-path
  :functions python-nav-end-of-block
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (defconst fate/python-tools
    '("ruff-lsp"
      "ruff"
      "basedpyright"
      ;; lsp bridge dependencies
      "epc" "orjson" "sexpdata" "six" "setuptools" "paramiko" "rapidfuzz")

   "python cli tools")

  (with-eval-after-load 'core-packages
    (fate/create-install-tools!
      "python" ("pip3" "install" "--upgrade") fate/python-tools))

  :bind
  (:map python-mode-map
   ("C-c C-l" . fate/ruff-fmt)
   :map python-ts-mode-map
   ("C-c C-l" . fate/ruff-fmt))
  :hook
  (python-mode . fate/python-setup-hs-mode))


(provide 'fate-python)
;;; fate-python.el ends here
