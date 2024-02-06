;;; fate-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

;; Author: Binbin Ye <braineo@Binbins-iMac.local>
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


(defun fate/pydocstring ()
  "Insert docstring for function at point."
  (interactive)
  (let* ((indent (save-excursion
                   (beginning-of-line)
                   (back-to-indentation)
                   (current-indentation)))
         (current-buffer (current-buffer))
         (file-path (buffer-file-name))
         (line-number (line-number-at-pos)))
    (with-temp-buffer
      (insert (shell-command-to-string
                (format "pydocstring -f google %s '(%d,)'"
                  file-path
                  (+ 1 line-number))))
      (goto-char (point-min))
      (kill-line)
      (kill-line)
      (set-mark (point-min))
      (goto-char (point-max))
      (indent-rigidly (region-beginning) (region-end) (+ indent 4))
      (let* ((docstring (buffer-substring-no-properties (point-min) (point-max))))
        (with-current-buffer current-buffer
          (save-excursion
             (forward-line)
             (beginning-of-line)
             (insert docstring)))))))

(use-package python
  :defines gud-pdb-command-name pdb-path
  :functions python-nav-end-of-block
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (defconst fate/python-tools
   '("ruff"
     "pydocstring")
   "python cli tools")
  (with-eval-after-load 'core-packages
    (fate/create-install-tools
      "python" ("pip3" "install" "--upgrade") fate/python-tools))
  :bind
  (:map python-mode-map
    ("C-c M-d" . fate/pydocstring))
  :hook
  (python-mode . fate/python-setup-hs-mode))

;; Install:
;; pip install black
;; pip install black-macchiato
(use-package python-black
  :demand t
  :after python
  :custom
  (python-black-extra-args '("--line-length=120" "--skip-string-normalization"))
  :bind
  (:map python-mode-map
    ("C-c C-l" . python-black-partial-dwim)))

(provide 'fate-python)
;;; fate-python.el ends here
