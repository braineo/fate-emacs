;;; fate-assist.el --- find references on internet   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  binbin

;; Author: binbin
;; Keywords: search, gpt, llm

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

(use-package engine-mode
  :bind-keymap ("C-c s" . engine-mode-prefixed-map)
  :config
  (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (with-eval-after-load 'transient
    (transient-define-prefix engine-transient ()
      "Search Engine"
      ["Search Engine"
        ("g" "Google" engine/search-google)
        ("h" "GitHub" engine/search-github)]))
  (engine-mode t))



(use-package gptel
  :config
  (defun fate/gptel-setup ()
    (let* ((ollama-models (seq-filter #'(lambda (line)(length> line 0))
                            (mapcar #'(lambda (line) (car (split-string line "\t")))
                             (cdr (split-string (shell-command-to-string "ollama ls 2>/dev/null") "\n")))))

           (llama-cpp (gptel-make-openai "llama-cpp"
                         :stream t
                         :protocol "http"
                         :host "localhost:8080"
                         :models '("any"))))
      (if ollama-models
        (setq-default
          gptel-backend (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :stream t
                           :models ollama-models)
          gptel-model (car ollama-models))
        (setq-default
          gptel-backend llama-cpp
          gptel-model "any"))))
  (fate/gptel-setup))
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap Info-goto-emacs-command-node] . helpful-function))

(provide 'fate-assist)
;;; fate-assist.el ends here
