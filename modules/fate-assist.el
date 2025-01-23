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

(require 'transient)

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


(defconst fate/docstring-prompt-template "You are going to write docstring comment.

Function definition:
```
%s
```

- Now write %s docstring for this function.
- Return the inline docstring only.
- Do not output code or explanation.
- Do not output markdown codeblock.
- Add linebreak at the end.
- Response concisely.
")

(defun fate/gptel-backend-setup ()
  "Setup additional model backend for gptel."
  (let* ((ollama-models (mapcar #'intern  ;; Convert strings to symbols
                          (seq-filter #'(lambda (line)(length> line 0))
                            (mapcar #'(lambda (line) (string-trim (car (split-string line))))
                              (cdr (split-string (shell-command-to-string "ollama ls 2>/dev/null") "\n" t))))))
         (llama-cpp (gptel-make-openai "llama-cpp"
                       :stream t
                       :protocol "http"
                       :host "localhost:8080"
                       :models '(any))))
    (if ollama-models
      (setq-default
        gptel-backend (gptel-make-ollama "Ollama"
                         :host "localhost:11434"
                         :stream t
                         :models ollama-models)
        gptel-model (car ollama-models))
      (setq-default
        gptel-backend llama-cpp
        gptel-model 'any))))

(transient-define-suffix fate/gptel-suffix-docstring ()
  "Generate docstring for region contents."
  :key "D"
  :description "Docstring"
  :if (and (derived-mode-p 'prog-mode) (use-region-p))
  (interactive)
  (let* ((lang (if (listp mode-name) (car mode-name) mode-name))
         (prompt (format fate/docstring-prompt-template
                        (buffer-substring-no-properties
                          (region-beginning) (region-end))
                        lang)))
    (gptel-request
      prompt
      :stream t
      :position (region-beginning)
      :system (format "You are an expert %s programmer writing docstring for a function." lang))))


(use-package gptel
  :commands (gptel-request)
  :config
  (fate/gptel-backend-setup)
  (with-eval-after-load 'gptel-transient
    (transient-insert-suffix 'gptel-menu '(2 -1)
      ["Document" :if use-region-p (fate/gptel-suffix-docstring)])))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap Info-goto-emacs-command-node] . helpful-function))

(use-package elisp-demos
  :after helpful
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))



(use-package ace-link
  :init
  (ace-link-setup-default))

(provide 'fate-assist)
;;; fate-assist.el ends here
