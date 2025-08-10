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

(defvar fate/docstring-language-configs
  '((rust
     :modes (rust-mode rust-ts-mode)
     :prompt-template "Generate comprehensive documentation for the following function. Use /// style comments that is compatible with rustdoc, with proper sections like Arguments, Returns, Panics, Errors and Examples where applicable:\n\n%s")
    (typescript
     :modes (typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode jtsx-tsx-mode jtsx-typescript-mode)
     :prompt-template "Generate comprehensive JSDoc documentation for the following TypeScript function. Include @param, @returns, and @throws sections where appropriate:\n\n%s")
    (go
     :modes (go-mode go-ts-mode)
     :prompt-template "Generate Go documentation comments for the following function. Follow Go documentation conventions with a comment starting with the function name:\n\n%s")
    (python
     :modes (python-mode python-ts-mode)
     :prompt-template "Generate a comprehensive Python docstring for the following function. Use Google style with Args, Returns, and Raises sections. Do not output function signature, only the comments:\n\n%s")
    (emacs-lisp
     :modes (emacs-lisp-mode lisp-interaction-mode)
     :prompt-template "Generate detailed Emacs Lisp documentation for the following function. Include parameter descriptions, and return value:\n\n%s")
    (c++
     :modes (c++-mode c++-ts-mode)
     :prompt-template "Generate comprehensive Doxygen-style documentation for the following C++ function. Include @brief, @param, @return, and @throws sections:\n\n%s"))
  "Configuration for different programming languages and their docstring formats.")


(defun fate/get-language-config ()
  "Get the docstring configuration for the current major mode."
  (cl-find-if (lambda (config)
                (memq major-mode (plist-get (cdr config) :modes)))
              fate/docstring-language-configs))

(defun fate/extract-function-context ()
  "Extract the function definition around the cursor position using
beginning-of-defun and end-of-defun."
  (let* ((config-entry (fate/get-language-config))
         (config (cdr config-entry)))
    (unless config
      (error "No configuration found for %s" major-mode))

    (save-excursion
      (condition-case nil
          (progn
            (beginning-of-defun)
            (let ((func-start (point))
                  (func-end))
              ;; Get the end of the function
              (end-of-defun)
              (setq func-end (point))

              (list :start func-start
                    :end func-end
                    :text (buffer-substring-no-properties func-start func-end))))
        (error nil)))))


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
  :if (derived-mode-p 'prog-mode)
  (interactive)
  (fate/gptel-generate-and-insert-docstring))

(defun fate/gptel-generate-and-insert-docstring ()

  (let* ((config-entry (fate/get-language-config))
         (config (cdr config-entry)))
    (unless config
      (user-error "Docstring generation not supported for %s" major-mode))

    (let* ((func-context (fate/extract-function-context)))
      (unless func-context
        (user-error "No function found around cursor position"))

      (let* ((func-text (plist-get func-context :text))
             (template (plist-get config :prompt-template))
             (prompt (format template func-text)))

        (message "Generating docstring...")
        (gptel-request
          prompt
          :system "You are a helpful programming assistant that generates clear, comprehensive documentation for functions. Return only the documentation content without any additional text or code or formatting."
          :callback
          (lambda (response info)
            (cond
              ((not response) (message "Failed to geneerate docstring: %s" (plist-get info :status)))
              ((listp response) nil) ;; reasoning response, ignore
              (t (fate/format-and-insert-docstring response func-context)))))))))


(defun fate/format-and-insert-docstring (input func-context)
 (let* ((trimmed (string-trim input))
        (cleaned (replace-regexp-in-string "\\(^```[^\n]*\n?\\|```$\\)" "" trimmed))
        (func-start (plist-get func-context :start))
        (func-end (plist-get func-context :end))
        (insert-point nil))
   (save-excursion
     (goto-char func-start)
     (cond
       ((memq major-mode '(python-mode python-ts-mode))
        (when (re-search-forward ":" func-end t)
          (forward-line)))
       ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
        (down-list 2)
        (backward-char)
        (forward-sexp)
        (forward-line)))
     (back-to-indentation)
     (setq insert-point (point))
     (let* ((indent (make-string (current-column) ?\s))
            (indented
              (mapconcat (lambda (line)
                           (concat indent line))
                (split-string cleaned "\n")
                "\n")))
       (insert indented)
       (newline)
       (delete-trailing-whitespace insert-point (point))))))

(use-package gptel
  :commands (gptel-request)
  :config
  (fate/gptel-backend-setup)
  (with-eval-after-load 'gptel-transient
    (transient-insert-suffix 'gptel-menu '(2 -1)
      ["Document" (fate/gptel-suffix-docstring)]))
  :custom
  (gptel-include-reasoning nil "do not include thinking in the response"))


(use-package inline-diff
  :vc (:url "https://code.tecosaur.net/tec/inline-diff")
  :after gptel-rewrite)


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
