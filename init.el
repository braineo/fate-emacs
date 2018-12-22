;; Always load newest byte code
(setq load-prefer-newer t)

(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))

;; Custom file
(setq custom-file (expand-file-name "custom.el" fate-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'fate-core)
(require 'fate-git)
(require 'fate-helm)
(require 'fate-python)
(require 'fate-lisp)
(require 'fate-langs)
(require 'fate-flycheck)
(require 'fate-lsp)

(when IS-MAC
  (require 'fate-macos))
