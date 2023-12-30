;;; fate-completion.el --- completion related        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Binbin Ye

;; Author: Binbin Ye
;; Keywords: completion, vertico

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


(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind (

         ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)

         ([remap Info-search]        . consult-info)
         ([remap imenu]              . consult-imenu)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#"   . consult-register-load)
         ("M-'"   . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)

         ;; Other custom bindings
         ("s-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("C-." . fate/consult-rg)

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)

         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)       ;; orig . isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig . isearch-edit-string
         ("M-i"   . consult-line)
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (defun fate/consult-rg (&optional dir given-initial)
    (interactive "P")
    (let ((initial (or given-initial
                       (when (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end)))
                       (thing-at-point 'symbol))))
      (consult-ripgrep dir initial))))

(use-package consult-projectile
  :after consult
  :bind
  (([remap projectile-find-file] . consult-projectile-find-file)
   ([remap projectile-switch-project] . consult-projectile-switch-project)))


(use-package consult-flycheck
  :bind
  ("M-g f" . consult-flycheck))

(provide 'fate-completion)
;;; fate-completion.el ends here
