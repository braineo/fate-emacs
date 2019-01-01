;;; core-editor.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

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

(eval-when-compile
  (require 'core-load-paths))

(use-package hydra)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Do not create backup and auto save files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Core package anzu better search and replace
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package dired
  :ensure nil
  :config
  ;; dired - reuse current buffer
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  :bind
  (:map dired-mode-map
   ("RET" . dired-find-alternate-file)))

;; Core package Projectile
(use-package projectile
  :diminish
  :bind
  (:map projectile-mode-map
        ("C-," . projectile-find-file)
        ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
        projectile-cache-file (concat fate-cache-directory "projectile.cache")
        projectile-known-projects-file (concat fate-cache-directory
                                               "projectile-bookmarks.eld")))
(use-package recentf
  :hook
  (after-init . recentf-mode)
  :config
  (progn
    (setq recentf-save-file (concat fate-cache-directory "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude (recentf-expand-file-name fate-cache-directory))))

;; Core package smartparens
(use-package smartparens
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :init
  (progn
    ;; settings
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil))
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))

;; Core package move to beginning of code first
(use-package mwim
  :defer t
  :bind
  (([remap  move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Core package easy kill. easy to copy the buffer name/path
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill))

;; Core package expand-region. Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Core package hideshow. Code folding tool
(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-<tab>" . hs-toggle-hiding))

;; Core package winum. Easy navigation to different buffers
(use-package winum
  :hook (after-init . winum-mode)
  :bind
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8))

;; Avy. Jump to anywhere like a sniper
(use-package avy
  :ensure t
  :bind
  ("C-c j" . avy-goto-word-or-subword-1)
  :config
  (setq avy-background t))

;; Ace window. specify which window to jump to
(use-package ace-window
  :bind
  ("s-w" . ace-window)
  ([remap other-window] . ace-window))

(use-package move-text
  :bind
  ("M-S-<up>" . move-text-up)
  ("M-S-<down>" . move-text-down))

(use-package multiple-cursors
  :bind
  ("C->"   . mc/mark-next-like-this)
  ("C-<"   . mc/mark-previous-like-this)
  ("C-M->" . mc/skip-to-next-like-this)
  ("C-M-<" . mc/skip-to-previous-like-this))

(use-package crux
  :bind
  (("C-c o" . crux-open-with)
   ("M-o" . crux-smart-open-line)
   ("s-o" . crux-smart-open-line-above)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c I" . crux-find-user-init-file)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-top-join-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(require 'fate-auto-complete)
(require 'fate-align-text)
(provide 'core-editor)
;;; core-editor.el ends here
