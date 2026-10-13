;;; fate-files.el --- Files, buffers, dired and projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Binbin Ye

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

;; Files, buffers, dired and projects

;;; Code:

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Do not create backup and auto save files
(setopt make-backup-files nil ; stop creating backup~ files
        auto-save-default nil ; stop creating #autosave# files
        create-lockfiles nil) ; stop creating .#interlock files

;; remember cursor position of visited files
(save-place-mode 1)

;; chmod +x on save for files starting with #!
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package tramp
  :defer t
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ;; open with the system default application
              ("C-<return>" . dired-do-open))
  :config
  ;; dired - reuse current buffer
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  ;; always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-movement-style 'cycle)
  (dired-listing-switches "-alh" "show human readable size")
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t "guess where to copy to")
  (dired-do-revert-buffer t "refresh after copy/rename")
  (dired-vc-rename-file t "rename with git mv")
  (dired-mark-region t "`m' marks every file in the region")
  (delete-by-moving-to-trash t))

(use-package wdired
  :ensure nil
  :defer t
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;; Core package Projectile
(use-package projectile
  :diminish
  :commands projectile-project-root
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :config
  (nconc projectile-other-file-alist
    '(
       ("graphql" . ("gql.ts"))
       ("ts" . ("spec.ts"))
       ("tsx" . ("spec.tsx" "less" "module.less" "scss" "module.scss" "css" "module.css" "sass" "module.sass"))))

  :custom
  (projectile-sort-order 'recentf)
  (projectile-switch-project-action 'projectile-vc)
  (projectile-auto-cleanup-known-projects t))

(use-package recentf
  :hook
  (after-init . recentf-mode)
  :commands (recentf-expand-file-name)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files
  (recentf-auto-cleanup 'never)
  :config
  (add-to-list 'recentf-exclude (recentf-expand-file-name fate-cache-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude (concat "\\`" (regexp-quote temporary-file-directory))))

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(defun fate/kill-buffer (prefix path-pattern)
  "Kill buffer with patterns.  PATH-PATTERN is pattern to compare with buffer name.
PREFIX determines whether to match file name or buffer name."
  (interactive "P\nsbuffer name pattern: ")
  (when path-pattern
    (cl-dolist (buffer (buffer-list))
      (let* ((file-name (cond
                          ((not prefix) (buffer-file-name buffer))
                          ((equal prefix 0) (buffer-name buffer)))))
        (message file-name)
        (when (and file-name
                   (string-match path-pattern file-name))
          (kill-buffer buffer))))))

(provide 'fate-files)
;;; fate-files.el ends here
