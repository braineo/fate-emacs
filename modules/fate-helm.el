;;; fate-helm.el ---                                 -*- lexical-binding: t; -*-

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

;;; Code:

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x f" . helm-recentf)
   ("s-r". helm-recentf)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)
   :map helm-map
   ("TAB" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :custom
  (helm-prevent-escaping-from-minibuffer t)
  (helm-bookmark-show-location t)
  (helm-display-header-line nil)
  (helm-split-window-inside-p t)
  (helm-always-two-windows t)
  (helm-echo-input-in-header-line nil)
  (helm-autoresize-min-height 10)
  (helm-autoresize-max-height 25)
  (helm-buffer-max-length 40 "make helm buffer wider to display full file names")
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-show-completion-display-function #'helm-display-buffer-in-own-frame)
  :config
  (progn
    (helm-autoresize-mode t))
  :hook (after-init . helm-mode))

(use-package helm-projectile
  :after (helm projectile)
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile-ag
             helm-projectile-rg
             helm-projectile
             helm-projectile-switch-project
             fate-helm-rg)
  :init
  (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on)
  :config
  (defun fate-helm-rg ()
    "Run projectile-rg if in a project, otherwise rg current directory."
    (interactive)
    (if (projectile-project-p)
      (helm-projectile-rg)
      (helm-rg nil)))
  :bind
  (("C-." . fate-helm-rg)))

(use-package helm-rg
  :after (helm)
  :config
  (defun fate/helm-to-color-rg ()
    (interactive)
    (message helm-rg--glob-string)
    (helm-run-after-exit
     (lambda ()
       (color-rg-search-input helm-pattern (color-rg-project-root-dir)))))
  :bind
  (:map helm-rg-map
    ("M-b" . nil)
    ("M-d" . nil)
    ("M-i" . fate/helm-to-color-rg)
    ("M-o" . helm-rg--set-dir))
  :custom-face
  (helm-rg-preview-line-highlight ((t (:inherit highlight :distant-foreground "black")))))

;; Better isearch behavior
(use-package swiper
  :defer t
  :custom
  (swiper-action-recenter t)
  :bind (:map swiper-map
         ("M-%" . swiper-query-replace)
         :map isearch-mode-map
         ("M-i" . swiper-from-isearch)
         ([remap isearch-delete-char] . isearch-del-char)))

(provide 'fate-helm)
;;; fate-helm.el ends here
