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

;; TODO add following packages
;; ace-jump-helm-line
;; auto-highlight-symbol
;; bookmark
;; helm
;; helm-ag
;; helm-descbinds
;; helm-flx
;; helm-make
;; helm-mode-manager
;; helm-projectile
;; helm-swoop
;; helm-themes
;; (helm-spacemacs-help :location local)
;; (helm-spacemacs-faq :location local)
;; helm-xref
;; imenu
;; persp-mode
;; popwin
;; projectile;; 

;;; Code:

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x f" . helm-recentf)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)
   :map helm-map
   ("TAB" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)))

(use-package helm-swoop
  :defer t
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-speed-or-color t
          helm-swoop-split-window-function 'helm-default-display-buffer)))

(provide 'fate-helm)
;;; fate-helm.el ends here
