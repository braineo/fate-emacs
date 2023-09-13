;;; fate-langs.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

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

;; collection of simple language package settings

;;; Code:


;; Lisp

(defun fate-init-parinfer-mode ()
  "Disable some minor modes having troubles with parinfer-rus-mode."
  (progn
    (electric-pair-local-mode -1)
    (parinfer-rust-mode)))

(use-package parinfer-rust-mode
  :if (string-match-p "MODULES" system-configuration-features)
  :hook
  ((emacs-lisp-mode scheme-mode) . fate-init-parinfer-mode)
  :init
  (setq parinfer-rust-auto-download t)
  :custom
  (parinfer-rust-library-directory (concat fate-cache-directory "parinfer-rust/")))


(use-package treesit
  :ensure nil
  :defer t
  :custom
  (treesit-font-lock-level 4))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . (lambda()
                           (setq-local prettier-js-args
                            '("--parser" "markdown")))))

(use-package jinx
  :hook ((markdown-mode text-mode) . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package swift-mode
  :mode (("\\.swift\\'" . swift-mode)))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode)))

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :custom (rust-format-on-save t))

(use-package cargo-mode
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; QML
(reformatter-define qmlformat
  :program "qmlformat"
  :stdin nil
  :args (list input-file "--no-sort"))

(use-package qml-mode
  :mode (("\\.qml\\'" . qml-mode))
  :bind (:map qml-mode-map
         ("C-c C-l" . qmlformat-buffer)))

;; i18n
(use-package po-mode
  :mode (("\\.po\\'" . po-mode)))

(use-package conf-mode
  :ensure nil
  :mode (("/\\.env" . conf-mode)))

;; shaders
(use-package glsl-mode
  :mode (("/\\.glsl" . glsl-mode)))


(provide 'fate-langs)
;;; fate-langs.el ends here
