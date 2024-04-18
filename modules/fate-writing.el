;;; fate-writing.el --- org and markdown related settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  binbin

;; Author: binbin <binbin@BTSB25100GJU>
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

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . (lambda()
                           (setq-local prettier-js-args
                            '("--parser" "markdown")))))

(use-package markdown-toc
  :defer)


(defun fate/enable-jinx ()
  "Enable jinx only for writing modes."
  (unless (member major-mode '(yaml-ts-mode toml-ts-mode nxml-mode))
    (jinx-mode)))

(use-package jinx
  :hook ((markdown-mode text-mode) . fate/enable-jinx)
  :bind ([remap ispell-word] . jinx-correct))

(use-package org
  :ensure nil
  :hook (org-mode . (lambda() (set-fill-column 100))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package atomic-chrome
  :demand t
  :straight (atomic-chrome
             :repo "KarimAziev/atomic-chrome"
             :type git
             :flavor nil
             :host github)
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server))


(provide 'fate-writing)
;;; fate-writing.el ends here
