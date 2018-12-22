;;; init.el ---                                      -*- lexical-binding: t; -*-

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

;;

;;; Code:

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
(require 'fate-company)
(require 'fate-python)
(require 'fate-lisp)
(require 'fate-langs)
(require 'fate-flycheck)
(require 'fate-lsp)

(when IS-MAC
  (require 'fate-macos))

(provide 'init)
;;; init.el ends here
