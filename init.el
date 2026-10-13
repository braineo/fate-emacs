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

;; `load-prefer-newer' and GC tuning live in early-init.el.
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))

(require 'fate-const)
(require 'fate-core)
(require 'fate-git)
(require 'fate-delta)
(require 'fate-vertico)
(require 'fate-langs)
(require 'fate-python)
(require 'fate-web)
(require 'fate-writing)
(require 'fate-flycheck)
(require 'fate-lsp)
(require 'fate-flycheck-bridge)
(require 'fate-assist)
(require 'fate-transient)
(require 'fate-docker)
(require 'fate-worldtime)

(when IS-MAC
  (require 'fate-macos))

(provide 'init)
;;; init.el ends here
