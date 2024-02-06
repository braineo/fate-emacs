;;; fate-transient.el --- Transient entries                  -*- lexical-binding: t; -*-

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

(require 'transient)

(transient-define-prefix gate-of-babylon ()
  "Gate of Babylon."
  [["Text"
    ("x" "Align" align-text-transient)
    ("c" "Case" string-inflection-transient)]

   [""
    ("h" "Highlight" symbol-overlay-transient)
    ("l" "Language Server" lsp-transient)]

   ["Assistant"
     ("s" "Search Engine" engine-transient)
     ("a" "GPT" gptel-menu)]])

(global-set-key (kbd "M-m") 'gate-of-babylon)

(provide 'fate-transient)
;;; fate-transient.el ends here
