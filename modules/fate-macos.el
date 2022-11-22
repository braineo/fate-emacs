;;; fate-macos.el ---                                -*- lexical-binding: t; -*-

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

;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
;; However it is too expensive, maybe not a good idea to enable it
;; (use-package exec-path-from-shell
;;   :config
;;   (progn
;;     (exec-path-from-shell-initialize)
;;     (exec-path-from-shell-copy-env "PYTHONPATH")))

;; Swap command and option
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; There's no point in hiding the menu bar on macOS, so let's not do it
(menu-bar-mode +1)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'fate-macos)
;;; fate-macos.el ends here
