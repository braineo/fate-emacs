;;; core-load-paths.el ---                           -*- lexical-binding: t; -*-

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

(defun add-to-load-path (dirs)
  "Add a list of directories `DIRS' to 'load-path."
  (add-to-list 'load-path dirs))

(defvar fate-directory
  (file-truename user-emacs-directory)
  "Root directory of Fate Emacs.")

(defconst fate-core-directory
  (expand-file-name "core" fate-directory)
  "Fate core directory.")

(defconst fate-modules-directory
  (expand-file-name "modules" fate-directory)
  "Fate modules directory.")

(defconst fate-cache-directory
  (expand-file-name (concat fate-directory ".cache/"))
  "Fate cache directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(unless (file-exists-p fate-cache-directory)
    (make-directory fate-cache-directory))

;; Custom file
(setq custom-file (expand-file-name "custom.el" fate-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; load paths
(mapc 'add-to-load-path
      `(
        ,fate-core-directory
        ,fate-modules-directory))

(provide 'core-load-paths)
;;; core-load-paths.el ends here
