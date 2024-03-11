;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Binbin Ye

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

;; Speed up initialization. Suggestions taken from Doom emacs configs.
(setq gc-cons-threshold most-positive-fixnum ;; 2^61 bytes
  gc-cons-percentage 0.6)

(defvar custom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (progn
      (setq gc-cons-threshold 16777216 ;; 16mb
        gc-cons-percentage 0.1)
      (setq file-name-handler-alist custom--file-name-handler-alist))))

(setq package-enable-at-startup nil) ;; don't auto-initialize!

(when (fboundp 'startup-redirect-eln-cache)
  (let ((eln-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory))))
   (if (version< emacs-version "29")
     (add-to-list 'native-comp-eln-load-path eln-path)
     (startup-redirect-eln-cache eln-path))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
;; Always load newest byte code
(setq load-prefer-newer t)

(setq initial-scratch-message "")

(provide 'early-init)
;;; early-init.el ends here
