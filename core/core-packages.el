;;; core-packages.el --- core package setup          -*- lexical-binding: t; -*-

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

;; Package configs
;; package.el is activated before init.el (`package-enable-at-startup'),
;; and use-package is built in, so no bootstrapping is needed.
(require 'core-load-paths)

(setopt package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))
        use-package-always-ensure t
        use-package-vc-prefer-newest t
        package-install-upgrade-built-in t)
(when (file-readable-p custom-file) (load custom-file))

;; Only enable for benchmarking
(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun fate/update-configuration ()
  "Update packages and configure repository."
  (when (executable-find "git")
    (message "Pulling configuration update from git")
    (let ((default-directory fate-directory))
      (call-process "git" nil nil t "pull")))
  (message "Upgrading packages")
  (package-upgrade-all))


(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))))

(setopt native-comp-async-report-warnings-errors 'silent)

(provide 'core-packages)
;;; core-packages.el ends here
