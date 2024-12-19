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
(require 'package)
(require 'core-load-paths)
(require 'comp)

(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized
  (package-initialize))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-vc-prefer-newest t)

(when (file-readable-p custom-file) (load custom-file))

;; Only enable for benchmarking
(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun fate/update-configuration ()
  "Update packages and configure repository."
  (if (executable-find "git")
    (progn
      (print "Pulling configuration update from git")
      (call-process-shell-command "cd ~/.emacs.d && git pull" nil nil t)))
  (print "Pulling configuration update from git")
  (package-upgrade-all))


(use-package no-littering)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

(defmacro fate/create-install-tools! (name command tools &optional tool-name-fun)
  "Install necessary tools for a given executable.
`NAME' is function and buffer name it generates.
`COMMAND' is the full command without package name
`TOOLS' is the list of packages to install.
`TOOL-NAME-FUN' is for tranforming package name"
  (let* ((executable (car command)))

    `(defun ,(intern (format "fate/%s-install-tools" name)) ()
       (interactive)

       (unless (listp ,tools)
         (error (format "Last argument must be a list of tools")))

       (unless (executable-find ,executable)
         (error (format "Cannot find `%s' executable" ,executable)))

       (message (format "Installing %d tools" (length ,tools)))
       (dolist (tool ,tools)
         (set-process-sentinel
          (funcall #'start-process (format "%s-tools" ,name) (format "*%s Tools*" (capitalize ,name))
            ,@command (if ,tool-name-fun
                        (funcall ,tool-name-fun tool)
                        tool))
          `(lambda (proc _)
             (let ((status (process-exit-status proc)))
               (if (= 0 status)
                   (message (format "Installed %s" ,tool))
                 (message (format "Failed to install %s" ,tool))))))))))

(provide 'core-packages)
;;; core-packages.el ends here
