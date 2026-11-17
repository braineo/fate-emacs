;;; prune-packages.el --- Remove packages init.el no longer uses -*- lexical-binding: t; -*-

;; Usage: make prune
;;
;; Loads init.el (and user-lisp/), records every package `use-package'
;; ensures, makes that the new `package-selected-packages', then deletes
;; installed packages nothing needs, after asking.
;;
;; Only top-level `use-package' forms count: a package you only use via M-x
;; with no `use-package' line is removed.  ELPA copies of built-ins (eglot,
;; project, ...) are never touched.

(defvar prune--used nil)

(define-advice use-package-ensure-elpa (:before (name args _state &rest _) prune)
  (dolist (a args)
    (push (cond ((eq a t) name) ((consp a) (car a)) (t a)) prune--used)))

(define-advice use-package-vc-install (:before (arg &rest _) prune)
  (push (car arg) prune--used))

(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp"))
(load (locate-user-emacs-file "early-init.el") nil t)
(package-activate-all)
(require 'use-package-ensure)
;; Under -Q customize refuses to save unless `user-init-file' is set.
(setq user-init-file (locate-user-emacs-file "init.el"))
(load user-init-file nil t)
(dolist (f (directory-files (locate-user-emacs-file "user-lisp") t "\\`[^.].*\\.el\\'"))
  (load f nil t))

(setq package-selected-packages
      (sort (delete-dups (seq-filter (lambda (p) (and p (symbolp p) (not (eq p t))))
                                     prune--used))
            #'string<))
;; Not `package-autoremove': it deletes in alist order and errors when a
;; removable package (flymake) is a dep of one not yet deleted (eglot).
;; Everything here is unneeded, so force past that check.
(let ((removable (seq-remove (lambda (p) (package-built-in-p (package-desc-name p)))
                             (package--removable-packages))))
  (if (not removable)
      (message "Nothing to remove")
    (when (y-or-n-p (format "Delete %d packages (%s)? " (length removable)
                            (mapconcat #'package-desc-full-name removable ", ")))
      (dolist (p removable) (package-delete p t)))))
(customize-save-variable 'package-selected-packages package-selected-packages)

;;; prune-packages.el ends here
