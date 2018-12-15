;;; core/core-load-paths.el -*- lexical-binding: t; -*-

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defvar fate-directory
  user-emacs-directory
  "Root directory of Fate Emacs")

(defconst fate-core-directory
  (expand-file-name "core" fate-directory)
  "Fate core directory")

(defconst fate-modules-directory
  (expand-file-name "modules" fate-directory)
  "Fate modules directory")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

;; load paths
(mapc 'add-to-load-path
      `(
        ,fate-core-directory
        ,fate-modules-directory))
        
(provide 'core-load-paths)
;;; core-load-paths.el ends here