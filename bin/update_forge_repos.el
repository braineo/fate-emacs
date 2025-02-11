;;; update_forge_repos.el ---                               -*- lexical-binding: t; -*-

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; prefer to use the trancient package from elpa
(setq load-path (reverse load-path))

(require 'forge)

(setq forge-database-file "~/.emacs.d/var/forge/database.sqlite")

(defvar forge-pull-completed 0
  "Number of completed `forge-pull` operations.")

(defvar forge-pull-total 0
  "Number of completed `forge-pull` operations.")

(pcase-dolist (`(,name ,id)
               (forge-sql [:select [name id] :from repository
                           ;;:where (= class 'gitlab)
                           :order-by [(asc owner) (asc name)]]))
  (progn
    (setq forge-pull-total (1+ forge-pull-total))
    (when-let ((repo (forge-get-repository :id id))
               ((forge-get-repository repo :tracked?)))
      (forge--pull repo (lambda () (setq forge-pull-completed (1+ forge-pull-completed)))))))

(while (< forge-pull-completed forge-pull-total)
  (sleep-for 1))
