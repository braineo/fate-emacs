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

(defvar forge-pull-repos '()
  "Remaining repos")

(pcase-dolist (`(,name ,id)
               (forge-sql [:select [name id] :from repository
                           :where (= condition ':tracked)
                           :order-by [(asc owner) (asc name)]]))
  (progn
    (when-let ((repo (forge-get-repository :id id))
               (name (oref repo name)))
      (setq forge-pull-total (1+ forge-pull-total))
      (push name forge-pull-repos)
      (forge--pull repo (lambda (_)
                          (setq forge-pull-completed (1+ forge-pull-completed))
                          (setq forge-pull-repos (delete name forge-pull-repos))
                          (when (length> forge-pull-repos 0)
                            (print (format "Remaining repos %s" forge-pull-repos))))))))

(while (< forge-pull-completed forge-pull-total)
  (sleep-for 1))
