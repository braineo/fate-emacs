;;; update_forge_repos.el ---                               -*- lexical-binding: t; -*-

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; prefer to use the trancient package from elpa
(setq load-path (reverse load-path))

(require 'forge)

;; messages are re-emitted by `forge-pull--log', so the bar stays last
(setq inhibit-message t)

(setq forge-database-file "~/.emacs.d/var/forge/database.sqlite")

(defvar forge-pull-completed 0
  "Number of completed `forge-pull` operations.")

(defvar forge-pull-total 0
  "Number of completed `forge-pull` operations.")

(defvar forge-pull-repos '()
  "Remaining repos")

(defconst forge-pull-tty (not (member (getenv "TERM") '(nil "dumb")))
  "Non-nil on a real terminal; from cron just print the log lines.")

(defun forge-pull--report ()
  "Redraw a one-line progress bar in place on stdout."
  (when forge-pull-tty
    (let* ((width 30)
           (done (/ (* width forge-pull-completed) (max 1 forge-pull-total))))
      (send-string-to-terminal
       (format "\r[%s%s] %d/%d %s\e[K"
               (make-string done ?=)
               (make-string (- width done) ?\s)
               forge-pull-completed forge-pull-total
               (string-join (last forge-pull-repos 3) " "))))))

(defun forge-pull--log (fmt &rest args)
  "Print a log line above the progress bar, then redraw the bar."
  (when fmt
    (send-string-to-terminal
     (concat (if forge-pull-tty "\r\e[K" "") (apply #'format-message fmt args) "\n"))
    (forge-pull--report)))

(advice-add 'message :after #'forge-pull--log)

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
                          (forge-pull--report))))))

(forge-pull--report)
(while (< forge-pull-completed forge-pull-total)
  (sleep-for 1))
(send-string-to-terminal "\n")
