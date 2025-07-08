;;; fate-writing.el --- org and markdown related settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  binbin

;; Author: binbin <binbin@BTSB25100GJU>
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

(eval-when-compile
  (require 'which-func)
  (require 'copy-as-format)
  (require 'org))

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git"))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . (lambda()
                           (setq-local prettier-js-args
                            '("--parser" "markdown")))))

(use-package markdown-toc
  :defer)


(defun fate/enable-jinx ()
  "Enable jinx only for writing modes."
  (unless (member major-mode '(yaml-ts-mode toml-ts-mode nxml-mode))
    (jinx-mode)))

(use-package jinx
  :hook ((markdown-mode text-mode) . fate/enable-jinx)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  ;; disable checking spell of Chinses
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package org
  :ensure nil
  :hook (org-mode . (lambda() (set-fill-column 120)))
  :config
  (add-to-list 'org-export-backends '(pandoc))
  :custom
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t)
                              (js . t)
                              (python . t))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package ox-pandoc)

(use-package org-capture
  :ensure nil
  :bind
  ("C-c m" . (lambda () "Open tmp capture window"
               (interactive)
               (org-capture nil "t")))
  :config
  (add-to-list 'org-capture-templates
               `("cf" "Code Reference with Comments to Current Task"
                 plain (clock)
                 "%(fate/format-org-capture-code-block \"%F\")\n\n   %?"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               `("cl" "Link to Code Reference to Current Task"
                 plain (clock)
                 "%(fate/format-org-capture-code-block \"%F\")"
                 :empty-lines 1 :immediate-finish t))

  ;; https://github.com/howardabrams/hamacs/blob/5182b352cd2d4e03d18b5a37505db64e1fdf1f62/ha-org-clipboard.org
  (defun fate/format-org-capture-code-block (filename)
    "Given a file, F, this captures the currently selected text
  within an Org SRC block with a language based on the current mode
  and a backlink to the function and the file."

    (with-current-buffer (find-buffer-visiting filename)
      (let* ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
             (func-name (which-function))
             (extracted-text (copy-as-format--extract-text))
             (file-name   (buffer-file-name))
             (file-base   (file-name-nondirectory file-name))
             (line-number (line-number-at-pos (region-beginning)))
             (initial-txt (if (null func-name)
                            (format "From [[file:%s::%s][%s]]:"
                              file-name line-number file-base)
                           (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                             func-name file-name line-number
                             file-base))))

        (format " %s
#+begin_src %s
  %s
#+end_src" initial-txt org-src-mode extracted-text))))



  (defun fate/org-capture-code (&optional start end)
    "Send the selected code to the current clocked-in org-mode task."
    (interactive)
    (org-capture nil "cl"))

  (defun fate/org-capture-code-comment (&optional start end)
    "Send the selected code (with comments) to the current clocked-in org-mode task."
    (interactive)
    (org-capture nil "cf")))

(use-package org-mem
  :defer
  (org-mem-updater-mode)
  :custom
  (org-mem-do-sync-with-org-id t))

(use-package org-node
  :init
  :config
  (org-node-cache-mode)
  :custom
  (org-node-file-directory-ask t))

(use-package atomic-chrome
  :demand t
  :vc (:url "https://github.com/KarimAziev/atomic-chrome")
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server)
  :custom
  (atomic-chrome-url-major-mode-alist '(("git" . gfm-mode)
                                        ("\\(mattermost\\|slack\\)" . gfm-mode))))


(defun fate/format-gitlab-json (format-type)
  (interactive
   (list (intern (completing-read "Format type: "
                                  '("email" "planning")
                                  nil t))))
  (fate/format-gitlab-json-to-markdown
   (cond
    ((eq format-type 'email) #'fate/gitlab-email-formatter)
    ((eq format-type 'planning) #'fate/gitlab-planning-formatter)
    (t (error "Unknown format type: %s" (symbol-name format-type))))))

(defun fate/format-gitlab-json-to-markdown (formatter-fn)
  "Format GitLab issues JSON in current buffer to markdown list in temp buffer."
  (let ((json-data (json-parse-string (buffer-string)))
        (markdown-lines '()))

    ;; Extract issues vector from the nested JSON structure
    (let ((issues (gethash "nodes"
                         (gethash "issues"
                           (gethash "project"
                             (gethash "data" json-data))))))

      (dotimes (i (length issues))
        (let* ((issue (aref issues i))
               (assignees-nodes (gethash "nodes" (gethash "assignees" issue)))
               (assignees '()))

          (when assignees-nodes
            (dotimes (j (length assignees-nodes))
              (push (aref assignees-nodes j) assignees)))
          (push (funcall formatter-fn issue (reverse assignees)) markdown-lines)))

      ;; Create temp buffer and insert formatted markdown
      (let ((temp-buffer (get-buffer-create "*GitLab Issues Markdown*")))
        (with-current-buffer temp-buffer
          (erase-buffer)
          (insert (string-join (reverse markdown-lines) "\n"))
          (insert "\n")
          (markdown-mode))
        (pop-to-buffer temp-buffer)))))


(defun fate/gitlab-planning-formatter (issue assignees)
  "Format issue for planning."
  (format "- [ ] %s %s %s"
          (gethash "title" issue)
          (gethash "reference" issue)
          (if assignees
              (string-join (mapcar (lambda (a) (concat "@" (gethash "username" a))) assignees) " ")
            "Unassigned")))

(defun fate/gitlab-email-formatter (issue assignees)
  "Format issue for email/slack."
  (format "- [%s](%s) by %s"
          (gethash "title" issue)
          (gethash "webUrl" issue)
          (if assignees
              (string-join (mapcar (lambda (a) (gethash "name" a)) assignees) ", ")
            "Unassigned")))

(defun fate/fill-iteration-dates ()
  "Fill in iteration dates in markdown templates.
Prompts for start and end dates, calculates endgame dates,
and replaces template strings in the current buffer."
  (interactive)
  (let* ((start-date (org-read-date nil nil nil "Start date: "))
         (end-date (org-read-date nil nil nil "End date: "))
         (end-date-time (org-time-string-to-time end-date))
         (endgame-end-date (format-time-string "%Y-%m-%d"
                                             (subtract-working-days end-date-time 0)))
         (endgame-start-date (format-time-string "%Y-%m-%d"
                                               (subtract-working-days
                                                (org-time-string-to-time endgame-end-date) 3))))

    ;; Replace template strings in buffer
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "<start_date>" nil t)
        (replace-match start-date))

      (goto-char (point-min))
      (while (search-forward "<end_date>" nil t)
        (replace-match end-date))

      (goto-char (point-min))
      (while (search-forward "<endgame_start_date>" nil t)
        (replace-match endgame-start-date))

      (goto-char (point-min))
      (while (search-forward "<endgame_end_date>" nil t)
        (replace-match endgame-end-date)))

    (message "Iteration dates filled: %s to %s (endgame: %s to %s)"
             start-date end-date endgame-start-date endgame-end-date)))

(defun subtract-working-days (date-time num-days)
  "Subtract NUM-DAYS working days from DATE-TIME.
Working days are Monday through Friday.
If DATE-TIME is a weekend, first moves to the previous Friday."
  (let ((current-time date-time)
        (days-subtracted 0))
    (while (or (< days-subtracted num-days)
             (not (is-weekday current-time)))
      (when (is-weekday current-time)
        (setq days-subtracted (1+ days-subtracted)))
      (setq current-time (time-subtract current-time (days-to-time 1))))

    current-time))

(defun is-weekday (date-time)
  (let ((day-of-week (string-to-number (format-time-string "%w" date-time))))
    (and (>= day-of-week 1) (<= day-of-week 5))))

(provide 'fate-writing)
;;; fate-writing.el ends here
