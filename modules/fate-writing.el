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
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))
  :custom
  (markdown-gfm-use-electric-backquote nil)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-wiki-links t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-open-command "mdopen")

  :hook (markdown-mode . (lambda()
                           (setq-local electric-pair-pairs
                                       (append electric-pair-pairs '((?` . ?`))))
                           (setq-local electric-pair-text-pairs '((?` . ?`)))
                           (setq-local prettier-js-args
                            '("--parser" "markdown")))))

(use-package markdown-toc
  :defer)

(use-package mermaid-mode
  :defer)

(use-package grip-mode
  :autoload grip-mode
  :init
  (with-eval-after-load 'markdown-mode
    (bind-key "g" #'grip-mode markdown-mode-command-map))
  :custom
  (grip-use-mdopen t))


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
  (dolist (lang-mode '(("bash" . bash-ts)
                       ("python" . python-ts)
                       ("tsx" . jtsx-tsx)
                       ("typescript" . jtsx-typescript)
                       ("jsx" . jtsx-jsx)
                       ("javascript" . jtsx-jsx)))
    (add-to-list 'org-src-lang-modes lang-mode))

  (dolist (lang '(jsx javascript typescript tsx))
    (defalias (intern (format "org-babel-execute:%s" lang))
              'org-babel-execute:js
              (format "Execute %s code using org-babel-execute:js." lang)))
  :custom
  (org-babel-js-cmd "bun")
  (org-babel-python-command (if (executable-find "uv") "uv run python" "python3"))
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t)
                              (js . t)
                              (python . t))))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (with-eval-after-load 'modus-themes
    (modus-themes-with-colors
      (setq org-modern-todo-faces
            `(("BACKLOG"   . (:background ,bg-dim :foreground ,fg-main))
              ("CONCEPT"   . (:background ,blue-intense :foreground ,bg-main))
              ("DESIGN"    . (:background ,magenta-intense :foreground ,bg-main))
              ("IMPLEMENT" . (:background ,yellow-intense :foreground ,bg-main))
              ("REVIEW"    . (:background ,cyan-intense :foreground ,bg-main))
              ("DROPPED"   . (:background ,red-intense :foreground ,bg-main :strike-through t)))))))

(use-package ox-pandoc)

(use-package org-capture
  :ensure nil
  :bind
  (("C-c c" . org-capture))
  :init
  (defvar fate-code-capture-session-file nil
    "The file used for the active code capturing session.")

  (defun fate/start-code-capturing-session ()
    "Start or resume a code capturing session.

If creating a new session, it prompts for a title and a destination folder.
The filename is generated from the title by lowercasing it, replacing
non-alphanumeric characters with hyphens, and stripping leading/trailing
hyphens. If the title is empty or only special characters, it defaults
to `session.org`. If a file with the generated name already exists,
a timestamp suffix `YYYYMMDDHHMMSS` is appended to ensure uniqueness.

If resuming, it prompts to select an existing file."
    (interactive)
    (let ((action (completing-read "Session action: " '("Create new session" "Resume existing session") nil t)))
      (if (string= action "Create new session")
          (let* ((title (read-string "Session title: "))
                 (dir (read-directory-name "Select folder: " (or (bound-and-true-p org-directory) "~/")))
                 (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))
                 (slug (replace-regexp-in-string "^-\\|-$" "" slug))
                 (filename (if (string-empty-p slug) "session.org" (format "%s.org" slug)))
                 (filepath (expand-file-name filename dir)))
            (when (file-exists-p filepath)
              (setq filepath (expand-file-name (format "%s-%s.org" slug (format-time-string "%Y%m%d%H%M%S")) dir)))
            (with-temp-file filepath
              (insert (format "#+TITLE: %s\n#+DATE: %s\n\n" title (format-time-string "[%Y-%m-%d %a %H:%M]"))))
            (setq fate-code-capture-session-file filepath))
        (setq fate-code-capture-session-file
              (expand-file-name (read-file-name "Select session .org file: "
                                                (and fate-code-capture-session-file
                                                     (file-name-directory fate-code-capture-session-file))
                                                nil t nil
                                                (lambda (f) (or (file-directory-p f)
                                                                (string-match-p "\\.org\\'" f))))))))
    (message "Active code capturing session is now: %s" fate-code-capture-session-file))

  (defun fate/goto-code-capture-session ()
    "Navigate to the correct location in the active code capturing session."
    (unless fate-code-capture-session-file
      (user-error "No active code capturing session.  Run `fate/start-code-capturing-session' first."))
    (set-buffer (org-capture-target-buffer fate-code-capture-session-file))
    (goto-char (point-max)))

  :config
  ;; Define the "c" prefix group so standard org-capture menus render it properly
  (add-to-list 'org-capture-templates
               '("c" "Code Capturing Session Options"))

  (add-to-list 'org-capture-templates
               `("cf" "Code Reference with Comments to Current Session"
                 plain (function fate/goto-code-capture-session)
                 "%(fate/format-org-capture-code-block \"%F\")\n\n   %?"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               `("cl" "Link to Code Reference to Current Session"
                 plain (function fate/goto-code-capture-session)
                 "%(fate/format-org-capture-code-block \"%F\")"
                 :empty-lines 1 :immediate-finish t))

  (add-to-list 'org-capture-templates
             `("b" "Backlog Task (Unassigned)" entry
               (file+headline ,(concat org-directory "/work/project/roadmap.org") "Backlog")
               "** %?\n"
               :empty-lines 1))

  ;; https://github.com/howardabrams/hamacs/blob/5182b352cd2d4e03d18b5a37505db64e1fdf1f62/ha-org-clipboard.org
  (defun fate/format-org-capture-code-block (filename)
    "Given a file, F, this captures the currently selected text
  within an Org SRC block with a language based on the current mode
  and a backlink to the function and the file."

    (with-current-buffer (find-buffer-visiting filename)
      (let* ((mode-name-no-mode (intern (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))))
             (org-src-mode (cond
                            ;; Shell scripts dynamically sniffed from shebang or extension
                            ((memq major-mode '(sh-mode bash-ts-mode))
                             (let ((ext (and buffer-file-name (file-name-extension buffer-file-name)))
                                   (shebang (save-excursion
                                              (goto-char (point-min))
                                              (when (looking-at "^#![ \t]*\\(?:/usr/bin/env +\\)?\\(?:.+/\\)?\\(bash\\|zsh\\|ksh\\|sh\\|fish\\)\\b")
                                                (match-string 1)))))
                               (cond (shebang shebang)
                                     ((member ext '("bash" "zsh" "ksh" "sh" "fish")) ext)
                                     (t "bash"))))
                            ;; jtsx-jsx-mode is a many-to-one mode for js/jsx, dynamically fetch from file extension
                            ((eq major-mode 'jtsx-jsx-mode)
                             (if (and buffer-file-name (string= (file-name-extension buffer-file-name) "jsx"))
                                 "jsx"
                               "javascript"))
                            ;; Otherwise do a reverse lookup
                            (t
                             (or (car (rassq mode-name-no-mode org-src-lang-modes))
                                 (symbol-name mode-name-no-mode)))))
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
    "Send the selected code to the active code capturing session."
    (interactive)
    (org-capture nil "cl"))

  (defun fate/org-capture-code-comment (&optional start end)
    "Send the selected code (with comments) to the active code capturing session."
    (interactive)
    (org-capture nil "cf")))

(use-package org-mem
  :defer
  :config
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
  (let* ((start-date (org-read-date nil nil nil "Start date: " nil "-1mon"))
         (start-time (org-time-string-to-time start-date))
         ;; Pre-fill the prompt with "++4fri" (4th friday from start-time)
         (end-date (org-read-date nil nil nil "End date: " start-time "++4sun"))
         (end-date-time (org-time-string-to-time end-date))
         (endgame-end-date (org-read-date nil nil "--fri" nil end-date-time))
         (endgame-start-date (org-read-date nil nil "--mon" nil end-date-time)))

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


(provide 'fate-writing)
;;; fate-writing.el ends here
