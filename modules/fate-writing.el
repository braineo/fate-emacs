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
  (require 'copy-as-format))

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


(use-package atomic-chrome
  :demand t
  :vc (:url "https://github.com/KarimAziev/atomic-chrome")
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server)
  :custom
  (atomic-chrome-url-major-mode-alist '(("git" . gfm-mode))))

(provide 'fate-writing)
;;; fate-writing.el ends here
