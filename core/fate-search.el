;;; fate-search.el --- Search, replace, regex and grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Binbin Ye

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

;; Search, replace, regex and grep

;;; Code:

(defconst regex-cheatsheet-posframe-name "*Regex Cheat Sheet*")

(defconst fate/regex-cheatsheet-string
  "| Function                   | PCRE                  | Emacs                              |
|----------------------------|-----------------------|------------------------------------|
| Group, Count, Alternation  | (cat|dog){3}          | \\(cat\\|dog\\)\\{3\\}                  |
| Classes                    | \\w \\d \\s              | [[:word:]] [[:digit:]] [[:space:]] |
| Word Constituent           | \\w does not match ' \" | \\w match syntax table text         |
| New line                   | \\n                    | C-q C-j                            |
| Start/End Of Buffer/String |                       | \\` \\'                              |
| Captured                   | $1 $2                 | \\1 \\2                              |")

(defun fate/toggle-regex-cheatsheet-posframe ()
  "Toggle display of a floating regex help posframe during search.
When called, if the posframe is already visible, it is dismissed.
Otherwise, it is shown at point without disturbing your current window layout."
  (interactive)
  (if (get-buffer regex-cheatsheet-posframe-name)
    (posframe-delete regex-cheatsheet-posframe-name)
    (posframe-show regex-cheatsheet-posframe-name
                   :string fate/regex-cheatsheet-string
                   :poshandler #'posframe-poshandler-window-bottom-right-corner
                   :border-width 1
                   :border-color "gray"
                   :accept-focus nil
                   :timeout 0)))

(defun fate/close-regex-cheatsheet-posframe ()
  (when (get-buffer regex-cheatsheet-posframe-name)
    (posframe-delete regex-cheatsheet-posframe-name)))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)
              ("C-c C-h" . fate/toggle-regex-cheatsheet-posframe))
  :hook
  (isearch-mode-end . fate/close-regex-cheatsheet-posframe)
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  ;; space matches anything in between: "foo bar" finds "foo...bar"
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-repeat-on-direction-change t)
  (isearch-allow-scroll 'unlimited))

(use-package replace
  :ensure nil
  :bind ((:map minibuffer-local-map
               ("C-c C-h" . fate/toggle-regex-cheatsheet-posframe))
         (:map occur-mode-map
               ("C-x C-q" . occur-edit-mode)))
  :hook (minibuffer-exit . fate/close-regex-cheatsheet-posframe))

;; Minor mode for editing regular expressions in the minibuffer.
(minibuffer-regexp-mode 1)

(use-package casual
  :defer t)

(use-package casual-isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ("<f2>" . casual-isearch-tmenu)))

(use-package re-builder
  :ensure nil
  :bind ((:map reb-mode-map
           ("C-<return>" . fate/reb-query-replace-regexp))
         (:map reb-subexp-mode-map
           ("C-<return>" . fate/reb-query-replace-regexp)))
  :custom
  (reb-re-syntax 'string)
  :config
  ;; Stay in the original buffer position on re-builder
  (define-advice reb-update-overlays
      (:around (orig-fun &rest args) save-window-excursion)
    "Advice for `reb-update-overlays' to inhibit window config changes."
    (save-window-excursion (apply orig-fun args)))

  (defun fate/reb-query-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument `DELIMITED', only replace matches
surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-value 'reb-regexp))
           (start (use-region-beginning))
           (end (use-region-end))
           (replacement (query-replace-read-to re
                          (concat "Query replace"
                            (if current-prefix-arg
                              (if (eq current-prefix-arg '-) " backward" " word")
                              "")
                            " regexp"
                            (if (use-region-p) " in region"))
                          t)))
      (with-selected-window reb-target-window
        (reb-quit)
        (query-replace-regexp re replacement delimited start end)))))


(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  (xref-file-name-display 'project-relative))

;; Applies to grep, occur, compile and flycheck jumps
(setopt next-error-recenter '(4)
        next-error-message-highlight t)

(use-package color-rg
  :vc (:url "https://github.com/manateelazycat/color-rg")
  :config
  (add-to-list 'display-buffer-alist
             '("\\*color-rg\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.35) ; Take up 35% of the screen width
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil))

(provide 'fate-search)
;;; fate-search.el ends here
