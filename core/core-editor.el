;;; core-editor.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Binbin Ye

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
  (require 'core-load-paths))

(require 'posframe)
(require 're-builder)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; handle sooooooooo long line like one liner json file
(when (>= emacs-major-version 27)
  (global-so-long-mode t))

;; Do not create backup and auto save files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .#interlock files


;; disable bidi to improve longline rendering performance
;; reference https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(use-package tramp
  :defer t
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

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
  (lazy-count-prefix-format "%s/%s "))

(use-package replace
  :ensure nil
  :hook
  ((minibuffer-setup . (lambda ()
                         (define-key minibuffer-local-map (kbd "C-c C-h") #'fate/toggle-regex-cheatsheet-posframe)))
   (minibuffer-exit . fate/close-regex-cheatsheet-posframe)))

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

(use-package dired
  :ensure nil
  :config
  ;; dired - reuse current buffer
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-movement-style 'cycle)

  (setq dired-dwim-target t)
  :custom
  (dired-listing-switches "-alh" "show human readable size")
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t "guess where to copy to"))

;; Core package Projectile
(use-package projectile
  :diminish
  :commands projectile-project-root
  :bind
  (:map projectile-mode-map
        ("C-," . projectile-find-file)
        ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf)
  :custom projectile-switch-project-action 'projectile-dired)

(use-package recentf
  :hook
  (after-init . recentf-mode)
  :commands (recentf-expand-file-name)
  :config
  (progn
    (setq
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude (recentf-expand-file-name fate-cache-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

;; Power package visualizing undo redo history
(use-package undo-redo
  :ensure nil
  :bind
  (("C-/" . undo-only)
   ("M-_" . undo-redo)))

;; Pair parentheses, brace, quotes
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Core package move to beginning of code first
(use-package mwim
  :defer t
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2))

(defun fate/repo-dir-file-name()
  "Get file path relative to project root."
  (let* ((repo-buffer-name (substring buffer-file-name (length (projectile-project-root))))
         (repo-root-dir-name (car (last (split-string (or (projectile-project-root) "") "/" t)))))
    (concat (if repo-root-dir-name
              (concat repo-root-dir-name
                "/") repo-root-dir-name) repo-buffer-name)))

(defun fate/permanent-link-at-point()
  "Get permanent link of current line."
  (let
    ((remote-url (string-trim (shell-command-to-string "git remote get-url origin")))
     (hash (string-trim (shell-command-to-string "git rev-parse HEAD")))
     (relative-path (file-relative-name buffer-file-name (projectile-project-root))))
    (format "https://%s/blob/%s/%s#L%s"
      (concat (seq-reduce (lambda (string regexp-replacement-pair)
                           (replace-regexp-in-string
                             (car regexp-replacement-pair)
                             (cdr regexp-replacement-pair)
                             string))
               '(("\\.git$" . "") ("[[:alpha:]]+://" . "") ("git@" . "") (":" . "/"))
               remote-url))
      hash
      relative-path
      (format-mode-line "%l"))))

(use-package copy-as-format
  :bind
  ("C-c w g" . copy-as-format-github)
  :config
  (advice-add 'copy-as-format--extract-text :filter-return #'fate/copy-as-format--extract-text))

(defun fate/copy-as-format--extract-text (extracted-text)
  "Extend copy-as-format--extract-text to insert line number to extracted text.
EXTRACTED-TEXT is output from copy-as-format--extract-text."
  (if (use-region-p)
    (save-excursion
      (goto-char (region-beginning))
      (let* ((file-name-linenum (concat (fate/repo-dir-file-name) ":"
                                  (number-to-string (line-number-at-pos (region-beginning)))))
             (mode (buffer-local-value 'major-mode (current-buffer))))
        (with-temp-buffer
          (funcall mode)
          (insert file-name-linenum)
          (comment-line 1)
          (goto-char (point-max))
          (dotimes (_number 2)
            (newline))
          (insert extracted-text)
          (buffer-string))))))


(defun fate/easy-kill-on-buffer-file-name (n)
  "Extend buffer file name kill function.
if `N' is 8, return the path in repo.
if `N' is 9, return root dir + repo path."
  (unless (or buffer-file-name (projectile-project-root))
    (easy-kill-echo "No `buffer-file-name'")
    (cl-return))

  (easy-kill-adjust-candidate 'buffer-file-name
    (pcase n
      (`8 (concat (fate/repo-dir-file-name) ":" (format-mode-line "%l")))
      (`9 (fate/repo-dir-file-name)))))

;; Core package easy kill. easy to copy the buffer name/path
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  :commands (easy-kill-echo easy-kill-adjust-candidate)
  :config
  (defun easy-kill-on-permanent-link (_n)
    "copy permanent link of current line."
    (easy-kill-adjust-candidate 'permanent-link (fate/permanent-link-at-point)))

  (advice-add 'easy-kill-on-buffer-file-name :after #'fate/easy-kill-on-buffer-file-name)
  (add-to-list 'easy-kill-alist '(?p permanent-link)))

;; Core package expand-region. Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Core package hideshow. Code folding tool
(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :init
  (add-to-list 'hs-special-modes-alist '(fate-json-mode "{" "}" "/[*/]" nil)))
  ;; :bind
  ;; ("C-<tab>" . hs-toggle-hiding))

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-parsers-typescript treesit-fold-parsers-python)
  :init
  (defun fate/treesit-fold-range-jsx-self-close-tag (node offset)
    "Define fold range for `jsx_self_closing_element'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
    (when-let* ((identifier-node (car (treesit-fold-find-children node "identifier")))
                (beg (treesit-node-end identifier-node))
                (end (treesit-node-end node)))
      (treesit-fold--cons-add (cons beg (- end 2)) offset)))

  (defun fate/treesit-fold-parers-tsx ()
    "Rule set for tsx"
    (append
      (treesit-fold-parsers-typescript)
      '((jsx_element . treesit-fold-range-html)
        (jsx_attribute . treesit-fold-range-seq)
        (jsx_expression . treesit-fold-range-seq)
        (jsx_self_closing_element . fate/treesit-fold-range-jsx-self-close-tag))))

  (defun fate/treesit-fold-range-python-block (node offset)
    "Define fold range for `if_statement'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
    (when-let* ((colon-node (car (treesit-fold-find-children node ":")))
                (beg (treesit-node-start colon-node))
                (end (treesit-node-end node)))
      (treesit-fold--cons-add (cons (+ beg 1) end) offset)))

  (defun fate/treesit-fold-parsers-python ()
    (append
      (treesit-fold-parsers-python)
      (mapcar (lambda (keyword) (cons keyword 'fate/treesit-fold-range-python-block))
        '(while_statement for_statement if_statement elif_clause else_clause
           match_statement case_clause try_statement except_clause with_statement))))
  ;; jtsx mode
  (add-to-list 'treesit-fold-range-alist `(jtsx-tsx-mode . ,(fate/treesit-fold-parers-tsx)))
  (add-to-list 'treesit-fold-summary-parsers-alist '(jtsx-tsx-mode . treesit-fold-summary-javadoc))
  (add-to-list 'treesit-fold-range-alist `(jtsx-typescript-mode . ,(treesit-fold-parsers-typescript)))

  ;; python mode
  (setq treesit-fold-range-alist (assq-delete-all 'python-mode treesit-fold-range-alist))
  (setq treesit-fold-range-alist (assq-delete-all 'python-ts-mode treesit-fold-range-alist))
  (add-to-list 'treesit-fold-range-alist `(python-ts-mode . ,(fate/treesit-fold-parsers-python)))
  (add-to-list 'treesit-fold-range-alist `(python-mode . ,(fate/treesit-fold-parsers-python)))

  (global-treesit-fold-mode)
  :bind
  ("C-<tab>" . treesit-fold-toggle))


;; Core package winum. Easy navigation to different buffers
(use-package winum
  :hook (after-init . winum-mode)
  :bind
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8))

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Avy. Jump to anywhere like a sniper
(use-package avy
  :ensure t
  :bind
  ("C-c j" . avy-transient)
  :init
  (with-eval-after-load 'transient
    (transient-define-prefix avy-transient ()
      "Avy"
      ["Avy"
       ("l" "Line" avy-goto-line)
       ("j" "Jump" avy-goto-word-or-subword-1)]

      [["Char"
        ("c" "Char" avy-goto-char)
        ("C" "2 Chars" avy-goto-char-2)
        ("L" "Char in line" avy-goto-char-in-line)]

       ["Word"
        ("w" "Word by char" avy-goto-word-1)
        ;; jump to beginning of some word
        ("W" "Some word" avy-goto-word-0)
        ("s" "Subword by char" avy-goto-subword-1)
        ("S" "Some subword" avy-goto-subword-0)]]))
  :config
  (avy-setup-default)
  :custom
  (avy-background t))


(use-package move-text
  :bind
  ("M-S-<up>" . move-text-up)
  ("M-S-<down>" . move-text-down))

(use-package string-inflection
  :defer t
  :init
  (with-eval-after-load 'transient
    (transient-define-prefix string-inflection-transient ()
      "String Inflection"
      [[""
        ("c" "camelCase" string-inflection-lower-camelcase)
        ("C" "CamelCase" string-inflection-camelcase)
        ("_" "Underscore" string-inflection-underscore)]

       [""
        ("-" "Kebab" string-inflection-kebab-case)
        ("u" "Upper" string-inflection-upcase)
        ("t" "Cycle" string-inflection-all-cycle :transient nil)]])))

(use-package multiple-cursors
  :diminish
  :init
  :bind
  ("C->"   . mc/mark-next-like-this)
  ("C-<"   . mc/mark-previous-like-this)
  ("C-M->" . mc/skip-to-next-like-this)
  ("C-M-<" . mc/skip-to-previous-like-this))

(use-package crux
  :bind
  (("C-c o" . crux-open-with)
   ("M-o" . crux-smart-open-line)
   ("s-o" . crux-smart-open-line-above)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c I" . crux-find-user-init-file)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-top-join-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode t))

(use-package quickrun
  :bind (("<f9>" . quickrun)
         ("C-c x" . quickrun)))

(use-package bm
  :bind
  (("<f2>" . bm-next)
   ("<C-f2>" . bm-toggle))
  :custom
  (bm-buffer-persistence nil "Do not save bookmarks"))

(use-package color-rg
  :vc (:url "https://github.com/manateelazycat/color-rg")
  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  :config
  (custom-set-faces
   `(color-rg-font-lock-header-line-text ((t (:foreground ,(doom-color 'base7)))))
   `(color-rg-font-lock-header-line-keyword ((t (:foreground ,(doom-color 'red)))))
   `(color-rg-font-lock-header-line-directory ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-header-line-edit-mode ((t (:foreground ,(doom-color 'magenta)))))
   `(color-rg-font-lock-command ((t (:background ,(doom-color 'modeline-bg) :foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-file ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-line-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-column-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-match ((t (:foreground ,(doom-color 'red)))))))

(defun fate/kill-buffer (prefix path-pattern)
  "Kill buffer with patterns.  PATH-PATTERN is pattern to compare with buffer name.
PREFIX determines whether to match file name or buffer name."
  (interactive "P\nsbuffer name pattern: ")
  (when path-pattern
    (cl-dolist (buffer (buffer-list))
      (let* ((file-name (cond
                          ((not prefix) (buffer-file-name buffer))
                          ((equal prefix 0) (buffer-name buffer)))))
        (message file-name)
        (when (and file-name
                   (string-match path-pattern file-name))
          (kill-buffer buffer))))))

(require 'fate-auto-complete)
(require 'fate-align-text)
(require 'fate-windows)
(provide 'core-editor)
;;; core-editor.el ends here
