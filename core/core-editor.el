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

(use-package posframe)
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
(setopt indent-tabs-mode nil   ;; don't use tabs to indent
        tab-width 8            ;; but maintain correct appearance
        ;; Newline at end of file
        require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; handle sooooooooo long line like one liner json file
(global-so-long-mode t)

;; Emacs fontifies as you type, which can cause lags when tree sitter mode in large buffer
(setq redisplay-skip-fontification-on-input t)

;; disable bidi to improve longline rendering performance
;; reference https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; Built-in undo/redo and case DWIM commands
(use-package simple
  :ensure nil
  :bind
  (("C-/" . undo-only)
   ("M-_" . undo-redo)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("M-c" . capitalize-dwim))
  :custom
  ;; after one C-u C-SPC, plain C-SPC keeps popping the mark
  (set-mark-command-repeat-pop t))

;; Repeat the last key of a command alone: C-x o o o, C-x u u, M-g n n
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

;; C-v/M-v reach the buffer edge instead of signaling an error
(setopt scroll-error-top-bottom t
        scroll-preserve-screen-position t)

(put 'narrow-to-region 'disabled nil)

(keymap-global-set "C-h K" #'describe-keymap)

;; Pair parentheses, brace, quotes
(use-package elec-pair
  :ensure nil
  :hook ((after-init . electric-pair-mode)
         (minibuffer-setup . (lambda () (electric-pair-local-mode -1)))))

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
  (advice-add 'copy-as-format--extract-text :filter-return #'fate/copy-as-format--extract-text)
  (advice-add 'copy-as-format--language :filter-return #'fate/copy-as-format--language))

(defun fate/copy-as-format--language (ext)
  "Map major-mode to language names for copy-as-format, with fallback to extension."
  (let ((mode-name (symbol-name major-mode)))
    (pcase mode-name
      ((or "c++-mode" "c++-ts-mode" "c-mode" "c-ts-mode") "cpp")
      ((or "js-mode" "js2-mode" "js-ts-mode" "javascript-mode") "javascript")
      ((or "jtsx-typescript-mode" "typescript-ts-mode" "typescript-mode") "typescript")
      ((or "jtsx-tsx-mode" "tsx-ts-mode") "tsx")
      ("jtsx-jsx-mode" (if (and (stringp ext) (string= ext "jsx")) "jsx" "javascript"))
      ("less-css-mode" "less")
      ("emacs-lisp-mode" "elisp")
      ("sh-mode" (if (boundp 'sh-shell) (symbol-name sh-shell) "sh"))
      ("fundamental-mode" ext)
      (_ (replace-regexp-in-string "-ts-mode$\\|-mode$" "" mode-name)))))

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
          (delay-mode-hooks (funcall mode))
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
  (if (not (or buffer-file-name (projectile-project-root)))
    (easy-kill-echo "No `buffer-file-name'")
    (easy-kill-adjust-candidate 'buffer-file-name
      (pcase n
        (8 (concat (fate/repo-dir-file-name) ":" (format-mode-line "%l")))
        (9 (fate/repo-dir-file-name))))))

;; Save current clipboard content into kill ring before overwriting
;; Do not save same content in the kill ring
(setopt save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t)

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
  (add-to-list 'hs-special-modes-alist '(json-ts-mode "{" "}" "/[*/]" nil)))
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
        ("c" "camelCase" string-inflection-lower-camel-case)
        ("C" "CamelCase" string-inflection-camel-case)
        ("s" "snake_case" string-inflection-snake-case)
        ("S" "Snake_Case" string-inflection-capital-snake-case)]

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

;;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/

(defun er-keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))

(keymap-global-set "<remap> <keyboard-quit>" #'er-keyboard-quit)

(use-package ghostel
  :hook (ghostel-mode . (lambda()
                          (setq-local global-hl-line-mode nil)))
  :bind
  (("<f5>" . ghostel)))

(defun fate/open-term-here ()
  "Open a terminal in current location."
  (interactive)
  (let ((dir (expand-file-name (or default-directory "~"))))
    (call-process "wezterm" nil 0 nil "start" "--new-tab" "--cwd" dir)))

(keymap-global-set "C-<f5>" #'fate/open-term-here)

(require 'fate-auto-complete)
(require 'fate-align-text)
(require 'fate-windows)
(require 'fate-search)
(require 'fate-files)
(provide 'core-editor)
;;; core-editor.el ends here
