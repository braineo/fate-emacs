;;; fate-custom.el --- initialize custom values before starting up  -*- no-byte-compile: t -*-


;;; Commentary:
;;; This file is used for custom setting in different environment, private settings should not go to main repo
;;; For executable path/versions you can find following examples on how to setup.
;;; Alternatively you can use `purcell/exec-path-from-shell`, but it is very slow on Mac.

;;; Code:

;; Font
(when (display-graphic-p)
  (cond
    ((font-installed-p "JetBrains Mono") (set-face-attribute 'default nil :font "JetBrains Mono" :height 75))
    ((font-installed-p "Operator Mono") (set-face-attribute 'default nil :font "Operator Mono" :height 80 :weight 'semilight)))

  (cond
   ((font-installed-p "LXGW Neo XiHei")
    (dolist (char '(han kana cjk-misc))
      (set-fontset-font t char "LXGW Neo XiHei")))
   ((font-installed-p "Noto Sans")
    (progn
      (set-fontset-font t 'han "Noto Sans CJK SC Regular")
      (set-fontset-font t 'kana "Noto Sans CJK JP Regular")
      (set-fontset-font t 'cjk-misc "Noto Sans CJK Sc Regular"))))

  (when (font-installed-p "Noto Color Emoji")
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)))

;; Fix path issue or executable not found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/home/user/.cargo/bin:/home/user/go/bin"))
;; (setq exec-path (append '("/usr/local/bin" "/home/user/.cargo/bin" "/home/user/go/bin") exec-path))

;; GO related env vars
;; (setenv "GOPATH" "/home/user/go")
;; (setenv "GO111MODULE" "auto")

;; Open from mac
(setenv "LANG" "en_US.UTF-8")

;; Use venv for lsp bridge
;; (setq lsp-bridge-python-command "/home/user/.emacs.d/.venv/bin/python3")

;;; Forge
;; (setq fate/forge-alist '(("git.site.com" "git.site.com/api/v4" "git.site.com" forge-github-repository)))

;;; Theme
;; (setq fate/theme 'doom-tokyo-night)

(provide 'custom)
;;; custom.el ends here
