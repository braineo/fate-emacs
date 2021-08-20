;;; fate-custom.el --- initialize custom values before starting up  -*- no-byte-compile: t -*-


;;; Commentary:
;;; This file is used for custom setting in different environment, private settings should not go to main repo
;;; For executable path/versions you can find following examples on how to setup.
;;; Alternatively you can use `purcell/exec-path-from-shell`, but it is very slow on Mac.

;;; Code:

;; Font
(when window-system
  (when (and (x-list-fonts "Operator Mono"))
    (let ((font "Operator Mono:weight=light:style=book:pixelsize=12"))
      ;; (set-frame-font font)
      (add-to-list 'default-frame-alist `(font . ,font))))

  (when (x-list-fonts "Noto Sans")
    (set-fontset-font t 'han "Noto Sans CJK SC Regular")
    (set-fontset-font t 'kana "Noto Sans CJK JP Regular")
    (set-fontset-font t 'cjk-misc "Noto Sans CJK Sc Regular")))


;; Fix path issue or executable not found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))
;; (setq exec-path (append '("/usr/local/bin") exec-path))

;; GO related env vars
;; (setenv "GOPATH" "/home/user/.go")
;; (setenv "GO111MODULE" "auto")

;; Open from mac
(setenv "LANG" "en_US.UTF-8")


;;; Forge
;; (setq fate/forge-alist '(("git.site.com" "git.site.com/api/v4" "ssh://git@git.site.com" forge-github-repository)))

;;; fate-custom.el ends here
