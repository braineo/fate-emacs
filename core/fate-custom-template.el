;;; fate-custom.el --- initialize custom values before starting up  -*- no-byte-compile: t -*-


;;; Commentary:


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

;;; fate-custom.el ends here
