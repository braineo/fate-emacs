;;; doom-snazzy-theme.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Binbin Ye

;; Author: Binbin Ye
;; Keywords: theme

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

;; theme inspired by snazzy

;;; Code:

(require 'doom-themes)

;;
(def-doom-theme doom-snazzy
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#282c34" nil       nil))
   (bg-alt     '("#21242b" nil       nil))
   (base0      '("#1B2229" "black"   "black"))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"))
   (base2      '("#202328" "#2e2e2e" "brightblack"))
   (base3      '("#23272e" "#262626" "brightblack"))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"))
   (base5      '("#5B6268" "#525252" "brightblack"))
   (base6      '("#73797e" "#6b6b6b" "brightblack"))
   (base7      '("#9ca0a4" "#979797" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"))
   (fg         '("#bbc2cf" "#bfbfbf" "brightwhite"))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"))

   (grey       base4)
   (red        '("#ff5c57" "#ff5c57" "red"))
   (orange     '("#da8548" "#dd8844" "brightred"))
   (green      '("#5af78e" "#5af78e" "green"))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"))
   (yellow     '("#f3f99d" "#f3f99d" "yellow"))
   (blue       '("#57c7ff" "#57c7ff" "brightblue"))
   (dark-blue  '("#2257A0" "#2257A0" "blue"))
   (magenta    '("#ff6ac1" "#ff6ac1" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"))
   (cyan       '("#9aedfe" "#9aedfe" "brightcyan"))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
     `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0)))
   (modeline-bg-l
     `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt)
   (mode-line-emphasis
    :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l)

   ;; Doom modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)))

(provide 'doom-snazzy-theme)
;;; doom-snazzy-theme.el ends here
