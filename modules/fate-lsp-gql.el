;;; fate-lsp-gql.el --- Graphql lsp client           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Binbin Ye

;; Author: Binbin Ye
;; Keywords: lsp

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

;; Support for running graphql lsp.  Support multiple server running at the same time when editing tsx/jsx.

;;; Code:

(eval-when-compile
  (require 'lsp-mode)
  (require 'lsp-completion))

(lsp-dependency 'graphql-language-service-cli
                '(:npm :package "graphql-language-service-cli"
                       :path "graphql-lsp"))


(add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))

;; (lsp-stdio-connection '("graphql-lsp" "server" "--configDir=/home/binbin/mujin/checkoutroot/app-scene-editor" "--method=stream"))

(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda()
                                            `(,(lsp-package-path 'graphql-language-service-cli)
                                               "server"
                                               "--method=stream")))
    :major-modes '(graphql-mode)
    :language-id "graphql"
    :server-id 'graphql-lsp
    :priority -1
    :add-on? t
    :multi-root t
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure
                           'graphql-language-service-cli
                           callback
                           error-callback))
    :activation-fn (lambda (filename &optional _)
                     (or (string-match-p (rx (one-or-more anything) "."
                                           (or "ts" "js" "jsx" "tsx" "html" "vue" "svelte" "graphql" "gql")eos)
                           filename)
                       (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode 'svelte-mode)
                         (not (string-match-p "\\.json\\'" filename)))))))


(provide 'fate-lsp-gql)
;;; fate-lsp-gql.el ends here
