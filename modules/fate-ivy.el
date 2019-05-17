;;; fate-ivy.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Binbin Ye

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

;; 

;;; Code:

(use-package swiper
  :defer t
  :custom
  (swiper-action-recenter t)
  :bind (:map swiper-map
          ("M-%" . swiper-query-replace)
          :map isearch-mode-map
          ("M-i" . swiper-from-isearch)))

(provide 'fate-ivy)
;;; fate-ivy.el ends here
