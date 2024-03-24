;;; number-perspective.el --- 0-9 perspective        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: tools, convenience

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

;; This idea is originally suggested at
;; <https://qiita.com/atsisy/items/1b7249bf3cd6046b5681>.

;;; Code:

(require 'perspective)

(defun num-persp-generate-perspective ()
  (interactive)
  (mapc
   (lambda (i)
     (persp-switch (int-to-string i))
     (let ((cbuf (current-buffer)))
       (switch-to-buffer "*Messages*") ; All persp include *Messages* buffer.
       (switch-to-buffer cbuf)))
   (number-sequence 0 9)))

(defun num-persp-local-switch-workspace (index)
  `(lambda ()
     (interactive)
     (persp-switch (int-to-string ,index))))

(defun num-persp-assign-keys ()
  (interactive)
  (mapc
   (lambda (i)
     (global-set-key (kbd (format "M-%d" i))
                     (num-persp-local-switch-workspace i)))
   (number-sequence 0 9)))

(provide 'number-perspective)
;;; number-perspective.el ends here
