;;; auto-save-recentf.el --- auto save rencent files

;; Copyright (C) 2012  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'recentf)

(defvar auto-save-recentf-interval 30)
(defvar recentf-auto-save-timer nil)

(defun auto-save-recentf-new-timer (variable timer)
  "Give VARIABLE value to TIMER and cancel old timer.
This is the expanded form of `anaything-new-timer'."
  (let
      ((it
        (symbol-value variable)))
    (if it
        (cancel-timer it)))
  (set variable timer))

(defun enable-auto-save-recentf ()
  "Enable the timer for auto-save-recentf."
  (interactive)
  (auto-save-recentf-new-timer 'recentf-auto-save-timer
                      (run-with-idle-timer auto-save-recentf-interval t 'recentf-save-list)))

(defun disable-auto-save-recentf ()
  "Disable the timer for auto-save-recenrf."
  (interactive)
  (auto-save-recentf-new-timer 'recentf-auto-save-timer nil))

(defun toggle-auto-save-recentf ()
  "Toggle the timer for auto-save-recentf."
  (interactive)
  (if recentf-auto-save-timer
      (disable-auto-save-recentf)
    (enable-auto-save-recentf)))

(provide 'auto-save-recentf)
;;; auto-save-recentf.el ends here
