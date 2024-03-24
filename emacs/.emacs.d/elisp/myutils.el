;;; myutils.el --- utility commands                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: abbrev, tools

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

;; Utility functions. No keybind is set here.

;;; Code:

(require 'cl-lib)

(defun change-alpha-value (value)
  "Set the alpha-value as `VALUE'."
  (interactive "nalpha value: ")
  (set-frame-parameter nil 'alpha value))

;;; Copy
(defun copy-whole-line (&optional arg)
  "Copy current line.
With prefix ARG, Copy that many lines starting from the current line.
If ARG is negative, kill backward.  Also kill the preceding newline.
This is meant to make \\[repeat] work well with negative arguments.
If ARG is zero, kill current line but exclude the trailing newline.

Just replace `kill-region' as `copy-region-as-kill' in the
function `kill-whole-line'.

This function was originaly suggested by akisute3 and publish at
their blog `http://d.hatena.ne.jp/akisute3/20120412/1334237294'."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'copy-region-as-kill)
    (kill-new "")
    (setq last-command 'copy-region-as-kill))
  (cond
   ((zerop arg)
    (save-excursion
      (copy-region-as-kill (point)
                           (progn (forward-visible-line 0) (point)))
      (copy-region-as-kill (point)
                           (progn (end-of-visible-line) (point)))))
   ((< arg 0)
    (save-excursion
      (copy-region-as-kill (point)
                           (progn (end-of-visible-line) (point)))
      (copy-region-as-kill (point)
                           (progn (forward-visible-line (1+ arg))
                                  (unless (bobp) (backward-char))
                                  (point)))))
   (t
    (save-excursion
      (copy-region-as-kill (point)
                           (progn (forward-visible-line 0) (point)))
      (copy-region-as-kill (point)
                           (progn (forward-visible-line arg) (point))))))
  (message (substring (car kill-ring-yank-pointer) 0 -1)))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing match for `REGEXP'.

Second and third arg `RSTART' and `REND' specify the region to operate on.
Lines partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil `INTERACTIVE' argument.

The two pragrph above is the copy form `flush-lines'.
See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)

This was originally published at
`http://www.emacswiki.org/emacs-en/KillMatchingLines'"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))


;;; Window
(defun split-window-vertically-n (num_wins)
  "Split the current frame vertically into `NUM_WINS'."
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

(defun split-window-horizontally-n (num_wins)
  "Split the current frame horizontally into `NUM_WINS'."
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun other-window-or-split ()
  "If the current frame has only one window, split horizontaly."
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

(defun other-window-or-split-2 ()
  "Split the current window into two or three siede-by-side windows.
If the frame column is less then 270, split into two windows,
else split three.

This kind of function originally suggested by rubikitch
and posted at
`https://rubikitch.hatenadiary.org/entry/20100210/emacs'.
Later, shiayu36 changed a little and publish the function
at `http://shibayu36.hatenablog.com/entry/2012/12/18/161455'"
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

(defun other-window-or-split-or-close (arg)
  "Split, move or close window depend on the situations.
When the number of windows is one, split it into two side-by-side
windows.  When two or more, select another window in cyclic
ordering of windows.

If ARG is 4, by \\[universal-argument], select another window in
inverse cyclic order.  If ARG is 16, by \\[universal-argument] \\[universal-argument], delete the current window."
  (interactive "p")
  (cl-case arg
    (4  (other-window -1))
    (16 (delete-window))
    (t  (other-window-or-split))))

(defun swap-window-positions ()         ; Stephen Gildea
  "*Swap the positions of this window and the next one.

The function was published at
`https://www.emacswiki.org/emacs/TransposeWindows'"
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))


;;; pass the region to specific interpreter
(defun perl-on-region (beg end)
  "Process the text between `BEG' and `END' with perl.
The result is shown in *Perl Output*

The function wsa originally suggested at
`http://ryo1miya.hatenablog.com/entry/20121108/1352390277'"
  (interactive "r")
  (let ((buf "*Perl Output*")
        (curwin (selected-window)))
    (shell-command-on-region beg end "perl" buf)
    (pop-to-buffer buf)
    (select-window curwin)))

(defun ruby-on-region (beg end)
  "Process the text between `BEG' and `END' with ruby.
The result is shown in *Ruby Output*

The function wsa originally suggested at
`http://ryo1miya.hatenablog.com/entry/20121108/1352390277'"
  (interactive "r")
  (let ((buf "*Ruby Output*")
        (curwin (selected-window)))
    (shell-command-on-region beg end "ruby" buf)
    (pop-to-buffer buf)
    (select-window curwin)))

(defun python-on-region (beg end)
"Process the text between `BEG' and `END' with python.
The result is shown in *Python Output*

The function wsa originally suggested at
`http://ryo1miya.hatenablog.com/entry/20121108/1352390277'"
  (interactive "r")
  (let ((buf "*Python Output*")
        (curwin (selected-window)))
    (shell-command-on-region beg end "python" buf)
    (pop-to-buffer buf)
    (select-window curwin)))

;;; keymacro
(defvar kmacro-save-file "~/.emacs.d/user-kmacro.el")
(defun kmacro-save (symbol)
  "Save keyboard macro as `SYMBOL' name.

The macro is saved in `kmacro-save-file'.  Therefore if you load
it you can use it like a lisp function.

Probably `emacros.el' at
`https://github.com/CsBigDataHub/emacros' is better choice now."
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)
  (with-current-buffer (find-file-noselect kmacro-save-file)
    (goto-char (point-max))
    (insert-kbd-macro symbol)
    (basic-save-buffer)))

;;; keymap
(defvar overriding-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `overriding-minor-mode'.")

(define-minor-mode overriding-minor-mode
  "The minor mode for force keybinding.

I use it to conflict keybind in org-mode etc."
  :init-value t
  :lighter ""
  :keymap overriding-minor-mode-map
  )

;;; font
(defun check-font-here ()
  "カーソルの位置のフォントを確認する."
  (interactive)
  (princ (font-xlfd-name (font-at (point)))))

(defun print-all-available-font-familes ()
  "使用可能なfont-familyをすべて表示する."
  (interactive)
  (let ((buf (get-buffer-create "*Font Familes*"))
        (cbuf (current-buffer)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      ;; (print (font-family-list) buf)
      (dolist (x (font-family-list))
        (progn  (print x buf)
                (delete-region (- (point-max) 1) (point-max)))))
    (pop-to-buffer buf)
    (delete-region (point-min) (+ (point-min) 1))
    ;; (goto-char (point-max))
    (switch-to-buffer-other-window cbuf)))

(defun print-all-available-fontsets ()
  "使用可能なfont-setをすべて表示する."
  (interactive)
  (let ((buf (get-buffer-create "*Fontsets"))
        (cbuf (current-buffer)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      ;; (print (font-family-list) buf)
      (dolist (x (x-list-fonts "*"))
        (progn  (print x buf)
                (delete-region (- (point-max) 1) (point-max)))))
    (pop-to-buffer buf)
    (delete-region (point-min) (+ (point-min) 1))
    ;; (goto-char (point-max))
    (switch-to-buffer-other-window cbuf)))

(provide 'myutils)
;;; myutils.el ends here
