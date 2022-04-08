;;; split-move.el --- Split based buffer movement
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-01-30
;; Version:  1.0
;; Keywords: split move chop
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; `split-move' moves up or down a buffer in diminishing chunks to hone in on
;; a location. It it similiar to what `chop' does, but it works on buffers
;; instead of windows and it does not require `cl'.
;;
;; The `chop' module that `split-move' is based on was written by Luke Gorrie
;; <luke@bluetail.com> and can be found here:
;;
;; http://fresh.homeunix.net/%7Eluke/misc/emacs/chop.el
;;
;;; Installation:
;;
;; Put `split-move.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'split-move)
;;   (when (load "split-move" t)
;;     (global-set-key (kbd "M-<up>") 'split-move-up)
;;     (global-set-key (kbd "M-<down>") 'split-move-down))
;;
;; Here I've mapped the `meta-up' and the `meta-down' arrow keys to use
;; `split-move'.

;;; Code:

(defvar split-move-size
  nil
  "Number of lines for last move.")

;;;###autoload
(defun split-move-up ()
  "Move up half the remaining distance of previous split move."
  (interactive)
  (split-move -1))

;;;###autoload
(defun split-move-down ()
  "Move down half the remaining distance of previous split move."
  (interactive)
  (split-move 1))

(defun split-move (direction)
  "Move `split-move-size' lines in DIRECTION.

DIRECTION is 1 for down and -1 for up.

If this is the first call `split-move-size' is initialized to
half the distance from the current location to the beginning or
end of the buffer."
  (setq this-command 'split-move)
  (if (split-move-p)
      ;; repeated call
      (progn
        (setq split-move-size (/ split-move-size 2))
        (forward-line (* split-move-size direction)))
    ;; first call
    (progn
      (if (eql direction 1)
          (setq split-move-size (/ (- (line-number-at-pos (point-max))
                                      (line-number-at-pos)) 2))
        (setq split-move-size (/ (line-number-at-pos) 2)))
      (forward-line (* split-move-size direction)))))

(defun split-move-p ()
  "Return non-nil if previous command was the same as COMMAND."
  (and (not current-prefix-arg)
       (eq last-command 'split-move)))

(provide 'split-move)

;;; split-move.el ends here
