;;; timer.el --- Countdown Timers
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2013-07-11
;; Version:  1.0
;; Keywords: timer timers countdown
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Provides `timer' function that takes a region or list of things to time.
;;
;;; Installation:
;;
;; Put `timer.el' where you keep your elisp files and add something like the
;; following to your .emacs file:
;;
;;   (require 'timer)
;;
;;; Usage:
;;
;; Call `timer' with either a selected region or a list of things to time.
;;
;; Example:
;;
;;   (timer
;;    '("Something 1"
;;      "Somwthing 2"))

;;; Code:

;; buffer name
(defvar timer-buffer-name
  "*Timer*"
  "Buffer name to use for timers.")

;; default countdown timer (2 hours)
(defvar timer-default-countdown
  "Default countdown time in seconds."
  (* 2 60 60))

;; things
(defvar timer-things
  "List of things to time."
  nil)

;; timers
(defvar timer-timers
  "List of timers."
  nil)

;; reset timer
(defun timer-reset (things timers)
  "Reset timer under `point'."
  (setcar (nth (1- (line-number-at-pos)) timer-timers)
          (time-add (current-time) (seconds-to-time timer-default-countdown))))

;; timer
;;;###autoload
(defun timer (&optional things)
  "Interface for controlling countdown timers."
  (interactive)
  (setq timer-things (or things
                         (if (and transient-mark-mode mark-active)
                             (cl-remove-if (lambda (x) (zerop (length x)))
                                           (split-string (buffer-substring-no-properties (point) (mark)) "\n"))
                           "")))
  (setq timer-timers (make-list (length things) nil))
  ;; setup buffer
  (let ((buffer (generate-new-buffer timer-buffer-name)))
    (switch-to-buffer buffer)
    (local-set-key (kbd "<return>") 'timer-reset)
    ;; turn off undo
    (buffer-disable-undo buffer)
    ;; loop
    (while t
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; output timers
      (cl-do ((things timer-things (cdr things))
              (timers timer-timers (cdr timers)))
          ((not things))
        (let ((thing (car things))
              (timer (car timers)))
          (if timer
              (let* ((time-diff (time-subtract timer (current-time)))
                     (microsecs (third time-diff))
                     (total-seconds (+ (* (first time-diff) 65536) (second time-diff)))
                     (hours (floor (/ total-seconds 3600)))
                     (mins (floor (/ (- total-seconds (* hours 3600)) 60)))
                     (secs (- total-seconds (* hours 3600) (* mins 60))))
                (if (plusp total-seconds)
                    (insert (format "%s [%2d:%02d:%02d]" thing hours mins secs))
                  (insert (format "%s [--:--:--]" thing))))
            (insert (format "%s [--:--:--]" thing)))
          (newline)))
      (setq buffer-read-only t)
      ;; draw screen and wait 1 second
      (sit-for 1.0))))

(provide 'timer)

;;; Tests:

;; (timer '("Something 1" "Somwthing 2"))

;;; timer.el ends here
