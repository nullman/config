;;; epoch.el --- Epoch Time Conversion
;;
;;; Copyright (C) 2009 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2009-11-17
;; Version:  1.0
;; Keywords: epoch time calendar
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
;; Provides `time-to-epoch', `epoch-to-time', and `epoch' functions to help
;; converting to and from time formats.
;;
;;; Installation:
;;
;; Put `epoch.el' where you keep your elisp files and add something like the
;; following to your .emacs file:
;;
;;   (require 'epoch)
;;
;;; Usage:
;;
;; Start interactive converter:
;;
;;   (epoch)
;;
;; Converting dates to epochs:
;;
;;   (time-to-epoch "2010-01-01")
;;   (time-to-epoch "2010-01-01 00:00")
;;   (time-to-epoch "2010-01-01 00:00:00")
;;
;; Converting epochs to dates:
;;
;;   (epoch-to-time 1262304000.0)
;;   (epoch-to-time "1262304000")
;;
;; Note: `epoch-to-time' requires either a float or a string due to Emacs'
;; inability to handle integers larger than `most-positive-fixnum'.

;;; Code:

;; widget
(require 'widget)

;; use UTC timezone
(set-time-zone-rule "UTC")

;; customize group
(defgroup epoch nil
  "Epoch Time Conversion."
  :prefix "epoch-"
  :group 'external
  :group 'applications)

;; custom truncate
;; the elisp `truncate' command throws a `range-error' when numbers are too large
;; this is a custom version of truncate that handles larger numbers
(defun truncate (arg &optional divisor)
  "Truncate a floating point number to an int.
Rounds ARG toward zero.
With optional DIVISOR, truncate ARG/DIVISOR."
  (when divisor
    (setq arg (/ arg divisor)))
  (string-to-number (replace-regexp-in-string "\\..*$" "" (number-to-string arg))))

;; float to string
(defun float-to-string (num)
  "Return string version of float NUM.
\nIf NUM ends with '.0', that part is removed."
  (replace-regexp-in-string "\\.0$" "" (number-to-string num)))

;; time to epoch conversion
;;;###autoload
(defun time-to-epoch (&optional time)
  "Return epoch (seconds since 1970-01-01) from TIME (or current time if nil)."
  (interactive)
  (unless time
    (setq time (format-time-string "%Y-%m-%d %H:%M:%S" nil t)))
  (when (< (length time) 11)
    (setq time (concat time " 00:00:00")))
  (truncate (float-time (date-to-time time))))

;; epoch to time conversion
;;;###autoload
(defun epoch-to-time (&optional epoch)
  "Return time from EPOCH (or current time if nil)."
  (interactive)
  (unless epoch
    (setq epoch (truncate (float-time (current-time)))))
  (when (stringp epoch)
    (setq epoch (string-to-number epoch)))
  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time epoch) t))

;; parse time
(defun epoch-parse-time (time &optional current-time)
  "Return epoch time from parsing TIME.

Supported types:

  Internal: (HIGH LOW . IGNORED)
  ISO: YYYY-MM-DD HH:MM:SS.SSS
  Decoded: (SECONDS MINUTES HOUR DAY MONTH YEAR DOW DST ZONE)
  Epoch: SECONDS
  Historic: -SECONDS

For decoded type any values may be nil and will be replaced with
those from the current time.

If CURRENT-TIME is non-nil, it is used in place of
`current-time'.  This allows for making multiple calls using a
consistent time."
  (if current-time
      (setq current-time (seconds-to-time (epoch-parse-time current-time)))
    (setq current-time (current-time)))
  (cond
   ;; if time is a list...
   ((listp time)
    (cond
     ;; if length <= 4, assume time value
     ((<= (length time) 4)
      (truncate (float-time time)))
     ;; if length 9, assume decoded time value
     ((= (length time) 9)
      (let ((time
             (cl-do ((time time (cdr time))
                     (current-time (decode-time current-time) (cdr current-time))
                     result)
                 ((not time) (nreverse result))
               (push (or (car time) (car current-time)) result))))
        (truncate (float-time (apply #'encode-time time)))))
     (t nil)))
   ;; if time is a number...
   ((numberp time)
    (if (> time 0)
        ;; if time is positive, assume epoch seconds
        time
      ;; else assume seconds before current time
      (+ (truncate (float-time current-time)) time)))
   ;; if time is a string, assume date string
   ((stringp time)
    (truncate (float-time (date-to-time time))))
   (t nil)))

;; parse times
(defun epoch-parse-times (times)
  "Return a list of Emacs times from parsing TIMES.
\nSupported types are listed in the `epoch-parse-time' function
definition."
  (remove nil (mapcar 'epoch-parse-time times)))

;; ;; initial times examples
;; (defcustom epoch-initial-time-examples
;;   `,(let* ((time (current-time))
;;            (time-hour (time-add time (seconds-to-time (* -1 60 60))))
;;            (time-morning (apply 'encode-time (append '(0 0 9) (cdddr (decode-time time)))))
;;            (time-day (time-add time (seconds-to-time (* -1 60 60 24))))
;;            (time-day-morning (apply 'encode-time (append '(0 0 9) (cdddr (decode-time (time-add time (seconds-to-time (* -1 60 60 24))))))))
;;            (time-week-morning (apply 'encode-time (append '(0 0 9) (cdddr (decode-time (time-add time (seconds-to-time (* -1 60 60 24 7))))))))
;;            (time-week2-morning (apply 'encode-time (append '(0 0 9) (cdddr (decode-time (time-add time (seconds-to-time (* -1 60 60 24 14))))))))
;;            (time-week4-morning (apply 'encode-time (append '(0 0 9) (cdddr (decode-time (time-add time (seconds-to-time (* -1 60 60 24 28)))))))))
;;       (epoch-parse-times
;;        (list time time-hour time-morning time-day time-day-morning
;;             time-week-morning time-week2-morning time-week4-morning)))
;;   "Initial list of times to query."
;;   :type 'list
;;   :group 'epoch)

;; initial times examples
(defcustom epoch-initial-time-examples
  `((0 (nil nil nil nil nil nil nil nil nil))
    (,(* -1 60 60) (nil nil nil nil nil nil nil nil nil))
    (0 (0 0 9 nil nil nil nil nil nil))
    (,(* -1 60 60 24) (nil nil nil nil nil nil nil nil nil))
    (,(* -1 60 60 24 7) (0 0 9 nil nil nil nil nil nil))
    (,(* -1 60 60 24 14) (0 0 9 nil nil nil nil nil nil))
    (,(* -1 60 60 24 28) (0 0 9 nil nil nil nil nil nil)))
  "Initial list of times to query.

List of tuples that are used for a two phase parse.  The first
element of the tuple is used to call `epoch-parse-time' using a
fixed value for CURRENT-TIME based on `current-time'.  The result
of that call is then passed in as CURRENT-TIME for a second call
to `epoch-parse-time', using the second value in the tuple.  This
allows for time addition as well as time filtering.

Supported types:

  Internal: (HIGH LOW . IGNORED)
  ISO: YYYY-MM-DD HH:MM:SS.SSS
  Decoded: (SECONDS MINUTES HOUR DAY MONTH YEAR DOW DST ZONE)
  Epoch: SECONDS
  Historic: -SECONDS

For decoded type any values may be nil and will be replaced with
those from the current time."
  :type 'list
  :group 'epoch)

;; times examples
(defvar epoch-time-examples
  nil
  "Examples of `time-to-epoch' and `epoch-to-time' times queried.
\nInitialized with `epoch-time-examples-reset' function.")

;; reset time examples
(defun epoch-time-examples-reset ()
  "Reset `epoch-time-examples' to initial values."
  (let ((time (current-time))
        examples)
    (dolist (entry epoch-initial-time-examples)
      (push
       (epoch-parse-time (cadr entry)
                         (epoch-parse-time (car entry) time))
       examples))
    (setq epoch-time-examples (nreverse examples))))

;; initialize examples
(epoch-time-examples-reset)

;; epoch buffer name
(defvar epoch-buffer-name
  "*Epoch Time Conversion*"
  "Buffer name to use for epoch interface.")

;; suppress compiler warnings for free variables
(defvar widget-time-epoch)
(defvar widget-time-string)

;; epoch interface
;;;###autoload
(defun epoch (&optional times epoch)
  "Interactive epoch-to-time and time-to-epoch converter interface.
\nIf TIMES is non-nil, it adds them to the examples list."
  (interactive)
  ;; make sure times is a list
  (when (and times (not (listp times)))
    (setq times (list times)))
  (let (buffer                           ; menu buffer
        time-examples                    ; times examples
        all-times                        ; all times
        last-time)                       ; last time
    ;; setup buffer
    (when (get-buffer epoch-buffer-name)
      (kill-buffer epoch-buffer-name))
    (setq buffer (get-buffer-create epoch-buffer-name))
    (set-buffer buffer)
    ;; (setq buffer-read-only nil)
    ;; (erase-buffer)
    (kill-all-local-variables)
    ;; local variable storage
    (make-local-variable 'widget-time-epoch)
    (make-local-variable 'widget-time-string)
    ;; create unique examples list and all times list
    (when times
      (dolist (time (epoch-parse-times times))
        (pushnew time time-examples :test '=)))
    (dolist (times epoch-time-examples)
      (pushnew times time-examples :test '=))
    (setq time-examples (nreverse time-examples))
    (setq all-times (sort all-times '<))
    ;; last time
    (setq last-time (or (car times) (car epoch-time-examples) (truncate (float-time (current-time)))))
    ;; add header
    (widget-insert (concat (propertize "Epoch Time Conversion" 'face 'font-lock-keyword-face) "\n\n"))
    ;; free form entry
    (widget-insert (propertize "Epoch: " 'face 'font-lock-keyword-face))
    (setq widget-time-epoch
          (widget-create
           'editable-field
           :value (float-to-string last-time)
           :size 10
           :notify (lambda (widget &rest ignore)
                     (let* ((value (widget-value widget))
                            (time-string (ignore-errors (epoch-to-time (string-to-number value)))))
                       (when time-string
                         (save-excursion
                           (widget-value-set widget-time-string time-string)))))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :value "+"
                   :notify `(lambda (&rest ignore)
                              (let ((time-epoch (string-to-number (widget-value widget-time-epoch))))
                                (delete time-epoch epoch-time-examples)
                                (push time-epoch epoch-time-examples)
                                (kill-buffer nil)
                                (epoch ,times))))
    (widget-insert "\n\n")
    (widget-insert (propertize "Time:  " 'face 'font-lock-keyword-face))
    (setq widget-time-string
          (widget-create
           'editable-field
           :value (epoch-to-time last-time)
           :size 19
           :notify (lambda (widget &rest ignore)
                     (let* ((value (widget-value widget))
                            (time-epoch (ignore-errors (float-to-string (time-to-epoch value)))))
                       (when time-epoch
                         (save-excursion
                           (widget-value-set widget-time-epoch time-epoch)))))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :value "+"
                   :notify `(lambda (&rest ignore)
                              (let ((time-epoch (string-to-number (widget-value widget-time-epoch))))
                                (delete time-epoch epoch-time-examples)
                                (push time-epoch epoch-time-examples)
                                (kill-buffer nil)
                                (epoch ,times))))
    (widget-insert "\n\n")
    ;; times selection
    (widget-insert (concat (propertize "Times:" 'face 'font-lock-keyword-face) "\n\n"))
    ;; add recent examples list
    (cl-do ((time-examples time-examples (cdr time-examples))
            (cnt 1 (1+ cnt)))
        ((not time-examples))
      (let ((time-epoch (car time-examples))
            (time-string (epoch-to-time (car time-examples))))
      (widget-insert "  ")
      (widget-insert (float-to-string time-epoch))
      (widget-insert "  ")
      (widget-insert time-string)
      (when (member time-epoch epoch-time-examples)
        (widget-insert "  ")
        (widget-create 'push-button
                       :value "-"
                       :notify `(lambda (&rest ignore)
                                  (setq epoch-time-examples (remove ,time-epoch epoch-time-examples))
                                  (kill-buffer nil)
                                  (epoch ,times)))))
      (widget-insert "\n"))
    (widget-insert "\n")
    ;; add configure button
    (widget-create 'push-button
                   :value "Configure"
                   :notify (lambda (&rest ignore)
                             (kill-buffer nil)
                             (customize-group "epoch")))
    (widget-insert "  ")
    ;; add example reset button
    (widget-create 'push-button
                   :value "Reset Times"
                   :notify `(lambda (&rest ignore)
                              (kill-buffer nil)
                              (epoch-time-examples-reset)
                              (epoch ,times)))
    (widget-insert "\n")
    ;; final setup
    (use-local-map widget-keymap)
    (widget-setup)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (widget-forward 1)))

(provide 'epoch)

;;; epoch.el ends here
