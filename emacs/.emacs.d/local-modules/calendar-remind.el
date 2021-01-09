;;; calendar-remind.el --- Remind in Calendar
;;
;;; Copyright (C) 2006,2007 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2007-11-09
;; Version:  1.0
;; Keywords: calendar remind
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
;; calendar-remind integrates the remind program into the Emacs calendar.
;;
;; When focus is on a calendar day, `calendar-remind-lookup' will renender the
;; results of running `remind' for that day. `calendar-remind-visit' will
;; visit the remind source file.  `calendar-remind-visit-insert' will visit
;; the remind source file and insert a template entry for the day.  These
;; functions should be mapped to keys as explained below.
;;
;;; Installation:
;;
;; Put `calendar-remind.el' where you keep your elisp files and add something
;; like the following to your .emacs file:
;;
;;   (require 'calendar-remind)
;;   (when (load "calendar-remind" t)
;;     (defun local-calendar-remind-load-hook ()
;;       (define-key calendar-mode-map "\r" 'calendar-remind-lookup)
;;       (define-key calendar-mode-map "r" 'calendar-remind-lookup)
;;       (define-key calendar-mode-map "v" 'calendar-remind-visit)
;;       (define-key calendar-mode-map "V" 'calendar-remind-visit-insert))
;;     (add-hook 'calendar-load-hook 'local-calendar-remind-load-hook))
;;
;; I've mapped the `return' key and the `r' key to remind lookup, the `v' key
;; to visit the `~/.reminders' file, and the `V' key to visit and insert a new
;; entry at the end of the `~/.reminders' file.  Change the key mappings to
;; what ever you would like to use.
;;
;; Can be customized with the following command:
;;
;;   (customize-group "remind")

;;; Code:

;; customize group
(defgroup remind nil
  "Calendar remind extensions."
  :prefix "calendar-remind-"
  :group 'calendar)

;; reminders file name
(defcustom calendar-remind-reminders-file-name
  (expand-file-name "~/.reminders")
  "Main reminders file name to use."
  :type 'file
  :group 'remind)

;; remind buffer name
(defcustom calendar-remind-buffer-name
  "*Remind*"
  "Buffer name to use for remind output."
  :type 'string
  :group 'remind)

;; remind print output buffer name
(defconst calendar-remind-print-buffer-name
  "*calendar-remind-print-buffer*"
  "Buffer name to use for temporary calendar remind print output.")

;; remind text property
(defconst calendar-remind-text-property
  "remind-source"
  "Name of text property to store file name and line number.")

;; regular expression to match a blank line
(defconst calendar-remind-blank-regexp
  "^$"
  "Regular expression that matches a blank line.")

;; regular expression to match a fileinfo line
(defconst calendar-remind-fileinfo-regexp
  "^# fileinfo "
  "Regular expression that matches a fileinfo line.")

;; regular expression to match a fileinfo line
(defun calendar-remind-date-regexp (date)
  "Regular expression that matches a dated remind line."
  (concat "^" date " [^ ]+ [^ ]+ [^ ]+ [^ ]+ "))

;; remind mode map
(defvar calendar-remind-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'calendar-remind-visit-position)
    (define-key map "v" 'calendar-remind-visit-position)
    (define-key map "r" 'calendar-remind-lookup)
    map))

;; remind mode
(defun calendar-remind-mode ()
  "Major mode for displaying calendar reminders.
\n\\{calendar-remind-mode-map}"
  ;;(interactive)
  (kill-all-local-variables)
  (setq major-mode 'calendar-remind-mode)
  (setq mode-name "Reminders")
  (use-local-map calendar-remind-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'calendar-remind-mode-hook))

;; remind date
;;;###autoload
(defun calendar-remind-date (&optional day mon year)
  "Return the given date in various formats.

If any date parameters are nil, the selected calendar date is
used to populate them.

Return an association list containing the following elements:

  ((:numeric (D M YYYY))
   (:standard \"YYYY-MM-DD\")
   (:internal \"YYYY/MM/DD\")
   (:string (\"D\" \"Mon\" \"2000\"))
   (:remind \"Mon D YYYY\"))

Example:

  ((:numeric (1 1 2000))
   (:standard \"2000-01-01\")
   (:internal \"2000/01/01\")
   (:string (\"1\" \"Jan\" \"2000\"))
   (:remind \"Jan 1 2000\"))"
  (interactive)

  (let ((month-map '((1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May")
                     (6 "Jun") (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct")
                     (11 "Nov") (12 "Dec"))) ; month mappings
        dates)                          ; list of dates in various formats

    ;; switch to calendar buffer if needed
    (save-excursion
      (save-current-buffer
        (when (not (equal (buffer-name) calendar-buffer))
          (when (not (bufferp (get-buffer calendar-buffer)))
            (calendar))
          (set-buffer (get-buffer calendar-buffer)))
        ;; set date parameters if not given
        (unless day
          (setq day (extract-calendar-day (calendar-cursor-to-date))))
        (unless mon
          (setq mon (extract-calendar-month (calendar-cursor-to-date))))
        (unless year
          (setq year (extract-calendar-year (calendar-cursor-to-date))))))

    ;; add numeric format (D M YYYY)
    (push (list :numeric (list day mon year)) dates)

    ;; add standard format "YYYY-MM-DD"
    (push (list :standard (format "%04d-%02d-%02d" year mon day)) dates)

    ;; add internal format "YYYY/MM/DD"
    (push (list :internal (format "%04d/%02d/%02d" year mon day)) dates)

    ;; convert date values to strings
    (when (numberp day)
      (setq day (number-to-string day)))
    (when (numberp mon)
      (setq mon (cadr (assq mon month-map))))
    (when (numberp year)
      (setq year (number-to-string year)))

    ;; add string format ("D" "Mon" "YYYY")
    (push (list :string (list day mon year)) dates)

    ;; add remind format "Mon D YYYY"
    (push (list :remind (format "%s %s %s" mon day year)) dates)

    ;; return dates
    (nreverse dates)))

;; remind print
;;;###autoload
(defun calendar-remind-print (&optional day mon year)
  "Execute the `remind' program and print the results into the current buffer.
\nUse function `calendar-remind-date' to determine the date for
any parameters not given."
  (interactive "*")

  (let (buffer                          ; remind output buffer
        reminders                       ; reminders list
        dates)                          ; dates associaton list

    ;; get dates
    (setq dates (calendar-remind-date day mon year))
    (setq day (first (cadr (assoc :string dates)))
          mon (second (cadr (assoc :string dates)))
          year (third (cadr (assoc :string dates))))

    ;; make sure cursor is at the start of a new line
    (when (not (point-at-bol))
      (goto-char (point-at-eol))
      (newline))

    ;; ;; call remind to put header into current buffer
    ;; (call-process "remind" nil t nil
    ;;               (expand-file-name calendar-remind-reminders-file-name)
    ;;               (cadr (assoc :remind dates)) "| head -n 1")
    ;; (newline)

    ;; add header
    (insert (concat "Reminders for " (cadr (assoc :standard dates)) ":"))
    (newline)
    (newline)

    ;; create temporary buffer to hold reminders
    (setq buffer (generate-new-buffer calendar-remind-print-buffer-name))
    (save-current-buffer
      (set-buffer buffer)
      ;; call remind and put output into buffer
      (call-process "remind" nil t nil "-s+1l"
                    (expand-file-name calendar-remind-reminders-file-name)
                    day mon year)
      ;; copy buffer contents into a string
      (setq reminders (buffer-string))
      ;; delete temporary buffer
      (kill-buffer buffer))

    ;; convert reminders string to a list
    (setq reminders (split-string reminders "\n"))

    ;; loop through reminder lines reading data
    (cl-loop while reminders
             for fi = (split-string (pop reminders))
             for rem = (pop reminders)
             if (and (equal (cadr fi) "fileinfo")
                     (equal (substring rem 0 10) (cadr (assoc :internal dates))))
             do (let ((beg (point)))
                  ;; insert reminder
                  (insert rem)
                  ;; save position
                  (save-excursion
                    ;; remove header
                    (goto-char beg)
                    (search-forward-regexp (calendar-remind-date-regexp (cadr (assoc :internal dates))))
                    (kill-region beg (point)))
                  ;; add property that links to source
                  (add-text-properties beg (point)
                                       (list calendar-remind-text-property
                                             (list (fourth fi) (third fi))))
                  (newline)))))

;; remind lookup
;;;###autoload
(defun calendar-remind-lookup (&optional day mon year)
  "Execute the `remind' program and output the results.
\nUse function `calendar-remind-print' to output the remind information.
Use variable `calendar-remind-buffer-name' for the buffer name."
  (interactive)

  (let (dates                           ; dates associaton list
        cal)                            ; calendar window has focus

    ;; get dates
    (setq dates (calendar-remind-date day mon year))
    (setq day (first (cadr (assoc :numeric dates)))
          mon (second (cadr (assoc :numeric dates)))
          year (third (cadr (assoc :numeric dates))))

    ;; check for calendar having focus
    (when (equal (buffer-name) calendar-buffer)
      (setq cal t))

    ;; setup calendar-remind-buffer-name buffer
    (get-buffer-create calendar-remind-buffer-name)
    (set-buffer calendar-remind-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)

    ;; call remind print function
    (calendar-remind-print day mon year)

    ;; set remind buffer to read-only
    (setq buffer-read-only t)

    ;; more setup
    (when cal
      (other-window 1))
    (switch-to-buffer calendar-remind-buffer-name)
    (goto-char (point-min))
    (calendar-remind-mode)
    (forward-line 2)
    (when cal
      (other-window 1))))

;; remind visit
;;;###autoload
(defun calendar-remind-visit (&optional file-name)
  "Edit file FILE-NAME using function `find-file'.
\nFILE-NAME is the file to edit (defaults to variable
`calendar-remind-reminders-file-name')."
  (interactive)
  ;; if current buffer is calendar buffer, switch to other window
  (when (equal (buffer-name) calendar-buffer)
    (other-window 1))
  ;; visit file
  (if file-name
      (find-file file-name)
    (find-file calendar-remind-reminders-file-name))
  ;; goto end of buffer
  (goto-char (point-max)))

;; remind visit and insert
;;;###autoload
(defun calendar-remind-visit-insert (&optional file-name)
  "Call function `calendar-remind-visit' then insert a new entry with the selected date."
  (interactive)
  (let ((dates (calendar-remind-date))) ; calendar dates
    ;; call calendar-remind-visit
    (calendar-remind-visit file-name)
    ;; insert reminder text
    (insert "REM " (cadr (assoc :remind dates)) " AT : DURATION : MSG ")
    (newline)
    (forward-line -1)))

;; remind visit position
;;;###autoload
(defun calendar-remind-visit-position ()
  "Edit file stored in `remind-source' property at current position.
\nUse on an entry in `calendar-remind-buffer-name' buffer."
  (interactive)
  ;; check that current buffer is remind buffer
  (when (equal (buffer-name) calendar-remind-buffer-name)
    ;; get properties
    (let ((file-name (first (get-text-property (point) calendar-remind-text-property)))
          (line (string-to-number (second (get-text-property (point) calendar-remind-text-property)))))
      ;; continue if properties look good
      (when (and (stringp file-name)
                 (file-exists-p file-name)
                 (integerp line))
        ;; visit file
        (find-file file-name)
        ;; goto line
        (goto-char (point-min))
        (forward-line (1- line))))))

;; ;; calendar hook
;; ;; define default keys if not already defined
;; (when (not (fboundp 'calendar-remind-load-hook))
;;   (defun calendar-remind-load-hook ()
;;     ;; remind lookup
;;     (define-key calendar-mode-map "\r" 'calendar-remind-lookup)
;;     (define-key calendar-mode-map "r" 'calendar-remind-lookup)
;;     (define-key calendar-mode-map "v" 'calendar-remind-visit)
;;     (define-key calendar-mode-map "V" 'calendar-remind-visit-insert)))
;; ;; add hook
;; (add-hook 'calendar-load-hook 'calendar-remind-load-hook)

(provide 'calendar-remind)

;;; calendar-remind.el ends here
