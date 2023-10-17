;;; multi-task.el --- Multiple Task Library
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-02-12
;; Version:  1.0
;; Keywords: tools
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
;; Provides `multi-task' function that takes a list of commands, runs them
;; asynchronously, displays running statistics, and finally returns when the
;; commands have completed.
;;
;;; Installation:
;;
;; Put `multi-task.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'multi-task)
;;
;;; Usage:
;;
;; Call `multi-task' with a list of commands, each of which is a list containing:
;;
;;   NAME    is the name of the process.
;;   PROGRAM is the program to run.
;;   ARGS    is a list of string arguments to pass to PROGRAM.
;;   BUFFER  is a buffer name to direct the output."
;;
;; Example:
;;
;;   (multi-task
;;    '(("pwd test" "pwd" nil "pwd test")
;;      ("ls test" "ls" ("-l" "-a") "ls test")
;;      ("sleep test" "sleep" ("5s") "sleep test")))

;;; Code:

;; status buffer name
(defvar multi-task-status-buffer-name
  "*Multi-Task-Status*"
  "Buffer name to use for process status.")

;; start commands
(defun multi-task-start (commands)
  "Start COMMANDS asynchronously and return a process list.

COMMANDS is a list of commands containing:

  NAME    is the name of the process.
  PROGRAM is the program to run.
  ARGS    is a list of string arguments to pass to PROGRAM.
  BUFFER  is a buffer name to direct the output.

Returned list contains lists of processes and timestamps."
  (let (processes)
    (dolist (command commands)
      (let* ((name (car command))
             (program (cadr command))
             (args (caddr command))
             (buffer (cadddr command)))
        (push (list (eval `(start-process name buffer program ,@args)) (current-time)) processes)))
    (nreverse processes)))

;; multi task status
(defun multi-task-status (processes &optional times)
  "Print the status of PROCESSES and return non-nil if any are still running.

If optional parameter TIMES is non-nil, timing information will
be displayed and returned."
  (let ((done t))
    ;; initialize times
    (when (and times (not (listp times)))
      (setq times nil)
      (dolist (lst processes)
        (push (cons (process-name (car lst)) (list 0 0 0)) times)))
    (insert "Process Status")
    (newline)
    (newline)
    (insert "Process                       Command                       Status")
    (when times
      (insert "  Time"))
    (newline)
    (insert "----------------------------  ----------------------------  ------")
    (when times
      (insert "  ---------------"))
    (newline)
    ;; loop through processes
    (dolist (lst processes)
      (let* ((process (car lst))
             (time (cadr lst))
             (name (process-name process))
             (status (process-status process))
             time-diff
             command)
        (when times
          (setq time-diff (cdr (assoc name times))))
        ;; get process command as a string
        (dolist (x (process-command process))
          (if command
              (setq command (concat command " " x))
            (setq command x)))
        ;; if process is running
        (when (eq status 'run)
          (when times
            ;; calculate time difference
            (setq time-diff (time-subtract (current-time) time))
            ;; set time difference
            (setcdr (assoc name times) time-diff))
          ;; if any processes are still running, set to not done
          (setq done nil))
        ;; output process status
        (when (> (length name) 28)
          (setq name (substring name 0 28)))
        (when (> (length command) 28)
          (setq command (substring command 0 28)))
        (if times
            (let* ((microsecs (caddr time-diff))
                   (total-seconds (+ (* (car time-diff) 65536) (cadr time-diff)))
                   (hours (floor (/ total-seconds 3600)))
                   (mins (floor (/ (- total-seconds (* hours 3600)) 60)))
                   (secs (- total-seconds (* hours 3600) (* mins 60))))
              (setq time-diff (concat
                               (if (> hours 0)
                                   (format "%2d:%02d:%02d.%d" hours mins secs microsecs)
                                 (if (> mins 0)
                                     (format "   %2d:%02d.%d" mins secs microsecs)
                                   (format "      %2d.%d" secs microsecs)))))
              (insert (format "%-29s %-29s %-7S %s" process command status time-diff)))
          (insert (format "%-29s %-29s %S" process command status)))
        (newline)))
    ;; return times or t, or nil if done
    (if done
        nil
      (if times
          times
        t))))

;; multi task
;;;###autoload
(defun multi-task (commands &optional kill-buffers)
  "Start COMMANDS asynchronously, report running statistics,
and return when commands have completed.

COMMANDS is a list of commands containing:

  NAME    is the name of the process.
  PROGRAM is the program to run.
  ARGS    is a list of string arguments to pass to PROGRAM.
  BUFFER  is a buffer name to direct the output.

If optional parameter KILL-BUFFERS is non-nil (default) then the
command buffers are killed after they finish running."
  (interactive)
  (let (buffer                          ; status buffer
        processes                       ; process list
        (times t))                      ; timing information
    ;; start commands
    (setq processes (multi-task-start commands))
    ;; setup buffer
    (setq buffer (generate-new-buffer multi-task-status-buffer-name))
    (switch-to-buffer buffer)
    ;; turn off undo
    (buffer-disable-undo buffer)
    ;; loop until all processes have finished
    (let ((status t))
      (while status
        (setq buffer-read-only nil)
        (erase-buffer)
        ;; output status
        (setq status (multi-task-status processes times))
        (when status
          (setq times status))
        (setq buffer-read-only t)
        ;; draw screen and wait 0.1 second
        (sit-for 0.1)))
    ;; order or kill buffers
    (dolist (lst (reverse processes))
      (let* ((process (car lst))
             (buffer (process-buffer process)))
        (if kill-buffers
            (kill-buffer buffer)
            (switch-to-buffer buffer))))
    ;; delete status buffer
    ;;(kill-buffer buffer)
    ;; switch to status buffer
    (switch-to-buffer buffer)))

(provide 'multi-task)

;;; Tests:

;;(multi-task '(("pwd test" "pwd" nil "pwd test") ("ls test" "ls" ("-l" "-a" "/etc") "ls test") ("sleep test" "sleep" ("2s") "sleep test")) :kill-buffers)

;;; multi-task.el ends here
