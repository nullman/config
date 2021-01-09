;;; workout-tracker.el --- Workout Tracker
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-06-19
;; Version:  0.1
;; Keywords: workout track gym
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
;; Provides `workout-tracker' function that allows for the entry of workout
;; data (routine, weight, repetitions, etc.), with tracking over time.
;;
;;; Installation:
;;
;; Put `workout-tracker.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'workout-tracker)
;;
;;; Usage:
;;
;;
;; Example:
;;
;;   (workout-tracker ... ???

;;; Code:

;; widget
(require 'widget)

;; customize group
(defgroup workout-tracker nil
  "Workout tracker."
  :prefix "workout-tracker-"
  :group 'applications)

;; database
(defvar workout-tracker-data
  nil
  "Buffer local database of workout tracking data.")
(make-variable-buffer-local 'workout-tracker-data)

;; data file
(defvar workout-tracker-data-file
  nil
  "Buffer local data file (to store workout data).")
(make-variable-buffer-local 'workout-tracker-data-file)

;; data file extension
(defvar workout-tracker-data-file-extension
  ".wtd"
  "Data file extension, including period.")

;; data file extension match
(defvar workout-tracker-data-file-extension-match
  (concat
   (replace-regexp-in-string "\\." "\\\\\." workout-tracker-data-file-extension)
   "$")
  "Data file extension match.")

;; data dir
(defcustom workout-tracker-data-dir
  (expand-file-name "~/.workout-tracker")
  "Directory to store workout data files."
  :type 'directory
  :group 'workout-traker)

;; workout tracker buffer name prefix
(defvar workout-tracker-buffer-name-prefix
  "workout-tracker"
  "Buffer name (prefix) to use for workout tracker.")

;; select server buffer name
(defvar workout-tracker-select-server-buffer-name
  "*Workout-Tracker-Select-Server*"
  "Buffer name to use for select server menu.")

;; workout-tracker-buffers
(defvar workout-tracker-buffers
  nil
  "Association list of active workout tracker buffers and their files.
\nList is in the following format:
  ((FILE . BUFFER) ...)")

;; set data macro
(defmacro workout-tracker-set-data (data)
  "Set local `workout-tracker-data' to DATA."
  `(setq workout-tracker-data ,data))

;; file name macro
(defmacro workout-tracker-file-name (file)
  "Returns FILE sans directory and extension parts."
  `(file-name-sans-extension (file-name-nondirectory ,file)))

;; set data file macro
(defmacro workout-tracker-set-data-file (file)
  "Set local `workout-tracker-data-file' to FILE."
  `(setq workout-tracker-data-file ,file))

;; tracker buffer name
(defun workout-tracker-buffer-name (file)
  "Generate buffer name from `workout-tracker-buffer-name-prefix' and FILE."
  (concat workout-tracker-buffer-name-prefix ":" (workout-tracker-file-name file)))

;; set data file extension
(defun workout-tracker-set-data-file-extension (extension)
  "Set `workout-tracker-data-file-extension' and `workout-tracker-data-file-extension-match'."
  (setq workout-tracker-data-file-extension extension)
  (setq workout-tracker-data-file-extension-match
        (concat
         (replace-regexp-in-string "\\." "\\\\\." workout-tracker-data-file-extension)
         "$")))

;; switch to data file
;;;###autoload
(defun workout-tracker-switch-to-data-file (file)
  "Return buffer of and switches to an existing data FILE.
\nReturn nil if not found."
  (interactive "F")
  (let ((buffer (assoc file workout-tracker-buffers)))
    (when buffer
      (switch-to-buffer (cdr buffer))
      (cdr buffer))))

;; load data file
;;;###autoload
(defun workout-tracker-load-data-file (file)
  "Load data FILE."
  (interactive "F")
  (let ((file (expand-file-name file))
        data
        buffer)
    ;; attempt to switch to an existing data file buffer
    (unless (workout-tracker-switch-to-data-file file)
      (if (file-exists-p file)
          (progn
            (with-temp-buffer
              ;; load file into temp buffer
              (insert-file-contents file)
              ;; persist contents
              (setq data (buffer-substring-no-properties (point-min) (point-max))))
            ;; create workout tracker buffer
            ;;(setq buffer (generate-new-buffer (workout-tracker-buffer-name file)))
            (setq buffer (get-buffer-create (workout-tracker-buffer-name file)))
            (switch-to-buffer buffer)
            ;; set data file
            (workout-tracker-set-file file)
            ;; copy file contents into local database
            (workout-tracker-set-data (eval data)))
        (message "Data file not found: %s" file)))))

;; save data file
;;;###autoload
(defun workout-tracker-save-data-file (file)
  "Save data FILE."
  (interactive "F")
  ;; create directory if needed
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (let ((file (expand-file-name file)))
    (with-temp-buffer
      ;; set data file
      (workout-tracker-set-file file)
      ;; write database into temp buffer
      (when (and (boundp 'workout-tracker-database)
                 workout-tracker-database)
        (insert workout-tracker-database))
      ;; save workout-tracker-database contents to file
      (write-file file))))

;; create data file
;;;###autoload
(defun workout-tracker-create-data-file (file)
  "Create empty data FILE.
\nIf FILE already exists, user is prompted to overwrite."
  (interactive "F")
  ;; TODO: check for unsaved changes
  ;; attempt to switch to an existing data file buffer
  (unless (workout-tracker-switch-to-data-file file)
    (with-temp-buffer
      ;; set data file
      (workout-tracker-set-file file)
      ;; clear database
      (workout-tracker-set-data nil)
      ;; save empty data file
      (workout-tracker-save-data-file file))))

;; open data file
;;;###autoload
(defun workout-tracker-open-data-file (file)
  "Open data FILE."
  (interactive "F")
  ;; attempt to switch to an existing data file buffer
  (unless (workout-tracker-switch-to-data-file file)
    ;; load data file from disk
    (workout-tracker-load-data-file file)
    ;; TODO: open file in buffer
    )

;; list dir files
(defun workout-tracker-dir-files (dir &optional match)
  "List all files in directory DIR that match pattern MATCH."
  ;; expand dir to full path
  (setq dir (expand-file-name dir))
  ;; make sure dir is a directory
  ;; (unless (file-directory-p dir)
  ;;   (error (format "`%s' is not a directory" dir)))
  (when (file-directory-p dir)
    ;; set default match if none given
    (let ((match (or match ".*"))
          items                                       ; items list to populate
          (files (nreverse (directory-files dir t)))) ; files in dir
      ;; loop through files
      (dolist (file files)
        ;; is item accessable?
        (when (file-readable-p file)
          ;; branch on type of item
          (cond
           ;; ignore `.' and `..'
           ((string-match "^\\.\\.?$" (file-name-nondirectory file))
            t)
           ;; matching file or directory
           ((string-match match file)
            ;; only add files
            (unless (file-directory-p file)
              ;; file
              (let ((file-name (file-name-sans-extension (file-name-nondirectory file))))
                ;; add file
                (push (list
                       file-name
                       file)
                      items)))
            t))))
      items)))

;; workout tracker (main method)
;;;###autoload
(defun workout-tracker (&optional file)
  "Start workout tracking.
\nIf FILE is given, use that workout data file.
Otherwise, call `workout-tracker-select-data-file'."
  (interactive)
  (if file
      (workout-tracker-open-data-file file)
    (workout-tracker-select-data-file)))

;; select data file
;;;###autoload
(defvar widget-data-file)
(defun workout-tracker-select-data-file ()
  "Present a list of data files the user may select from."
  (interactive)
  ;; setup buffer
  ;;(setq buffer (generate-new-buffer workout-tracker-select-server-buffer-name))
  (when (get-buffer workout-tracker-select-server-buffer-name)
    (kill-buffer workout-tracker-select-server-buffer-name))
  (let ((buffer (get-buffer-create workout-tracker-select-server-buffer-name)))
    (set-buffer buffer)
    (kill-all-local-variables)
    ;; add header
    (widget-insert "Workout Tracker\n\n")
    (widget-insert "Select an existing data file or create a new one:\n\n")
    ;; add existing data file list
    (let ((files (workout-tracker-dir-files workout-tracker-data-dir workout-tracker-data-file-extension-match)))
      (dolist (file files)
        (widget-create 'push-button
                       :value (car file)
                       :notify (lambda (widget &rest ignore)
                                 (let* ((value (widget-value widget))
                                        ;; set file to dir + value + extension
                                        (file (expand-file-name (concat value workout-tracker-data-file-extension)
                                                                workout-tracker-data-dir)))
                                   (kill-buffer nil)
                                   ;; open data file
                                   (workout-tracker-open-data-file file))))
        (widget-insert "\n"))
      (widget-insert "\n"))
    ;; use local variable to store data file
    (make-local-variable 'widget-data-file)
    ;; add create option
    (widget-insert "Data File: ")
    (widget-create 'editable-field
                   :size 40
                   :notify (lambda (widget &rest ignore)
                             (let* ((value (widget-value widget))
                                    (len-value (length value))
                                    (len-ext (length workout-tracker-data-file-extension)))
                               ;; set file to dir + value + extension
                               (setq widget-data-file
                                     (expand-file-name (concat value workout-tracker-data-file-extension)
                                                       workout-tracker-data-dir))
                               ;; remove extension if given
                               (when (and (> len-value len-ext)
                                          (string= (substring value (- 0 len-ext))
                                                   workout-tracker-data-file-extension))
                                 (setq widget-data-file (substring file 0 (- 0 len-ext)))))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :value "Create"
                   :notify (lambda (widget &rest ignore)
                             (let ((file widget-data-file))
                               (if (file-exists-p file)
                                   (message "File already exists: %s" file)
                                 (progn
                                   (kill-buffer nil)
                                   ;; create new data file
                                   (workout-tracker-create-data-file file))))))
    (widget-insert "\n")
    ;; final setup
    (use-local-map widget-keymap)
    (widget-setup)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (widget-forward 1))))

(provide 'workout-tracker)

;;; workout-tracker.el ends here
