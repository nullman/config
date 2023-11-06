;;; ironsworn.el --- Ironsworn RPG Assistant -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 2022 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2022-03-30
;; Version:  1.0
;; Keywords: game
;; Package-Requires: ((emacs "24.3"))
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
;; Provides `ironsworn-character-assistant', a widget based character helper
;; that uses an existing `org-mode' buffer to store its data.
;;
;;; Installation:
;;
;; Put `ironsworn.el' where you keep your elisp files and add something like the
;; following to your .emacs file:
;;
;;   (require 'ironsworn)
;;
;; Or
;;
;;   (use-package ironsworn)
;;
;;; Usage:
;;
;; Create an org file with an "Ironsworn" headline and a "Characters"
;; sub-headline. Then call `ironsworn-create-character' to generate a new
;; character sheet template.
;;
;; When point is somewhere on a character sheet, call
;; `ironsworn-character-assistant' to start the interactive character
;; assistant.

;;; Code:

;; requires
(require 'cl-lib)
(require 'widget)

;; customize group
(defgroup ironsworn nil
  "Ironsworn RPG Assistant."
  :prefix "ironsworn-"
  :group 'games)

;; level 1 title
(defcustom ironsworn-title-level-1
  "Ironsworn"
  "First level headline title."
  :type 'string
  :group 'ironsworn)

;; level 2 title
(defcustom ironsworn-title-level-2
  "Characters"
  "Second level headline title."
  :type 'string
  :group 'ironsworn)

;; character list
(defun ironsworn-character-list (&optional name buffer)
  "Return a list of character names and positions in BUFFER.

If NAME is non-nil, return only characters having that name.
If BUFFER is nil, current buffer is used."
  (cl-labels
      ((headline-data ()
                      (let ((headline (org-element-headline-parser (point))))
                        (list
                         :title (org-element-property :raw-value headline)
                         :level (org-element-property :level headline)
                         :point-marker (point-marker)))))
    (with-current-buffer (or buffer (current-buffer))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((title-l1 "Ironsworn")
              (title-l2 "Characters")
              (data (headline-data))
              characters)
          (while (and (not (and (string= (plist-get data :title) ironsworn-title-level-1)
                                (= (plist-get data :level) 1)))
                      (org-get-next-sibling))
            (setq data (headline-data)))
          (when (string= (plist-get data :title) ironsworn-title-level-1)
            (outline-next-heading)
            (setq data (headline-data))
            (while (and (not (and (string= (plist-get data :title) ironsworn-title-level-2)
                                  (= (plist-get data :level) 2)))
                        (org-get-next-sibling))
              (setq data (headline-data)))
            (when (string= (plist-get data :title) ironsworn-title-level-2)
              (outline-next-heading)
              (setq data (headline-data))
              (while (and data (= (plist-get data :level) 3))
                (unless (and name (not (string= (plist-get data :title) name)))
                  (push (cons (plist-get data :title)
                              (plist-get data :point-marker)) characters))
                (if (org-get-next-sibling)
                    (setq data (headline-data))
                  (setq data nil)))))
          (nreverse characters))))))

;; character information
(defun ironsworn-character-info (&optional name buffer)
  "Return Ironsworn character information of character at current position,
or character NAME, if given.

If BUFFER is nil, current buffer is used."
  (when (and buffer
             (not name)
             (not (string=
                   (if (stringp buffer) buffer (buffer-name buffer))
                   (buffer-name (current-buffer)))))
    (error "If BUFFER is given, either NAME must be given or BUFFER must be selected"))
  (cl-labels
      ((headline-data ()
                      (let ((headline (org-element-headline-parser (point))))
                        (list
                         :title (org-element-property :raw-value headline)
                         :level (org-element-property :level headline)
                         :point-marker (point-marker)))))
    (let ((characters (ironsworn-character-list name buffer))
          character
          character-data)
      (with-current-buffer (or buffer (current-buffer))
        (save-mark-and-excursion
          (if name
              (setq character (car characters))
            (dolist (x characters)
              (when (<= (cdr x) (point))
                (setq character x))))
          (when character
            (goto-char (cdr character))
            (org-map-tree
             (lambda ()
               (let ((data (headline-data)))
                 (push (cons (plist-get data :title)
                             (plist-get data :point-marker)) character-data))))
            (nreverse character-data)))))))

(defun ironsworn-character-assistant ()
  "Launch an Ironsworn character assistant.

Should be called from an `org-mode' buffer with this structure:

  * Ironsworn
  *** Characters
  ***** NAME 1
  ***** NAME 2
  ***** ...

If cursor is within the scope of a character, then that character
will be used. Otherwise, a list will be presented to pick from."
  (interactive "*")

  ;; make sure times is a list
  (when (and times (not (listp times)))
    (setq times (list times)))
  (let (buffer                           ; menu buffer
        time-examples                    ; times examples
        all-times                        ; all times
        last-time)                       ; last time
    ;; setup buffer
    (when (get-buffer ironsworn-buffer-name)
      (kill-buffer ironsworn-buffer-name))
    (setq buffer (get-buffer-create ironsworn-buffer-name))
    (set-buffer buffer)
    ;; (setq buffer-read-only nil)
    ;; (erase-buffer)
    (kill-all-local-variables)
    ;; local variable storage
    (make-local-variable 'widget-time-ironsworn)
    (make-local-variable 'widget-time-string)
    ;; create unique examples list and all times list
    (when times
      (dolist (time (ironsworn-parse-times times))
        (cl-pushnew time time-examples :test '=)))
    (dolist (times ironsworn-time-examples)
      (cl-pushnew times time-examples :test '=))
    (setq time-examples (nreverse time-examples))
    (setq all-times (sort all-times '<))
    ;; last time
    (setq last-time (or (car times) (car ironsworn-time-examples) (truncate (float-time (current-time)))))
    ;; add header
    (widget-insert (concat (propertize "Ironsworn Time Conversion" 'face 'font-lock-keyword-face) "\n\n"))
    ;; free form entry
    (widget-insert (propertize "Ironsworn: " 'face 'font-lock-keyword-face))
    (setq widget-time-ironsworn
          (widget-create
           'editable-field
           :value (float-to-string last-time)
           :size 10
           :notify (lambda (widget &rest ignore)
                     (let* ((value (widget-value widget))
                            (time-string (ignore-errors (ironsworn-to-time (string-to-number value)))))
                       (when time-string
                         (save-excursion
                           (widget-value-set widget-time-string time-string)))))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :value "+"
                   :notify `(lambda (&rest ignore)
                              (let ((time-ironsworn (string-to-number (widget-value widget-time-ironsworn))))
                                (delete time-ironsworn ironsworn-time-examples)
                                (push time-ironsworn ironsworn-time-examples)
                                (kill-buffer nil)
                                (ironsworn ,times))))
    (widget-insert "\n\n")
    (widget-insert (propertize "Time: " 'face 'font-lock-keyword-face))
    (setq widget-time-string
          (widget-create
           'editable-field
           :value (ironsworn-to-time last-time)
           :size 19
           :notify (lambda (widget &rest ignore)
                     (let* ((value (widget-value widget))
                            (time-ironsworn (ignore-errors (float-to-string (time-to-ironsworn value)))))
                       (when time-ironsworn
                         (save-excursion
                           (widget-value-set widget-time-ironsworn time-ironsworn)))))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :value "+"
                   :notify `(lambda (&rest ignore)
                              (let ((time-ironsworn (string-to-number (widget-value widget-time-ironsworn))))
                                (delete time-ironsworn ironsworn-time-examples)
                                (push time-ironsworn ironsworn-time-examples)
                                (kill-buffer nil)
                                (ironsworn ,times))))
    (widget-insert "\n\n")
    ;; times selection
    (widget-insert (concat (propertize "Times:" 'face 'font-lock-keyword-face) "\n\n"))
    ;; add recent examples list
    (cl-do ((time-examples time-examples (cdr time-examples))
            (cnt 1 (1+ cnt)))
        ((not time-examples))
      (let ((time-ironsworn (car time-examples))
            (time-string (ironsworn-to-time (car time-examples))))
      (widget-insert "  ")
      (widget-insert (float-to-string time-ironsworn))
      (widget-insert "  ")
      (widget-insert time-string)
      (when (member time-ironsworn ironsworn-time-examples)
        (widget-insert "  ")
        (widget-create 'push-button
                       :value "-"
                       :notify `(lambda (&rest ignore)
                                  (setq ironsworn-time-examples (remove ,time-ironsworn ironsworn-time-examples))
                                  (kill-buffer nil)
                                  (ironsworn ,times)))))
      (widget-insert "\n"))
    (widget-insert "\n")
    ;; add configure button
    (widget-create 'push-button
                   :value "Configure"
                   :notify (lambda (&rest ignore)
                             (kill-buffer nil)
                             (customize-group "ironsworn")))
    (widget-insert "  ")
    ;; add example reset button
    (widget-create 'push-button
                   :value "Reset Times"
                   :notify `(lambda (&rest ignore)
                              (kill-buffer nil)
                              (ironsworn-time-examples-reset)
                              (ironsworn ,times)))
    (widget-insert "\n")
    ;; final setup
    (use-local-map widget-keymap)
    (widget-setup)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (widget-forward 1)))

(provide 'ironsworn)

;;; ironsworn.el ends here
