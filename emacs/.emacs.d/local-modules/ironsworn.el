;;; ironsworn.el --- Ironsworn RPG assistant -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 2022 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2022-03-30
;; Version:  1.0
;; Keywords: game
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
;; sub-headline.  Then call `ironsworn-create-character' to generate
;; a new character sheet template.
;;
;; When point is somewhere on a character sheet, call
;; `ironsworn-character-assistant' to start the interactive character
;; assistant.

;;; Code:

;; widget
(require 'widget)

;; character list
(defun ironsworn-character-list (&optional buffer)
  "Return a list of character names and positions in BUFFER.

If BUFFER is nil, current buffer is used."
  (cl-labels
      ((headline-data ()
                      (let ((headline (org-element-headline-parser (point) :raw-secondary-p)))
                        (cons
                         (org-element-property :level headline)
                         (cons (org-element-property :raw-value headline)
                               (point))))))
    (with-current-buffer (or buffer (current-buffer))
      (save-mark-and-excursion
        (let ((title (headline-data)))
          (while (and (not (string= title "Ironsworn"))
                      (org-get-next-sibling))
            (setq title (headline-data)))
          (when (string= title "Ironsworn")
            (org-next-visible-heading)
            (setq title (headline-data))
            (while (and (not (string= title "Characters"))
                        (org-get-next-sibling))
              (setq title (headline-data)))


            ))))))

      ;; (org-element-map (org-element-parse-buffer) 'headline
      ;;   (lambda (x) (cons (org-element-parse-buffer :level x)
      ;;                     (org-element-parse-buffer :raw-value x)))))))

;; character information
(defun ironsworn-character-info (&optional name buffer)
  "Return character information of character at current position,
or character NAME, if given."
  (let ((tree (list
               (cons
                (org-element-property :raw-value (org-element-headline-parser (point)))
                (point))))
        characters)
    (save-mark-and-excursion
      (while (org-up-heading-safe)
        (push
         (cons
          (org-element-property :raw-value (org-element-headline-parser (point)))
          (point))
         tree)))
    (unless (and (string= (caar tree) "Ironsworn")
                 (string= (caadr tree) "Characters"))
      (error "Expected 'Ironsworn' -> 'Characters' healines not found"))
    (setq tree (seq-subseq tree 0 3))
    ;; (save-mark-and-excursion
    ;;   (when (> (length outline-path) 3)
    ;;     (while (> (org-up-heading-safe) 2)))
    ;; (when (> (length tree) 2)
    ;;     (goto-char (cdadr tree))
    ;;     (org-next-visible-heading)
    ;;     (org-map-entries
    ;;      (lambda () (org-element-property :title (org-element-headline-parser (point) :raw-secondary-p)))
    ;;      nil
    ;;      'tree)))
    tree
    ))


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
        (pushnew time time-examples :test '=)))
    (dolist (times ironsworn-time-examples)
      (pushnew times time-examples :test '=))
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
    (widget-insert (propertize "Time:  " 'face 'font-lock-keyword-face))
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
