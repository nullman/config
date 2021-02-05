;;; replacer.el --- Replace Strings as You Type
;;
;;; Copyright (C) 2019 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2019-02-12
;; Version:  1.0
;; Keywords: replace things typing helper abbreviations
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
;; Minor mode for as-you-type replacements.
;;
;;; Installation:
;;
;; Put `replacer.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'replacer)
;;
;; Set the key sequences that start and end replacements:
;;
;;   (setq replacer-trigger-start ";") ;; defaults to semicolon
;;   (setq replacer-trigger-end " ")   ;; defaults to space
;;
;; Define the trigger sequences and the replacements (strings or functions):
;;
;;   (setq replacer-replacements
;;         '(("a" . "abc")
;;           ("d" . dired)))
;;
;; Turn the mode on globally:
;;
;;   (replacer-mode 1)
;;
;;; Usage:
;;
;; Once the above is setup and the mode is enabled, just type the trigger
;; start key(s) followed by one of the defined replacement keys followed by
;; the trigger end key(s).  For example (using the above setup and "_"
;; denoting a space):
;;
;;   ;a_ -> "abc"
;;   ;d_ -> (dired)

;;; Code:

;; customize group
(defgroup replacer nil
  "Replace strings as you type."
  :prefix "replacer-"
  :group 'tools)

;; replacer trigger start
(defcustom replacer-trigger-start
  ";"
  "String that triggers the start of a replacer key sequence."
  :type 'string
  :group 'replacer)

;; replacer trigger end
(defcustom replacer-trigger-end
  " "
  "String that triggers the end of a replacer key sequence."
  :type 'string
  :group 'replacer)

;; replacer replacements
(defcustom replacer-replacements
  '()
  "List of pairs of trigger strings mapped to replacements.
Replacements can be strings, variables, or functions/lambdas."
  :type 'list
  :group 'replacer)

(defvar replacer-state
  nil
  "Internal buffer local variable to hold state.
Used to track the starting point of a possible replacer string.")
(make-variable-buffer-local 'replacer-state)

;; replacer function
(defun replacer-post-self-insert-hook-function ()
  "When `replacer-mode' is active, this function is called by
`post-self-insert-hook' after every key press to look for
possible replacements."
  (when (and (eq (char-before) last-command-event)
             (not executing-kbd-macro)
             (not noninteractive))
    (let ((trigger-start-len (length replacer-trigger-start))
          (trigger-end-len (length replacer-trigger-end))
          (point (point)))
      (cond
       ;; check for start trigger and set state
       ((and (> point trigger-start-len)
             (string= replacer-trigger-start
                      (buffer-substring-no-properties (- point trigger-start-len)
                                                      point)))
        (setq replacer-state (1- point)))
       ;; check for end trigger, clear state, and look for replacement match
       ((and (> point (+ trigger-start-len trigger-end-len 1))
             replacer-state
             (string= replacer-trigger-end
                      (buffer-substring-no-properties (- point trigger-end-len)
                                                      point)))
        (let* ((key (buffer-substring-no-properties (+ replacer-state trigger-start-len)
                                                    (- point trigger-end-len)))
               (replacement (cdr (assoc key replacer-replacements))))
          (when replacement
            (delete-region replacer-state point)
            (if (functionp replacement)
                (funcall replacement)
              (insert replacement)))
          (setq replacer-state nil)))))))

;; add company-mode backend for auto-completion of replacer keys
(when (fboundp 'company-mode)
  (defun company-replacer-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive
       (company-begin-backend 'company-replacer-backend))
      (prefix
       (when (looking-back (regexp-quote replacer-trigger-start)
                           (line-beginning-position))
         (match-string 0)))
      (candidates
       (cl-remove-if-not (lambda (x) (string-prefix-p arg (car x)))
                         replacer-replacements))
      (annotation
       (format " (%s)" (cdr (assoc arg replacer-replacements))))))
  (add-to-list 'company-backends 'company-replacer-backend))

;; ;; add company-mode backend for auto-completion of replacer keys
;; (when (fboundp 'company-mode)
;;   (defun company-replacer--make-candidate (candidate)
;;     (let ((text (car candidate))
;;           (meta (cadr candidate)))
;;       (propertize text 'meta meta)))

;;   (defun company-replacer--candidates (prefix)
;;     (let (res)
;;       (dolist (item replacer-replacements)
;;         (when (string-prefix-p prefix (car item))
;;           (push (company-replacer--make-candidate item) res)))
;;       res))

;;   (defun company-replacer--meta (candidate)
;;     (format "This will use %s of %s"
;;             (get-text-property 0 'meta candidate)
;;             (substring-no-properties candidate)))

;;   (defun company-replacer--annotation (candidate)
;;     (format " (%s)" (get-text-property 0 'meta candidate)))

;;   ;; replacer-mode
;;   (defun company-replacer-backend (command &optional arg &rest ignored)
;;     (interactive (list 'interactive))
;;     (cl-case command
;;       (interactive (company-begin-backend 'company-replacer-backend))
;;       (prefix (company-grab-symbol-cons
;;                replacer-trigger-start
;;                (length replacer-trigger-start)))
;;       (candidates (company-replacer--candidates arg))
;;       (annotation (company-replacer--annotation arg))
;;       (meta (company-replacer--meta arg))))

;;   (add-to-list 'company-backends 'company-replacer-backend))

;; replacer-mode
;;;###autoload
(define-minor-mode replacer-mode
  "Minor mode for as-you-type string replacements.

When called interactively, toggle `replacer-mode'.  With prefix
ARG, enable `replacer-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `replacer-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `replacer-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'replacer
  :global t
  (cond
   (replacer-mode
    (add-hook 'post-self-insert-hook #'replacer-post-self-insert-hook-function))
   (t
    (remove-hook 'post-self-insert-hook #'replacer-post-self-insert-hook-function))))

(provide 'replacer)

;;; replacer.el ends here
