;;; org-visibility-test.el --- Test org-visibility.el -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 2021 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2021-07-17
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'org-visibility)

;; (defun wrap-assert (test &optional clean-up)
;;   (let ((debug-on-error nil))
;;     (unwind-protect
;;         (condition-case err
;;             (funcall test)
;;           ('error
;;            (message (format "Assert error: %s" err))))
;;       (when clean-up (funcall clean-up)))))

;; (defun wrap-assert (test &optional clean-up)
;;   (condition-case err
;;       (funcall test)
;;     ('error
;;      (message (format "Assert error: %s" err))
;;      (when clean-up (funcall clean-up))))
;;   (when clean-up (funcall clean-up)))

(defun run-test (test)
  "Setup test environment, run TEST, then restore environment."
  (let ((org-startup-folded 'showeverything)
        (org-odd-levels-only t)
        (state-file org-visibility-state-file)
        (state-file-max-count org-visibility-state-file-max-count)
        (state-file-longevity org-visibility-state-file-longevity)
        (paths org-visibility-paths)
        (temp-file (make-temp-file "org-visibility-test-state-file-")))
    (setq org-visibility-state-file temp-file
          org-visibility-state-file-max-count 0
          org-visibility-state-file-longevity 0
          org-visibility-paths '())
    (ignore-errors
      (funcall test))
    (setq org-visibility-state-file state-file
          org-visibility-state-file-max-count state-file-max-count
          org-visibility-state-file-longevity state-file-longevity
          org-visibility-paths paths)
    (delete-file temp-file)))

(defun create-org-file (&optional with-local-var)
  "Create temporary `org-mode' file to test with."
  (let ((file (make-temp-file "org-visibility-test-" nil ".org")))
    (with-temp-file file
      (insert "* Heading 1")
      (newline)
      (insert "*** Heading 1.2")
      (newline)
      (insert "Body text 1.2")
      (newline)
      (insert "And some more")
      (newline)
      (insert "* Heading 2")
      (newline)
      (insert "*** Heading 2.1")
      (newline)
      (insert "***** Heading 2.1.1")
      (newline)
      (insert "Body text 2.1.1")
      (newline)
      (insert "*** Heading 2.2")
      (newline)
      (insert "Body text 2.2")
      (newline)
      (insert "* Heading 3")
      (newline)
      (insert "Body text 3")
      (newline)
      (when with-local-var
        (newline)
        ;; concat is used to prevent emacs from trying to set local variables on this file
        (insert (concat ";; Local " "Variables:"))
        (newline)
        (insert ";; org-visibility: t")
        (newline)
        (insert ";; End:")
        (newline)))
    file))

(defun check-visible-lines (lines)
  "Test that all LINES are visible, and no others, in current
buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (if (member (line-number-at-pos) lines)
          (assert (not (invisible-p (point))) nil (concat "Line not visible: " line))
        (assert (invisible-p (point)) nil (concat "Line visible: " line)))
      (forward-line 1))))

(defun test-no-persistence ()
  "Test no visibility persistence."
  (let ((file (create-org-file)))
    (run-test
     (lambda ()
       (find-file file)
       (wrap-assert
           (lambda ()
             (org-global-cycle)
             (forward-line 3)
             (org-cycle)
             (save-buffer)
             (kill-buffer (current-buffer))
             (find-file file)
             (check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11)))
           (lambda ()
             (kill-buffer (current-buffer))
             (delete-file file)))))))

(defun test-persistence ()
  "Test general visibility persistence."
  (let ((file (create-org-file :with-local-var)))
    (run-test
     (lambda ()
       (with-temp-file file
         (check-visible-lines '(1 4 10))
         (goto-char (point-min))
         (forward-line 3)
         (org-cycle))
       (with-temp-file file
         (check-visible-lines '(1 4 5 8 10)))))
    (delete-file file)))

;;; org-visibility-test.el ends here
