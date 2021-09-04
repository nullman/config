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
        (enable-local-variables :all)
        (enable-local-eval t)
        (state-file org-visibility-state-file)
        (state-file-max-count org-visibility-state-file-max-count)
        (state-file-longevity org-visibility-state-file-longevity)
        (paths org-visibility-paths)
        (temp-state-file (make-temp-file "org-visibility-test-state-file-"))
        errors)
    (setq org-visibility-state-file temp-state-file
          org-visibility-state-file-max-count 0
          org-visibility-state-file-longevity 0
          org-visibility-paths '())
    (unwind-protect
        (setq errors (funcall test temp-state-file))
      (progn
        (setq org-visibility-state-file state-file
              org-visibility-state-file-max-count state-file-max-count
              org-visibility-state-file-longevity state-file-longevity
              org-visibility-paths paths)
        (delete-file temp-state-file)))
    (assert (not errors) :show-args)))

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
        (insert ";; eval: (org-visibility-load)")
        (newline)
        (insert ";; End:")
        (newline)))
    file))

(defun check-visible-lines (lines)
  "Test that all LINES are visible, and no others, in current
buffer.

Return list of errors, or nil, if none."
  (let (errors)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
        (if (member (line-number-at-pos) lines)
            (unless (not (invisible-p (point)))
              (push (concat "Line not visible: " line) errors))
          (unless (invisible-p (point))
            (push (concat "Line visible: " line) errors)))
        (forward-line 1)))
    (nreverse errors)))

(defun test-no-persistence ()
  "Test no visibility persistence."
  (let (errors)
    (run-test
     (lambda (state-file)
       (let ((file (create-org-file)))
         (find-file file)
         (org-global-cycle)
         (forward-line 3)
         (org-cycle)
         (save-buffer)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12)) errors)
         (kill-buffer (current-buffer))
         (delete-file file))))
    (nreverse errors)))

(defun test-persistence ()
  "Test general visibility persistence."
  (let (errors)
    (run-test
     (lambda (state-file)
       (let ((file (create-org-file :with-local-var)))
         (find-file file)
         (org-global-cycle)
         (push (check-visible-lines '(1 5 11)) errors)
         (goto-char (point-min))
         (forward-line 4)
         (org-cycle)
         (push (check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (delete-file file))))
    (nreverse errors)))

;;; org-visibility-test.el ends here
