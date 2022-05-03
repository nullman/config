;; [[file:init-emacs.org::*Init][Init:1]]
;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;;==============================================================================
;;; init.el
;;
;;; Bootstrap Emacs Initialization File
;;
;; Author: Kyle W T Sherman
;;
;; This file was generated from init-emacs.org and should not be edited
;; manually.
;;
;; init-emacs.org => init.el
;;==============================================================================

;;------------------------------------------------------------------------------
;;; Org/Babel Bootstrap: Custom Tangle
;;------------------------------------------------------------------------------

;; (defun org-babel-generate-elisp-file (file &optional byte-compile force)
;;   "Generate an emacs-lisp file from an org-babel FILE.

;; Additionally, byte compile the file if BYTE-COMPILE is
;; non-nil.

;; Process file even if timestamp is not newer than target if FORCE
;; is non-nil."
;;   (let* ((case-fold-search t)
;;          (file-base (file-truename (expand-file-name (file-name-sans-extension file))))
;;          (file-org (concat (file-name-base file) ".org"))
;;          (file-elisp (concat file-base ".el"))
;;          (file-comp (concat file-base ".elc"))
;;          (heading-regexp "^\*+ ")
;;          (heading-comment-regexp "^\*+ COMMENT ")
;;          (begin-regexp "^[ \t]*#\\+BEGIN_SRC emacs-lisp")
;;          (begin-tangle-regexp "^[ \t]*#\\+BEGIN_SRC .*:tangle ")
;;          (end-regexp "^[ \t]*#\\+END_SRC")
;;          (indent-regexp "^  "))
;;     ;; generate elisp file if needed
;;     (when (or force
;;               (not (file-exists-p file-elisp))
;;               (file-newer-than-file-p file-org file-elisp))
;;       (message "Writing %s..." file-elisp)
;;       (with-temp-file file-elisp
;;         (insert-file-contents file)
;;         (goto-char (point-min))
;;         (let (code
;;               headings-counts
;;               (level 1)
;;               (comment-level 0)
;;               (end-comment ""))
;;           (while (not (eobp))
;;             (cond
;;              ;; comment heading
;;              ((let ((case-fold-search nil))
;;                 (looking-at heading-comment-regexp))
;;               (setq level (/ (- (match-end 0) (line-beginning-position) 8) 2))
;;               (when (or (zerop comment-level)
;;                         (< level comment-level))
;;                 (setq comment-level level))
;;               (delete-region (line-beginning-position) (progn (forward-line 1) (point))))
;;              ;; normal heading
;;              ((looking-at heading-regexp)
;;               (setq level (/ (- (match-end 0) (line-beginning-position)) 2))
;;               (when (or (zerop comment-level)
;;                         (<= level comment-level))
;;                 (setq comment-level 0)
;;                 (if (assoc level headings-counts)
;;                     (setf (cdr (assoc level headings-counts))
;;                           (cons (buffer-substring-no-properties (match-end 0) (line-end-position)) 1))
;;                   (setq headings-counts (append headings-counts (list (cons level (cons "No heading" 1)))))))
;;               (delete-region (line-beginning-position) (progn (forward-line 1) (point))))
;;              ;; start of tangled source block
;;              ((and (looking-at begin-regexp)
;;                    (zerop comment-level)
;;                    (not (looking-at begin-tangle-regexp))) ; skip blocks with their own tangle directive
;;               (let* ((heading-count (cdr (assoc level headings-counts)))
;;                      (heading (car heading-count))
;;                      (count (cdr heading-count)))
;;                 (delete-region (line-beginning-position) (progn (forward-line 1) (point)))
;;                 (unless (bobp)
;;                   (newline))
;;                 (when (fboundp 'org-link-escape)
;;                   (insert (format ";; [[file:%s::*%s][%s:%s]]\n" file-org (org-link-escape heading) heading count))
;;                   (setq end-comment (format ";; %s:%s ends here\n" heading count))
;;                   (cl-incf (cddr (assoc level headings-counts))))
;;                 (setq code t)))
;;              ;; end of tangled source block
;;              ((and code
;;                    (looking-at end-regexp))
;;               (delete-region (line-beginning-position) (progn (forward-line 1) (point)))
;;               (insert end-comment)
;;               (setq code nil
;;                     end-comment ""))
;;              ;; inside tangled source block
;;              (code
;;               (when (looking-at indent-regexp)
;;                 (delete-char (if (boundp 'org-edit-src-content-indentation)
;;                                  org-edit-src-content-indentation
;;                                2)))
;;               (forward-line 1))
;;              ;; outside tangled source block
;;              (t
;;               (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))
;;           (time-stamp))
;;         (message "Wrote %s..." file-elisp)))
;;     ;; byte compile elisp file if needed
;;     (when (and byte-compile
;;                (or (not (file-exists-p file-comp))
;;                    (file-newer-than-file-p file-elisp file-comp)))
;;       (byte-compile-file file-elisp))))

;; ;; generate and load main init file
;; (let* ((file-base (file-truename (expand-file-name "~/.emacs.d/init-emacs")))
;;        (file-org (concat file-base ".org"))
;;        (file-elisp (concat file-base ".el"))
;;        (file-comp (concat file-base ".elc")))
;;   ;; do not try to byte compile the generated file as it will fail since our environment is not setup
;;   (org-babel-generate-elisp-file file-org)
;;   ;; delete any existing byte compiled init file to prevent an outdated version from loading
;;   (when (file-exists-p file-comp)
;;     (delete-file file-comp))
;;   (if (file-exists-p file-elisp)
;;       (load file-elisp)
;;     (message "Error loading %s" file-elisp)))

;;------------------------------------------------------------------------------
;;; Org/Babel Bootstrap: Normal Tangle
;;------------------------------------------------------------------------------

(require 'ob-tangle)
(setq vc-follow-symlinks t)

;; generate (if needed) and load main init file
(let* ((file-base (file-truename (expand-file-name "~/.emacs.d/init-emacs")))
       (file-org (concat file-base ".org"))
       (file-elisp (concat file-base ".el"))
       (file-comp (concat file-base ".elc")))
  ;; do not try to byte compile the generated file as it will fail since our environment is not setup
  (when (file-newer-than-file-p file-org file-elisp)
    (org-babel-tangle-file file-org))
  ;; delete any existing byte compiled init file to prevent an outdated version from loading
  (when (file-exists-p file-comp)
    (delete-file file-comp))
  (if (file-exists-p file-elisp)
      (load file-elisp)
    (message "Error loading %s" file-elisp)))

;;==============================================================================
;;; init.el ends here
;;==============================================================================
;; Init:1 ends here
