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
;;; Bootstrap: Normal Tangle
;;------------------------------------------------------------------------------

(require 'ob-tangle)
(setq vc-follow-symlinks t)

;; generate (if needed) and load main init file
(let* ((file-base (concat (file-name-directory user-init-file) "init-emacs"))
       (file-org (file-truename (concat file-base ".org")))
       (file-elisp (file-truename (concat file-base ".el")))
       (file-comp (file-truename (concat file-base ".elc"))))
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
