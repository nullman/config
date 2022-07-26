;; [[file:../../../.emacs.d/init-emacs.org::#init-windows-init][Windows Init:1]]
;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;;==============================================================================
;;; init-windows.el
;;
;;; Bootstrap Emacs Initialization File for Windows
;;
;; Author: Kyle W T Sherman
;;
;; This file was generated from init-emacs.org and should not be edited
;; manually.
;;
;; init-emacs.org => init-windows.el
;;
;; On Windows machines copy this file to: %APPDATA%\init.el
;;==============================================================================

;;------------------------------------------------------------------------------
;;; Load Real Init File
;;------------------------------------------------------------------------------

(let ((home "C:\\msys64\\home\\kyle.sherman"))
  (setenv "HOME" home)
  (load (expand-file-name ".emacs.d\\init.el" home)))

;;==============================================================================
;;; init-windows.el ends here
;;==============================================================================
;; Windows Init:1 ends here
