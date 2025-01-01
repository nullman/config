;; [[file:init-emacs.org::*Early Init][Early Init:1]]
;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;;==============================================================================
;;; early-init.el
;;
;;; Early Emacs Initialization File
;;
;; Author: Kyle W T Sherman
;;
;; This file was generated from init-emacs.org and should not be edited
;; manually.
;;
;; init-emacs.org => early-init.el
;;==============================================================================

;; disable default emacs package handling (needed for straight)
(setq package-enable-at-startup nil)

;; do not shrink frame to match font size
(setq frame-resize-pixelwise t)

;; suppress display compilation warnings
(setq warning-suppress-types '((comp)))

;;==============================================================================
;;; early-init.el ends here
;;==============================================================================
;; Early Init:1 ends here
