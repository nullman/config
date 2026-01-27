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

(let ((data '(("Color" "Name" "Symbol" "Hex Code") ("Adwaita Dark Background (Original)" "" "" "#29353b") ("Adwaita Dark Background (Darker)" "" "" "#19252b") ("Adwaita Dark Background (Darkest)" "" "color-background" "#09151b") ("White Foreground" "" "color-foreground" "#bbc2cf") ("White Foreground Accent" "" "" "#798188") ("Yellow Cursor" "" "color-cursor" "#eeee22") ("Bright Yellow Highlight" "" "color-paren" "#ffff33") ("White Mouse" "" "color-mouse" "#ffffff") ("Outline Level 1" "pale yellow" "color-1" "#eeffaa") ("Outline Level 2" "light salmon" "color-2" "#ffbbaa") ("Outline Level 3" "sky blue" "color-3" "#aaeeff") ("Outline Level 4" "light green" "color-4" "#bbffaa") ("Outline Level 5" "navajo white" "color-5" "#ffeebb") ("Outline Level 6" "plum" "color-6" "#ffaacc") ("Outline Level 7" "pale turquoise" "color-7" "#bbffee") ("Outline Level 8" "pale green" "color-8" "#99ffbb"))))
;;------------------------------------------------------------------------------
;;; Constants: Colors
;;------------------------------------------------------------------------------

(mapc (lambda (x)
        (let ((color (caddr x))
              (value (cadddr x)))
          (when (> (length color) 0)
            (set (intern color) value))))
      (cdr data))
)

;; disable default emacs package handling (needed for straight)
(setq package-enable-at-startup nil)

;; do not resize frame to match font size
(setq frame-inhibit-implied-resize t)

;; do not round frame resizes to match font size
(setq frame-resize-pixelwise t)

;; prevent color flash when starting
(setq mode-line-format nil)
(set-face-attribute 'default nil :background `,color-background :foreground `,color-foreground)

;; suppress display compilation warnings
(setq warning-suppress-types '((comp)))

;;==============================================================================
;;; early-init.el ends here
;;==============================================================================
