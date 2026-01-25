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

(let ((data '(("Color" "Name" "Symbol" "Hex Code") ("Adwaita Dark Background (Original)" "" "" "#29353b") ("Adwaita Dark Background (Darker)" "" "" "#19252b") ("Adwaita Dark Background (Darkest)" "" "color-background" "#09151b") ("White Foreground" "" "color-foreground" "#bbc2cf") ("White Foreground Accent" "" "" "#798188") ("Yellow Cursor" "" "color-cursor" "#eeee22") ("Bright Yellow Highlight" "" "color-paren" "#ffff33") ("White Mouse" "" "color-mouse" "#ffffff") ("Outline Level 1" "light goldenrod" "color-1" "#eedd82") ("Outline Level 2" "light salmon" "color-2" "#ffa07a") ("Outline Level 3" "light green" "color-3" "#90ee90") ("Outline Level 4" "light sky blue" "color-4" "#87cefa") ("Outline Level 5" "tan" "color-5" "#d2b48c") ("Outline Level 6" "aquamarine" "color-6" "#7fffd4") ("Outline Level 7" "pale violet red" "color-7" "#db7093") ("Outline Level 8" "cadet blue" "color-8" "#5f9ea0"))))
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
