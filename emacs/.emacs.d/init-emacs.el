;; [[file:init-emacs.org::*Colors][Colors:1]]
(let ((data '(("Color" "Name" "Symbol" "Hex Code") ("Adwaita Dark Background (Original)" "" "" "#29353b") ("Adwaita Dark Background (Darker)" "" "" "#19252b") ("Adwaita Dark Background (Darkest)" "" "color-background" "#09151b") ("White Foreground" "" "color-foreground" "#bbc2cf") ("White Foreground Accent" "" "" "#798188") ("Yellow Cursor" "" "color-cursor" "#eeee22") ("Bright Yellow Highlight" "" "color-paren" "#ffff33") ("White Mouse" "" "color-mouse" "#ffffff") ("Outline Level 1" "goldenrod" "color-1" "#daa520") ("Outline Level 2" "light goldenrod" "color-2" "#eedd82") ("Outline Level 3" "yellow green" "color-3" "#9acd32") ("Outline Level 4" "light salmon" "color-4" "#ffa07a") ("Outline Level 5" "tan" "color-5" "#d2b48c") ("Outline Level 6" "light green" "color-6" "#90ee90") ("Outline Level 7" "coral" "color-7" "#ff7f50") ("Outline Level 8" "wheat" "color-8" "#f5deb3"))))
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
;; Colors:1 ends here

;; [[file:init-emacs.org::*Start][Start:1]]
;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;;==============================================================================
;;; init-emacs.el
;;
;;; Main Emacs Settings File
;;
;; Author: Kyle W T Sherman
;;
;; This file was generated from init-emacs.org and should not be edited
;; manually.
;;
;; init-emacs.org => init-emacs.el
;;==============================================================================

;;==============================================================================
;;; Start
;;==============================================================================
;; Start:1 ends here

;; [[file:init-emacs.org::*Start][Start:2]]
(defun message--with-timestamp (format-string &rest args)
  "Add timestamps to `*Messages*' buffer."
  (when (and (> (length format-string) 0)
             (not (string= format-string " ")))
    (let ((deactivate-mark nil))
      (save-mark-and-excursion
        (with-current-buffer "*Messages*"
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (when (not (bolp)) (newline))
            (insert (format-time-string "[%T] " (current-time)))))))))
;; advise `message'
(advice-add 'message :before #'message--with-timestamp)

(defun init-message (level format-string &rest args)
  "Custom version of `message' to log messages during Emacs initialization.

LEVEL is the indentation level."
  (let ((file (file-name-sans-extension
               (file-name-nondirectory
                (or load-file-name buffer-file-name (buffer-name))))))
    (message (concat ";;; " file " "
                     (make-string (* 2 level) ?-)
                     "> " format-string) args)))

(init-message 1 "Start")

;; display load time after startup
(defun emacs-startup-hook--message-startup-time ()
  "Message the Emacs startup time and number of garbage collections."
  (message "Emacs startup time: %f seconds"
           (float-time (time-subtract after-init-time before-init-time)))
  (message "Emacs startup garbage collections: %d" gcs-done))
(add-hook 'emacs-startup-hook #'emacs-startup-hook--message-startup-time)
;; Start:2 ends here

;; [[file:init-emacs.org::*Start][Start:3]]
;; reduce frequency of garbage collections
(setq gc-cons-threshold (* 50 1000 1000)) ; default: 800000
;; Start:3 ends here

;; [[file:init-emacs.org::*Start][Start:4]]
;; generic advice wrapper function to ignore errors
(defun advice--ignore-errors (orig-fun &rest args)
  "Ignore errors when interactively calling ORIG-FUN with ARGS."
  (condition-case err
      (apply orig-fun args)
    ('error
     (if (called-interactively-p 'any)
         (message "%s" err)
       (error err)))))
;; Start:4 ends here

;; [[file:init-emacs.org::*Start][Start:5]]
;; lock-file wrapper macro to evaluate code blocks only once per emacs session
(defmacro when-lock-file-acquired (lock-file &rest body)
  "Evaluate BODY unless another running Emacs instance has done so.

LOCK-FILE is a file name to be used as a lock for this BODY code.

Skips checks if run on Windows."
  (declare (indent 1))
  (let ((procdir (gensym "procdir")))
    `(let ((,procdir (format "/proc/%d" (emacs-pid))))
       (unless (or (string= system-type "windows-nt")
                (file-exists-p ,lock-file))
         (make-symbolic-link ,procdir ,lock-file t))
       (when (or (string= system-type "windows-nt")
                 (file-equal-p ,lock-file ,procdir))
         ,@body))))
;; Start:5 ends here

;; [[file:init-emacs.org::*Package Manager][Package Manager:1]]
;;==============================================================================
;;; Package Manager
;;==============================================================================

(init-message 1 "Package Manager")
;; Package Manager:1 ends here

;; [[file:init-emacs.org::*Straight][Straight:1]]
;;------------------------------------------------------------------------------
;;; Package Manager: Straight
;;------------------------------------------------------------------------------

(init-message 2 "Package Manager: Straight")

;; initialize package system
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

;; bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use straight with `use-package'
(straight-use-package 'use-package)

;; turn off package file modification check at startup
(setq straight-check-for-modifications
      (remove 'find-at-startup straight-check-for-modifications))

;; update recipe repositories
;;(straight-pull-recipe-repositories)
;; Straight:1 ends here

;; [[file:init-emacs.org::*Environment Settings][Environment Settings:1]]
;;==============================================================================
;;; Environment Settings
;;==============================================================================

(init-message 1 "Environment Settings")
;; Environment Settings:1 ends here

;; [[file:init-emacs.org::*Modules][Modules:1]]
;; load modules that are used for initialization
(use-package async
  :straight t)
(use-package bind-key
  :straight t
  :init
  ;; extract docstrings from lambdas, closures and keymaps if possible
  (setq bind-key-describe-special-forms t))
(use-package cl-generic
  :straight (:type built-in))
(use-package cl-macs
  :straight (:type built-in))
(use-package dash
  :straight t
  :demand t
  :config
  (use-package dash-functional
    :straight t
    :demand t))
(use-package diminish
  :straight (diminish :type git :host github :repo "myrjola/diminish.el"))
(use-package f
  :straight t
  :demand t)
(use-package s
  :straight t
  :demand t)
(use-package seq
  :straight (:type built-in))
(use-package subr-x
  :straight (:type built-in))
(use-package org
  :straight (:type built-in))
(use-package org-table
  :straight (:type built-in))
(use-package ob-tangle
  :straight (:type built-in))
(use-package ox
  :straight (:type built-in))
;; Modules:1 ends here

;; [[file:init-emacs.org::*Environment][Environment:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: Environment
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: Environment")
;; Environment:1 ends here

;; [[file:init-emacs.org::*Environment][Environment:2]]
;; set coding system to UTF-8
(setq current-language-environment "UTF-8"
      locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Environment:2 ends here

;; [[file:init-emacs.org::*Environment][Environment:3]]
;; set timezone to CST
;;(setenv "TZ" "CDT+6")
(setenv "TZ" "America/Chicago")
;; Environment:3 ends here

;; [[file:init-emacs.org::*Environment][Environment:4]]
;; determine if running on a MS-Windows display
(defconst window-system-windows
  ;;(memq system-type '(emx win32 w32 mswindows ms-dos windows-nt))
  (string= window-system "w32")
  "Non-nil if running on a MS-Windows display.")
;; Environment:4 ends here

;; [[file:init-emacs.org::*Environment][Environment:5]]
;; determine if running on a macintosh gnustep or cocoa display
(defconst window-system-mac
  (string= window-system "ns")
  "Non-nil if running on a Macintosh GNUstep or Cocoa display.")
;; Environment:5 ends here

;; [[file:init-emacs.org::*Environment][Environment:6]]
;; determine if running on a Linux X display
(defconst window-system-linux
  (string= window-system "x")
  "Non-nil if running on a Linux X display.")
;; Environment:6 ends here

;; [[file:init-emacs.org::*Environment][Environment:7]]
;; determine if running on a work system
(defconst work-system
  (file-exists-p "~/.work")
  "Non-nil if running on a work system.")
;; Environment:7 ends here

;; [[file:init-emacs.org::*Environment][Environment:8]]
;; cd to home
(cd "~")
;; Environment:8 ends here

;; [[file:init-emacs.org::*Environment][Environment:9]]
;; shell environment
(setq shell-file-name (or (getenv "SHELL") "/bin/bash")
      shell-command-switch "-c"
      explicit-shell-file-name shell-file-name
      explicit-sh-args '("-login" "-i"))

;; quote arguments on windows
(when window-system-windows
  (defvar w32-quote-process-args ?\"
    "Windows quote arguments."))

;; add /usr/local to path on mac
(when window-system-mac
  (add-to-list 'exec-path "/usr/local/sbin")
  (add-to-list 'exec-path "/usr/local/bin"))
;; Environment:9 ends here

;; [[file:init-emacs.org::*Environment][Environment:10]]
;; set object print depth (do not abbreviate printed objects)
(setq print-length nil
      print-level nil
      eval-expression-print-length nil
      eval-expression-print-level nil)

;; ;; turn off print header
;; (setq ps-print-header nil)
;; Environment:10 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: Global Variables
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: Global Variables")
;; Global Variables:1 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:2]]
;; set emacs home directory
(defconst emacs-home-dir
  (file-truename (expand-file-name "~/.emacs.d"))
  "Emacs configuration home directory.")
;; Global Variables:2 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:3]]
(defmacro emacs-home-sub-dir (dir)
  "Return expanded directory name of DIR if found as a
sub-directory of `emacs-home-dir', or just `emacs-home-dir'
otherwise."
  `(let ((file (expand-file-name ,dir emacs-home-dir)))
     (if (file-exists-p file)
         (file-truename file)
       emacs-home-dir)))
;; Global Variables:3 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:4]]
;; set emacs modules directory
(defconst emacs-modules-dir
  (emacs-home-sub-dir "modules")
  "Emacs modules directory.")
;; Global Variables:4 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:6]]
;; set local modules directory
(defconst local-modules-dir
  (emacs-home-sub-dir "local-modules")
  "Emacs local modules directory.")
;; Global Variables:6 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:7]]
;; set local work modules directory
(defconst local-work-modules-dir
  (emacs-home-sub-dir "local-work-modules")
  "Emacs local work modules directory.")
;; Global Variables:7 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:8]]
;; set customization file
(defconst customization-file
  (file-truename (expand-file-name "customization.el" emacs-home-dir))
  "Emacs customization file.")
(setq custom-file customization-file)
;; Global Variables:8 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:9]]
;; set init-emacs.org true file name
(defconst init-emacs-true-file-name
  (file-truename (expand-file-name "init-emacs.org" emacs-home-dir))
  "The true file name of this buffer.")
;; Global Variables:9 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:10]]
;; set user name
(defconst user-name "kyle")
(defconst user-full-name "Kyle W T Sherman")
(defconst user-short-name "Kyle Sherman")
(defconst user-first-name "Kyle")
(defconst user-last-name "Sherman")
;; Global Variables:10 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:11]]
;; set email address
(defconst user-mail-address
  (if (getenv "EMAIL")
      (getenv "EMAIL")
    (concat "kyle" "w" "sherman" "@" "gmail" "." "com"))
  "User email address.")
;; Global Variables:11 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:12]]
;; set no-spam email address
(defconst user-mail-address-nospam
  (replace-regexp-in-string "\\." " dot "
                            (replace-regexp-in-string "@" " at " user-mail-address))
  "Slightly obfuscated user email address.")
;; Global Variables:12 ends here

;; [[file:init-emacs.org::*Global Variables][Global Variables:13]]
(defun signature (&optional fortune)
  "Return a signature.

A fortune is added if FORTUNE is non-nil."
  (let ((name (or user-short-name user-full-name))
        (mail user-mail-address))
    (let ((sig (if (and name mail)
                   (concat name " <" mail ">")
                 (if name
                     name
                   mail))))
      (concat (if sig sig "")
              (if (and sig fortune) "\n\n" "")
              (if fortune
                  (shell-command-to-string
                   (concat "fortune -a "
                           (shell-quote-argument (expand-file-name "~/quotes"))
                           " | xargs echo -n"))
                "")))))
;; Global Variables:13 ends here

;; [[file:init-emacs.org::*Load Path][Load Path:1]]
(init-message 2 "Environment Settings: Load Path")

;; add paths to the head of `load-path' in reverse order.

;; personal elisp projects
(when (file-exists-p local-modules-dir)
  (add-to-list 'load-path local-modules-dir))

;; ;; personal emacs initialization scripts
;; (when (file-exists-p local-init-dir)
;;   (add-to-list 'load-path local-init-dir))

;; ;; emacs home directory
;; (when (file-exists-p emacs-home-dir)
;;   (add-to-list 'load-path emacs-home-dir))

;; emacs modules directory
(when (file-exists-p emacs-modules-dir)
  (add-to-list 'load-path emacs-modules-dir))

;; ;; org-mode directory
;; (when (file-exists-p (expand-file-name "org-mode/contrib/lisp" emacs-modules-dir))
;;   (add-to-list 'load-path (file-truename (expand-file-name "org-mode/contrib/lisp" emacs-modules-dir))))
;; (when (file-exists-p (expand-file-name "org-mode/lisp" emacs-modules-dir))
;;   (add-to-list 'load-path (file-truename (expand-file-name "org-mode/lisp" emacs-modules-dir))))
;; Load Path:1 ends here

;; [[file:init-emacs.org::*GUI][GUI:1]]
;;------------------------------------------------------------------------------
;;; General Settings: GUI
;;------------------------------------------------------------------------------

(when window-system

  (init-message 2 "General Settings: GUI")

  ;; clipboard
  (when (string= window-system "x")
    (setq select-enable-clipboard t                                      ; cutting and pasting uses clipboard
          select-enable-primary t                                        ; cutting and pasting uses primary selection
          x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING) ; data-type request for X selection
          save-interprogram-paste-before-kill t                          ; save clipboard strings into kill ring before replacing them
          interprogram-paste-function 'x-cut-buffer-or-selection-value   ; function to call to get text cut from other programs
          mouse-yank-at-point t))                                        ; mouse yank commands yank at point

  ;; inverse video on
  (setq inverse-video t)

  ;; visible bell
  (setq visible-bell t)

  ;; turn off bell
  (setq ring-bell-function 'ignore)

  ;; turn off cursor blinking
  (blink-cursor-mode 0)

  ;; ;; scroll bar on right
  ;; (setq scroll-bar-mode 'right)
  ;; (scroll-bar-mode -1)
  ;; (scroll-bar-mode 1)

  ;; turn off scroll bar
  (when (and (fboundp 'scroll-bar-mode)
             scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; turn off toolbar
  (when (and (fboundp 'tool-bar-mode)
             tool-bar-mode)
    (tool-bar-mode -1))

  ;; make default frame size fullscreen
  ;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; put current buffer name in title bar
  (setq frame-title-format "%b")

  ;; mouse button one drags the scroll bar
  (bind-keys* ([vertical-scroll-bar down-mouse-1] . scroll-bar-drag))

  ;; scroll mouse settings
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed t)

  ;; set default font
  (ignore-errors
    (cl-case window-system
      (x
       (condition-case nil
           (progn
             ;;(set-frame-font "8x13" nil t)
             ;;(set-frame-font "9x15" nil t)
             ;;(set-frame-font "Ubuntu Mono-13" nil t)
             ;;(set-frame-font "Inconsolata-15" nil t)
             ;;(set-frame-font "BitstreamVeraSansMono Nerd Font Mono-12" nil t)
             ;;(set-frame-font "DroidSansMono Nerd Font Mono-12" nil t)
             (set-frame-font "Hack Nerd Font Mono-12" nil t)
             ;;(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 132)
             ;;(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font Mono" :height 156)
             ;;(set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height 168 :weight 'regular)
             )
         ('error
          (set-frame-font "9x15" nil t))))
      (w32
       (condition-case nil
           (set-frame-font "Hack Nerd Font Mono-12" nil t)
         ('error
          nil)))
      (ns
       (condition-case nil
           (progn
             ;;(set-frame-font "BitstreamVeraSansMono Nerd Font Mono-14" nil t)
             ;;(set-frame-font "DroidSansMono Nerd Font Mono-14" nil t)
             (set-frame-font "Hack Nerd Font Mono-14" nil t)
             )
         ('error
          (set-frame-font "Menlo" nil t))))))

  ;; ;; set faces
  ;; ;; green foreground on black background with yellow cursor
  ;; (custom-set-faces
  ;;  `(default ((t (:foreground ,color-foreground :background ,color-background)))) ; green
  ;;  `(cursor ((t (:foreground ,color-background :background ,color-cursor))))) ; yellow

  ;; set faces
  ;; white foreground on black background with yellow cursor
  (custom-set-faces
   `(default ((t (:foreground ,color-foreground :background ,color-background))))
   `(cursor ((t (:foreground ,color-background :background ,color-cursor)))))

  ;; transparant background (not on Macs)
  (defvar background-alpha
    100
    "Background transparency alpha percentage.

Common values:

  100 = none
  90  = 10% transparency
  85  = 15% transparency
  80  = 20% transparency")
  (setq background-alpha (if (or window-system-mac window-system-windows)
                             100        ; 0% transparency
                           85))         ; 10% transparency
  (set-frame-parameter (selected-frame) 'alpha
                       `(,background-alpha . ,background-alpha))
  (add-to-list 'default-frame-alist
               `(alpha . (,background-alpha . ,background-alpha)))

  ;; set mouse color
  (set-mouse-color color-mouse)

  ;;------------------------------------------------------------------------------
  ;;;; Theme
  ;;------------------------------------------------------------------------------

  (init-message 3 "General Settings: GUI: Theme")

  ;; flatland theme
  ;; https://github.com/gchp/flatland-emacs
  (use-package flatland-theme
    :straight t
    :init (load-theme 'flatland t))

  ;; ;; darcula theme
  ;; ;; https://github.com/ianyepan/jetbrains-darcula-emacs-theme
  ;; (use-package jetbrains-darcula-theme
  ;;   :straight t
  ;;   :init (load-theme 'jetbrains-darcula t))

  ;; ;; gruber-darker theme
  ;; ;; https://github.com/rexim/gruber-darker-theme
  ;; (use-package gruber-darker-theme
  ;;   :straight t
  ;;   :init (load-theme 'gruber-darker t))

  ;; ;; dracula theme
  ;; ;; https://draculatheme.com/emacs/
  ;; (use-package dracula-theme
  ;;   :straight t
  ;;   :init (load-theme 'dracula t))

  ;; ;; material theme
  ;; ;; https://github.com/cpaulik/emacs-material-theme
  ;; (use-package material-theme
  ;;   :straight t
  ;;   :init (load-theme 'material t))

  ;; material theme
  ;; https://github.com/cpaulik/emacs-material-theme
  ;; (use-package material-theme
  ;;   :load-path (lambda () (file-truename (expand-file-name "material-theme.el" local-modules-dir)))
  ;;   :init (load-theme 'material t))

  ;; ;; zenburn theme
  ;; ;; https://github.com/bbatsov/zenburn-emacs
  ;; (use-package zenburn-theme
  ;;   :straight t
  ;;   :init
  ;;   (setq zenburn-override-colors-alist   ; default values
  ;;         '(("zenburn-bg+05" . "#181818") ; #383838
  ;;           ("zenburn-bg+1"  . "#1f1f1f") ; #4f4f4f
  ;;           ("zenburn-bg+2"  . "#2f2f2f") ; #5f5f5f
  ;;           ("zenburn-bg+3"  . "#3f3f3f"))) ; #6f6f6f
  ;;   (load-theme 'zenburn t))

  ;; ;; color-theme-sanityinc-tomorrow theme
  ;; ;; https://github.com/purcell/color-theme-sanityinc-tomorrow
  ;; (use-package color-theme-sanityinc-tomorrow
  ;;   :straight t
  ;;   :init (load-theme 'sanityinc-tomorrow-night t))

  ;; ;; spacemacs-theme
  ;; ;; https://github.com/nashamri/spacemacs-theme
  ;; (use-package spacemacs-theme
  ;;   :straight t
  ;;   :init (load-theme 'spacemacs-dark t))

  ;; ;; solarized theme
  ;; ;; https://github.com/bbatsov/solarized-emacs
  ;; (use-package solarized-theme
  ;;   :straight t
  ;;   :init
  ;;   ;; make the fringe stand out from the background
  ;;   ;;(setq solarized-distinct-fringe-background t)
  ;;   ;; do not change the font for some headings and titles
  ;;   (setq solarized-use-variable-pitch nil)
  ;;   ;; make the modeline high contrast
  ;;   ;;(setq solarized-high-contrast-mode-line t)
  ;;   ;; use less bolding
  ;;   (setq solarized-use-less-bold t)
  ;;   ;; use more italics
  ;;   ;;(setq solarized-use-more-italic t)
  ;;   ;; use less colors for indicators such as git:gutter, flycheck and similar
  ;;   ;;(setq solarized-emphasize-indicators nil)
  ;;   ;; do not change size of org-mode headlines (but keep other size-changes)
  ;;   (setq solarized-scale-org-headlines nil)
  ;;   ;; avoid all font-size changes
  ;;   (setq solarized-height-minus-1 1.0)
  ;;   (setq solarized-height-plus-1 1.0)
  ;;   (setq solarized-height-plus-2 1.0)
  ;;   (setq solarized-height-plus-3 1.0)
  ;;   (setq solarized-height-plus-4 1.0)
  ;;   ;; load theme
  ;;   (load-theme 'solarized-dark t))

  ;; ;; doom themes
  ;; (use-package doom-themes
  ;;   :straight t
  ;;   :custom
  ;;   (doom-themes-enable-bold t)      ; if nil, bold is universally disabled
  ;;   (doom-themes-enable-italic t)    ; if nil, italics is universally disabled
  ;;   (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;   :config
  ;;   (load-theme 'doom-one t)
  ;;   ;;(load-theme 'doom-dracula t)
  ;;   ;; enable flashing mode-line on errors
  ;;   (doom-themes-visual-bell-config)
  ;;   ;; enable custom neotree theme (all-the-icons must be installed)
  ;;   (doom-themes-neotree-config)
  ;;   ;; or for treemacs users
  ;;   (doom-themes-treemacs-config)
  ;;   ;; correct (and improve) org-mode's native fontification
  ;;   (doom-themes-org-config))
  )
;; GUI:1 ends here

;; [[file:init-emacs.org::*General][General:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: General
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: General")
;; General:1 ends here

;; [[file:init-emacs.org::*General][General:2]]
(init-message 3 "Disable Splash Screen")

;; disable splash screen
(setq inhibit-startup-screen t)
;; General:2 ends here

;; [[file:init-emacs.org::*General][General:3]]
;; prefer newer el files over elc
(setq load-prefer-newer t)
;; General:3 ends here

;; [[file:init-emacs.org::*General][General:4]]
(init-message 3 "Hide Menu Bar")

;; hide menu-bar (use C-M-z to activate)
(when (and (fboundp 'menu-bar-mode)
           menu-bar-mode)
  (menu-bar-mode -1))
;; General:4 ends here

;; [[file:init-emacs.org::*General][General:6]]
(init-message 3 "Set Default Buffer Mode to `org-mode'")

;; set default buffer mode to `org-mode'
(setq initial-major-mode 'org-mode)
;; General:6 ends here

;; [[file:init-emacs.org::*General][General:7]]
(init-message 3 "Start with Empty Scratch Buffer")

;; clear scratch buffer
(setq initial-scratch-message nil)
;; General:7 ends here

;; [[file:init-emacs.org::*General][General:9]]
(init-message 3 "Ask before Closing Emacs")

;; ask before closing emacs
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
            kill-emacs-query-functions))
;; General:9 ends here

;; [[file:init-emacs.org::*General][General:10]]
(init-message 3 "Make Backspace Key Work Correctly")

;; make baskspace key work
(normal-erase-is-backspace-mode 1)
;; General:10 ends here

;; [[file:init-emacs.org::*General][General:11]]
(init-message 3 "Add Some Characters to Word Boundaries")

;; add underscore to word boundaries
(modify-syntax-entry ?_ "w")

;; add dash to word boundaries
(modify-syntax-entry ?- "w")
;; General:11 ends here

;; [[file:init-emacs.org::*General][General:12]]
(init-message 3 "Beginning of Defun is Outermost Level Open-Paren.")

(setq open-paren-in-column-0-is-defun-start nil
      defun-prompt-regexp nil)
;; General:12 ends here

;; [[file:init-emacs.org::*General][General:13]]
(init-message 3 "Ignore Comments when Parsing S-Expressions")

;; do not parse comments in sexp's
(setq parse-sexp-ignore-comments t)
(setq-default parse-sexp-ignore-comments parse-sexp-ignore-comments)
;; General:13 ends here

;; [[file:init-emacs.org::*General][General:14]]
;; (init-message 3 "Wrap Lines by Default")

;; ;; wrap lines
;; (setq truncate-lines nil)
;; General:14 ends here

;; [[file:init-emacs.org::*General][General:15]]
(init-message 3 "Do Not Wrap Lines by Default")

;; turn off line wrapping
(setq truncate-lines t)
(setq-default truncate-lines t)
(toggle-truncate-lines 1)
;; General:15 ends here

;; [[file:init-emacs.org::*General][General:17]]
(init-message 3 "Turn Off `auto-fill-mode'")

;; do not automatically break lines by inserting newlines
(turn-off-auto-fill)
;; General:17 ends here

;; [[file:init-emacs.org::*General][General:18]]
;; (init-message 3 "Turn On `global-visual-line-mode'")

;; ;; visually break lines that are longer than the screen width
;; (global-visual-line-mode 1)

;; ;; (defun buffer-menu-mode-hook--visual-line-mode()
;; ;;   "Hook to turn on `visual-line-mode' in most buffers."
;; ;;   (when (not (string= (substring (buffer-name) 0 1) "*"))
;; ;;     (visual-line-mode 1)))
;; ;; (add-hook 'buffer-menu-mode-hook #'buffer-menu-mode-hook--visual-line-mode)

;; ;; use curly arrows to indicate a visual line wrap
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; General:18 ends here

;; [[file:init-emacs.org::*General][General:19]]
(init-message 3 "Turn On `global-visual-line-mode'")

;; do not visually break lines that are longer than the screen width
(global-visual-line-mode -1)

;; (defun buffer-menu-mode-hook--visual-line-mode()
;;   "Hook to turn off `visual-line-mode' in most buffers."
;;   (when (not (string= (substring (buffer-name) 0 1) "*"))
;;     (visual-line-mode -1)))
;; (add-hook 'buffer-menu-mode-hook #'buffer-menu-mode-hook--visual-line-mode)

;; use curly arrows to indicate a visual line wrap
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; General:19 ends here

;; [[file:init-emacs.org::*General][General:20]]
(init-message 3 "Set `display-line-numbers-type' to relative")

;; when `display-line-numbers-mode' is on use relative numbering
(setq display-line-numbers-type 'relative)
;; General:20 ends here

;; [[file:init-emacs.org::*General][General:21]]
(init-message 3 "Prevent `next-line' from Inserting Newlines")

;; stop cursor at the end of the file
(setq next-line-add-newlines nil)
;; General:21 ends here

;; [[file:init-emacs.org::*General][General:22]]
(init-message 3 "Keep the Cursor in the Same Column When Using Page-Up and Page-Down")

;; keep screen position when using page-up and page-down
(setq scroll-preserve-screen-position 'keep)
;; General:22 ends here

;; [[file:init-emacs.org::*General][General:23]]
(init-message 3 "Scroll Conservatively")

;; scroll one line at a time
(setq scroll-step 1)
;; scroll fewer lines
(setq scroll-conservatively 101)
;; turn off vertical auto-scroll
(setq auto-window-vscroll nil)
;; General:23 ends here

;; [[file:init-emacs.org::*General][General:24]]
(init-message 3 "Ignore Case on Search Matches")

;; make searches case-insensitive
(setq case-fold-search t)
;; General:24 ends here

;; [[file:init-emacs.org::*General][General:25]]
(init-message 3 "Highlight Search Matches")

;; highlight search matches
(setq search-highlight t
      ;;isearch-highlight t
      query-replace-highlight t)
;; General:25 ends here

;; [[file:init-emacs.org::*General][General:26]]
(init-message 3 "Hightlight Marked Regions")

;; make current selection visible
(transient-mark-mode 1)
(setq-default transient-mark-mode t)
;; General:26 ends here

;; [[file:init-emacs.org::*General][General:27]]
(init-message 3 "Set Default Tab Indentation to Four Spaces and Turn on Auto-Complete")

;; set tab indentation, width, and convert tabs to spaces
(setq indent-tabs-mode nil              ; do not insert tab characters
      tab-width 4                       ; default tab width is four spaces
      standard-indent 4                 ; default margin-changing functions indent
      tab-always-indent 'complete       ; tab key will try to auto-complete after auto-tabbing line
      tab-stop-list (number-sequence 4 180 4)) ; tab stops set to every 4 spaces
(setq-default indent-tabs-mode indent-tabs-mode
              tab-width tab-width
              standard-indent standard-indent
              tab-always-indent tab-always-indent
              tab-stop-list tab-stop-list)
;; General:27 ends here

;; [[file:init-emacs.org::*General][General:28]]
(init-message 3 "Set Default Line-Wrapping Column to 78")

;; set default fill column for `auto-fill-mode' mode and `fill-paragraph'
(setq fill-column 78)
(setq-default fill-column fill-column)
;; General:28 ends here

;; [[file:init-emacs.org::*General][General:29]]
(init-message 3 "Set Default Right-Margin Comment Indent Column to 40")

;; set default comment column for in-line comments
(setq comment-column 40)
(setq-default comment-column comment-column)
;; set default comment fill column for in-line comments
(setq comment-fill-column nil)
(setq-default comment-fill-column comment-fill-column)
;; General:29 ends here

;; [[file:init-emacs.org::*General][General:30]]
(init-message 3 "Have Cursor Movements Attempt to Keep Point on Original Column")

;; turn on goal column support
(put 'set-goal-column 'disabled nil)
;; General:30 ends here

;; [[file:init-emacs.org::*General][General:31]]
(init-message 3 "Sentences and Colons Should Have One Space after Them")

;; insert one space after a sentence when filling text
(setq sentence-end-double-space nil)
;; insert one space after a colon when filling text
(setq colon-double-space nil)
;; General:31 ends here

;; [[file:init-emacs.org::*General][General:32]]
;; (init-message 3 "Sentences and Colons Should Have Two Spaces after Them")

;; ;; insert two spaces after a sentence when filling text
;; (setq sentence-end-double-space t)
;; ;; insert two spaces after a colon when filling text
;; (setq colon-double-space t)
;; General:32 ends here

;; [[file:init-emacs.org::*General][General:33]]
(init-message 3 "Highlight Matching Parenthesis")

;; highlight matching parenthesis
(show-paren-mode 1)
(set-face-foreground 'show-paren-match color-paren)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;; General:33 ends here

;; [[file:init-emacs.org::*General][General:34]]
(init-message 3 "Highlight TABs")

;; highlight tabs
(setq highlight-tabs t)
(setq-default highlight-tabs highlight-tabs)
;; General:34 ends here

;; [[file:init-emacs.org::*General][General:35]]
(init-message 3 "Highlight Tabs and Trailing Whitespace")

;; ;; highlight trailing white spaces
;; (setq show-trailing-whitespace t)
;; (setq-default show-trailing-whitespace show-trailing-whitespace)

;; highlight tabs and trailing white spaces
(setq whitespace-style '(face trailing tab-mark))
(custom-set-faces `(whitespace-tab ((t (:foreground ,color-foreground :background ,color-background)))))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) ; 32 space ' ', 183 middle dot '·', 46 full stop '.'
        (newline-mark 10 [182 10]) ; 10 linefeed '\n', 182 ??? '¶'
        (tab-mark 9 [9655 9] [92 9])))    ; 9 tab '\t', 9655 '▷', 92 backslash '\'
(global-whitespace-mode)              ; enable whitespace mode everywhere
;; General:35 ends here

;; [[file:init-emacs.org::*General][General:36]]
(init-message 3 "Highlight Current Line")

;; highlight current line
(hl-line-mode 1)
(global-hl-line-mode 1)
;; General:36 ends here

;; [[file:init-emacs.org::*General][General:37]]
(init-message 3 "Turn on Syntax Highlighting")

;; turn on global font lock mode and syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
;; General:37 ends here

;; [[file:init-emacs.org::*General][General:38]]
(init-message 3 "Typing Replaces Highlighted Text")

;; replace highlighted text with typed text
(delete-selection-mode t)
;; General:38 ends here

;; [[file:init-emacs.org::*General][General:39]]
(init-message 3 "Set Commenting Style to Indent")

;; ;; set comment start (default) and padding
;; (setq comment-start "#"
;;       comment-padding " ")
;; set comment style
(setq comment-style 'indent)
;; General:39 ends here

;; [[file:init-emacs.org::*General][General:40]]
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
;; General:40 ends here

;; [[file:init-emacs.org::*General][General:42]]
(init-message 3 "Have `apropos' Search All Symbols and Order by Relevance")

;; make apropos command search all symbols
(setq apropos-do-all t)

;; make apropos command list results by relevance
(setq apropos-sort-by-scores t
      apropos-documentation-sort-by-scores t)
;; General:42 ends here

;; [[file:init-emacs.org::*General][General:43]]
(init-message 3 "Set Default `grep' Command")

;; set grep command
(setq grep-command "grep -n -H -i -r -e ")
;; General:43 ends here

;; [[file:init-emacs.org::*General][General:44]]
(init-message 3 "Set Email Sources")

;; email settings
(setq mail-sources `((pop :server "pop.gmail.com" :port 995
                          :user ,user-mail-address
                          :connection ssl :leave t)))
;; General:44 ends here

;; [[file:init-emacs.org::*General][General:45]]
(init-message 3 "Set Default Browser")

;; set default browser
;;(setq browse-url-generic-program "x-www-browser")
;;(setq browse-url-generic-program "w3m")
;;(setq browse-url-generic-program "mozilla")
(setq browse-url-browser-function 'browse-url-default-browser)
;;(setq browse-url-browser-function 'browse-url-generic)
;;(setq browse-url-browser-function 'eww-browse-url)
;;(setq browse-url-browser-function 'w3m-browse-url)
;;(setq browse-url-browser-function 'browse-url-firefox
;;      browse-url-new-window-flag  t
;;      browse-url-firefox-new-window-is-tab t)

;; set secondary browser
(setq browse-url-secondary-browser-function 'browse-url-default-browser)
;; General:45 ends here

;; [[file:init-emacs.org::*General][General:46]]
(init-message 3 "Single Character Deletion Commands Delete Active Regions Without Saving to the Kill Ring")

;; when deleting an active region via single character deletion command,
;; do not save to kill ring
(setq delete-active-region t)
;; General:46 ends here

;; [[file:init-emacs.org::*General][General:47]]
(init-message 3 "Disable `vc-git'.")

;; disable vc-git
(setq vc-handled-backends nil)
;; General:47 ends here

;; [[file:init-emacs.org::*General][General:48]]
;; (init-message 3 "Recenter window after `next-error'.")

;; ;; always recenter after `next-error'
;; (setq next-error-recenter '(4))
;; ;;(add-hook 'next-error-hook #'recenter :append)
;; General:48 ends here

;; [[file:init-emacs.org::*General][General:49]]
(init-message 3 "Recenter window after `occur-mode-goto-occurrence'.")

;; always recenter after `occur-mode-goto-occurrence'
(defun occur-mode-goto-occurrence--recenter (&optional arg)
  "Recenter when an `occur' result is selected."
  (recenter))
;; advise `occur-mode-goto-occurrence'
(advice-add 'occur-mode-goto-occurrence :after #'occur-mode-goto-occurrence--recenter)
;; General:49 ends here

;; [[file:init-emacs.org::*General][General:50]]
(init-message 3 "Set time zones to use for `display-time-world'.")

;; set display-time-world time zones
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "San Diego")
        ("America/Chicago" "Minneapolis")
        ("America/New_York" "New York")
        ("Etc/GMT" "GMT")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Asia/Tokyo" "Tokyo")))
;; General:50 ends here

;; [[file:init-emacs.org::*System][System:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: System
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: System")
;; System:1 ends here

;; [[file:init-emacs.org::*System][System:3]]
;; set max variable bindings
(setq max-specpdl-size 10000)           ; default: 1300
;; System:3 ends here

;; [[file:init-emacs.org::*System][System:4]]
;; set max eval depth
(setq max-lisp-eval-depth 10000)        ; default: 600
;; System:4 ends here

;; [[file:init-emacs.org::*System][System:5]]
;; set max message log size
(setq message-log-max 2048)             ; default: 1000
;; System:5 ends here

;; [[file:init-emacs.org::*System][System:6]]
;; set max history list size
(setq history-length 250)               ; default: 30

;; remove duplicates from history lists
(setq history-delete-duplicates t)      ; default: nil
;; System:6 ends here

;; [[file:init-emacs.org::*System][System:7]]
;; set max kill ring size
(setq kill-ring-max 100)                ; default: 60

;; set max mark ring size
(setq mark-ring-max 32)                 ; default: 16
;; System:7 ends here

;; [[file:init-emacs.org::*System][System:8]]
;; change all calls to `yes-or-no-p' to `y-or-n-p'
(fset 'yes-or-no-p 'y-or-n-p)
;; System:8 ends here

;; [[file:init-emacs.org::*System][System:9]]
;; enable upercase region (C-x C-u)
(put 'upcase-region 'disabled nil)

;; enable lowercase region (C-x C-l)
(put 'downcase-region 'disabled nil)

;; enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; turn off the disabling of certain commands
(setq disabled-command-function nil)
;; System:9 ends here

;; [[file:init-emacs.org::*System][System:10]]
;; turn off bidirectional paragraph formatting
(setq-default bidi-paragraph-direction 'left-to-right)

;; turn off bidirectional parentheses matching
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t))

;; turn on `so-long-mode' for files with long lines to help with performance
(when (version<= "27.1" emacs-version)
  (global-so-long-mode 1))
;; System:10 ends here

;; [[file:init-emacs.org::*System][System:11]]
(init-message 3 "Set `safe-local-variable-values'.")

;; org-babel noweb start and end patterns
(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-start . "{{"))
(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-end . "}}"))
;; System:11 ends here

;; [[file:init-emacs.org::*Files][Files:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: Files
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: Files")
;; Files:1 ends here

;; [[file:init-emacs.org::*Files][Files:2]]
;; increase maximum size before confirmation is requested
(setq large-file-warning-threshold 50000000)
;; Files:2 ends here

;; [[file:init-emacs.org::*Files][Files:3]]
;; enable file variables
(setq enable-local-variables t
      enable-local-eval 'maybe)
;; Files:3 ends here

;; [[file:init-emacs.org::*Files][Files:4]]
;; delete auto-save files
(setq delete-auto-save-files t)

;; do not make auto-save files
(setq auto-save-default nil)
(setq-default auto-save-default nil)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix temporary-file-directory
      auto-save-list-file-name nil
      auto-save-default nil)
;; Files:4 ends here

;; [[file:init-emacs.org::*Files][Files:5]]
;; do not make backup files
(setq make-backup-files nil)
(setq-default make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      make-backup-files nil
      backup-by-copying t
      version-control nil
      delete-old-versions t)
;; Files:5 ends here

;; [[file:init-emacs.org::*Files][Files:6]]
;; ;; do not make lock files
;; (setq create-lock-files nil)
;; (setq-default create-lock-files nil)
;; Files:6 ends here

;; [[file:init-emacs.org::*Files][Files:7]]
;; follow symlinks to version control files without asking or warning
(setq vc-follow-symlinks t)
(setq-default vc-follow-symlinks vc-follow-symlinks)
;; Files:7 ends here

;; [[file:init-emacs.org::*Files][Files:8]]
;; handle gzip/zip/jar/tar files
(auto-compression-mode t)
;; Files:8 ends here

;; [[file:init-emacs.org::*Files][Files:9]]
;; reuse existing buffers, following file links
(setq find-file-existing-other-name t)
;; Files:9 ends here

;; [[file:init-emacs.org::*Files][Files:10]]
;; end files with a newline
(setq require-final-newline t)
;; Files:10 ends here

;; [[file:init-emacs.org::*Files][Files:12]]
;; turn on auto buffer revert mode
(global-auto-revert-mode 1)
;; this is currently bugged and will cause the buffer to re-center vertically every couple of seconds
;; (setq global-auto-revert-non-file-buffers t ; auto refresh dired too
;;       auto-revert-verbose nil)              ; but, be quiet about it
;; Files:12 ends here

;; [[file:init-emacs.org::*Files][Files:13]]
(defun create-buffer-file-name-directory-if-needed ()
  "Create `buffer-file-name' directory if it does not already exist."
  (when (and buffer-file-name
             (not (file-exists-p (file-name-directory buffer-file-name))))
    (make-directory (file-name-directory buffer-file-name) t)))

;; create directories if needed on file save
(add-hook 'before-save-hook #'create-buffer-file-name-directory-if-needed)
;; Files:13 ends here

;; [[file:init-emacs.org::*Files][Files:14]]
;; delete trailing lines on call to `delete-trailing-whitespace'
(setq delete-trailing-lines t)

(defun delete-trailing-whitespace-if-not-read-only (&optional beg end)
  "Call `delete-trailing-whitespace' if current buffer is not read-only."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (when (not buffer-read-only)
    (delete-trailing-whitespace beg end)))

;; delete trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace-if-not-read-only)
;; Files:14 ends here

;; [[file:init-emacs.org::*Files][Files:15]]
;; make shell scripts executable when saving (and reset the buffer mode)
(when (fboundp 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))
;; Files:15 ends here

;; [[file:init-emacs.org::*Files][Files:16]]
;; set DOS file extensions
(add-to-list 'file-coding-system-alist '("\\.ASM\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.BAT\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.DO\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.SYS\\'" . dos))
;; Files:16 ends here

;; [[file:init-emacs.org::*Files][Files:17]]
;; auto-save bookmarks
(setq bookmark-save-flag 1)
;; Files:17 ends here

;; [[file:init-emacs.org::*Files][Files:18]]
;; desktop history
(when-lock-file-acquired (expand-file-name "emacs-desktop-history-lock-file"
                                          temporary-file-directory)
  (desktop-save-mode 1)
  (setq desktop-save 'ask-if-new
        desktop-load-locked-desktop t
        desktop-restore-eager 0 ; do not restore any buffers until all modules and modes have loaded
        desktop-buffers-not-to-save (concat "\\("
                                            "\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                            "\\.diary\\|\\diary\\|\\.bbdb"
                                            "\\)$"))
  (add-to-list 'desktop-globals-to-save 'file-name-history t)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode t)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode t)
  ;;(add-to-list 'desktop-modes-not-to-save 'dired-mode t)
  ;;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode t)
  )
;; Files:18 ends here

;; [[file:init-emacs.org::*Files][Files:19]]
;; save minibuffer history
(when (fboundp 'savehist-mode)
  (when-lock-file-acquired (expand-file-name "emacs-minibuffer-history-lock-file"
                                            temporary-file-directory)
    (savehist-mode 1)
    (setq savehist-save-minibuffer-history 1
          savehist-additional-variables '(search-ring
                                          regexp-search-ring))))
;; Files:19 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:1]]
;;------------------------------------------------------------------------------
;;; Environment Settings: Buffers and Windows
;;------------------------------------------------------------------------------

(init-message 2 "Environment Settings: Buffers and Windows")
;; Buffers and Windows:1 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:2]]
;; allow undo/redo of window settings
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; Buffers and Windows:2 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:4]]
;; delay buffer fontification to increase scroll speed
(setq jit-lock-defer-time 0.05)
;; Buffers and Windows:4 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:5]]
;; preserve buffer point for each window
(setq switch-to-buffer-preserve-window-point t)
;; Buffers and Windows:5 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:6]]
;; smoother mouse movement
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
;; Buffers and Windows:6 ends here

;; [[file:init-emacs.org::*Buffers and Windows][Buffers and Windows:7]]
;; increase maximum mini-window height
(setq max-mini-window-height 0.50)
;; Buffers and Windows:7 ends here

;; [[file:init-emacs.org::*Tabs][Tabs:1]]
;;------------------------------------------------------------------------------
;;; General Settings: Tabs
;;------------------------------------------------------------------------------

(init-message 2 "General Settings: Tabs")
;; Tabs:1 ends here

;; [[file:init-emacs.org::*Tabs][Tabs:2]]
;; regular tab width
(defvar custom-tab-width 4
  "Regular tab width.")

;; short tab width used for certain modes
(defvar custom-short-tab-width 2
  "Short tab width used for certain modes.")

;; set tabs
(defun set-tabs (enable &optional width)
  "Set default tab settings.

If ENABLED is non-nil, enable TAB characters.
Otherwise, disable TAB characters.

If WIDTH is given, it is used to set the TAB width.
Otherwise, `custom-tab-width' is used."
  (let ((width (or width custom-tab-width)))
    (setq indent-tabs-mode enable       ; whether to insert tab characters
          tab-width width               ; set tab width
          standard-indent width         ; set margin-changing functions indent
          tab-always-indent 'complete ; tab key will try to auto-complete after auto-tabbing line
          tab-stop-list (number-sequence width 180 width) ; set tab stops
          backward-delete-char-untabify-method nil))) ; backspace will just delete one character
          ;; backward-delete-char-untabify-method 'untabify))) ; backspace will turn a tab to many spaces, then delete one space
          ;; backward-delete-char-untabify-method 'hungry))) ; backspace will delete all whitespace, both tabs and spaces

;; disable tabs
(defun disable-tabs (&optional width)
  "Disable TAB character usage, using WIDTH if given."
  (set-tabs nil width))

;; enable tabs
(defun enable-tabs (&optional width)
  "Enable TAB character usage, using WIDTH if given."
  (set-tabs t width))

;; enable tabs 4
(defun enable-tabs-4 ()
  "Enable TAB character usage with a width of 4 spaces."
  (set-tabs t 4))

;; enable tabs 8
(defun enable-tabs-8 ()
  "Enable TAB character usage with a width of 8 spaces."
  (set-tabs t 8))
;; Tabs:2 ends here

;; [[file:init-emacs.org::*Configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;;; Environment Settings: Terminals: Configuration
;;------------------------------------------------------------------------------

(init-message 3 "Environment Settings: Terminals: Configuration")

(setq custom-terminal-history-size 10000
      custom-terminal-maximum-lines 10000)
;; Configuration:1 ends here

;; [[file:init-emacs.org::*eshell][eshell:1]]
;;------------------------------------------------------------------------------
;;;; Environment Settings: Terminals: eshell
;;------------------------------------------------------------------------------

(init-message 3 "Environment Settings: Terminals: eshell")

(defun custom-eshell-first-time-mode-hook ()
  ;; save command history
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

  ;; truncate history for performance
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer))

(use-package eshell
  :straight (:type built-in)
  :hook (eshell-first-time-mode . custom-eshell-first-time-mode-hook)
  ;; :bind (:map eshell-mode-map
  ;;             ([remap beginning-of-line] . eshell-bol)
  ;;             ([remap move-beginning-of-line] . eshell-bol)
  ;;             ("C-r" . counsel-esh-history))
  :custom
  (eshell-history-size custom-terminal-history-size)
  (eshell-buffer-maximum-lines custom-terminal-maximum-lines)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input t)
  :config
  (setenv "PAGER" "cat")                ; less does not work well inside Emacs

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands '("htop" "ssh" "vim" "zsh"))))

(use-package eshell-git-prompt
  :straight t
  :after (eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))
;; eshell:1 ends here

;; [[file:init-emacs.org::*term-bash][term-bash:1]]
;;------------------------------------------------------------------------------
;;;; Environment Settings: Terminals: term-bash
;;------------------------------------------------------------------------------

(init-message 3 "Environment Settings: Terminals: term-bash")

(defun term-bash ()
  "Start a BASH terminal-emulator in a new buffer."
  (interactive)
  (term "/bin/bash"))
;; term-bash:1 ends here

;; [[file:init-emacs.org::*term-zsh][term-zsh:1]]
;;------------------------------------------------------------------------------
;;;; Environment Settings: Terminals: term-zsh
;;------------------------------------------------------------------------------

(init-message 3 "Environment Settings: Terminals: term-zsh")

(defun term-zsh ()
  "Start a ZSH terminal-emulator in a new buffer."
  (interactive)
  (term "/bin/zsh"))
;; term-zsh:1 ends here

;; [[file:init-emacs.org::*vterm][vterm:1]]
;;------------------------------------------------------------------------------
;;;; Environment Settings: Terminals: vterm
;;------------------------------------------------------------------------------

(init-message 3 "Environment Settings: Terminals: vterm")

(use-package vterm
  :straight t
  :commands (vterm)
  :custom
  (vterm-max-scrollback custom-terminal-maximum-lines)
  (vterm-kill-buffer-on-exit nil))
;; vterm:1 ends here

;; [[file:init-emacs.org::*Key Bindings][Key Bindings:1]]
;;==============================================================================
;;; Key Bindings
;;==============================================================================

(init-message 1 "Key Bindings")
;; Key Bindings:1 ends here

;; [[file:init-emacs.org::*System Keys][System Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: System Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: System Keys")

(defun custom-key-bindings-system-keys ()
  "Set custom system key bindings."
  (cond
   ;; most of these are already set correctly for MacOS
   (window-system-mac
    ;; new buffer
    (when (fboundp 'new-scratch)
      (bind-keys* ("s-t" . new-scratch))))
   ((or window-system-linux window-system-windows)
    ;; cut
    (if (fboundp 'kill-region-or-word)
        (bind-keys* ("s-x" . kill-region-or-word))
      (bind-keys* ("s-x" . kill-region)))
    ;; copy
    (bind-keys* ("s-c" . kill-ring-save))
    ;; paste
    (bind-keys* ("s-v" . yank))
    ;; undo
    (bind-keys* ("s-z" . undo))
    ;; redo
    (when (fboundp 'undo-tree-redo)
      (bind-keys* ("s-y" . undo-tree-redo)))
    ;; select all
    (bind-keys* ("s-a" . mark-whole-buffer))
    ;; find
    (bind-keys* ("s-f" . isearch-forward))
    ;; find next
    (bind-keys* ("s-g" . isearch-forward))
    ;; find previous
    (bind-keys* ("s-r" . isearch-backward))
    ;; open file
    (bind-keys* ("s-o" . find-file))
    ;; print
    (bind-keys* ("s-p" . print-buffer))
    ;; save buffer
    (if (fboundp 'save-buffer-always)
        (bind-keys* ("s-s" . save-buffer-always))
      (bind-keys* ("s-s" . save-buffer)))
    ;; new buffer
    (when (fboundp 'new-scratch)
      (bind-keys* ("s-t" . new-scratch)))
    ;; close buffer
    (bind-keys* ("s-w" . kill-current-buffer))
    ;; set mark
    (bind-keys* ("s-SPC" . set-mark-command)))))

(init-message 3 "custom-key-bindings-system-keys")
(custom-key-bindings-system-keys)
;; System Keys:1 ends here

;; [[file:init-emacs.org::*Function Keys][Function Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Function Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Function Keys")

(defun custom-key-bindings-function-keys ()
  "Set custom function key bindings."
  (when (fboundp 'help)
    (bind-keys ("<f1>" . help)))        ; default: `help-for-help'
  (when (fboundp 'help-for-help)
    (bind-keys ("S-<f1>" . help-for-help)))
  (when (fboundp 'help-command)
    (bind-keys ("<f2>" . help-command))) ; default: Prefix Command
  (when (fboundp 'kmacro-start-macro-or-insert-counter)
    (bind-keys ("<f3>" . kmacro-start-macro-or-insert-counter))) ; default: `kmacro-start-macro-or-insert-counter'
  (when (fboundp 'kmacro-end-or-call-macro)
    (bind-keys ("<f4>" . kmacro-end-or-call-macro))) ; default: `kmacro-end-or-call-macro'
  ;; (when (fboundp 'define-word)
  ;;   (bind-keys ("<f5>" . define-word)))
  ;; (when (fboundp 'define-word-at-point)
  ;;   (bind-keys ("S-<f5>" . define-word-at-point)))
  ;; (when (fboundp 'ispell-word)
  ;;   (bind-keys ("<f6>" . ispell-word)))
  ;; (when (fboundp 'ispell)
  ;;   (bind-keys ("<S-f6>" . ispell)))
  (when (fboundp 'web-query)
    (bind-keys ("<f7>" . web-query)))
  (when (fboundp 'web-query-symbol-by-mode-at-point)
    (bind-keys ("<S-f7>" . web-query-symbol-by-mode-at-point)))
  ;; (when (fboundp 'web-query-word-at-point)
  ;;   (bind-keys ("S-<f7>" . web-query-word-at-point)))
  (when (fboundp 'neotree)
    (bind-keys ("<f8>" . neotree)))
  (when (fboundp 'cycle-buffer-backward)
    (bind-keys ("<f9>" . cycle-buffer-backward)))
  (when (fboundp 'cycle-buffer-backward-permissive)
    (bind-keys ("S-<f9>" . cycle-buffer-backward-permissive)))
  (when (fboundp 'cycle-buffer)
    (bind-keys ("<f10>" . cycle-buffer))) ; default: `tmm-menubar'
  (when (fboundp 'cycle-buffer-permissive)
    (bind-keys ("S-<f10>" . cycle-buffer-permissive)))
  )

(init-message 3 "custom-key-bindings-function-keys")
(custom-key-bindings-function-keys)
;; Function Keys:1 ends here

;; [[file:init-emacs.org::*Extended Keys][Extended Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Extended Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Extended Keys")

(defun custom-key-bindings-extended-keys ()
  "Set custom extended key bindings."

  ;; ;; turn off insert key
  ;; (unbind-key "<insert>")               ; default: `overwrite-mode'

  ;; ;; window move
  ;; (when (fboundp 'windmove-left)
  ;;   ;; shift arrow keys
  ;;   (bind-keys ("S-<left>" . windmove-left)
  ;;              ("S-<right>" . windmove-right)
  ;;              ("S-<up>" . windmove-up)
  ;;              ("S-<down>" . windmove-down)))

  ;; beginning of line
  (bind-keys* ("<home>" . beginning-of-line)) ; default: `move-beginning-of-line'

  ;; end of line
  (bind-keys* ("<end>" . end-of-line)) ; default: `move-end-of-line'

  ;; beginning of buffer
  (bind-keys* ("C-<home>" . beginning-of-buffer))

  ;; end of buffer
  (bind-keys* ("C-<end>" . end-of-buffer)))

(init-message 3 "custom-key-bindings-extended-keys")
(custom-key-bindings-extended-keys)
;; Extended Keys:1 ends here

;; [[file:init-emacs.org::*Movement Keys][Movement Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Movement Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Movement Keys")

(defun custom-key-bindings-movement-keys (&optional keymap)
  "Set custom movement key bindings on KEYMAP.
KEYMAP defaults to `override-global-map'."
  (let ((keymap (or keymap override-global-map)))
    ;; cursor movement keys (short, single character)
    (bind-keys* :map keymap
                ("<up>" . previous-line) ; default: `previous-line'
                ("<down>" . next-line)   ; default: `next-line'
                ("<left>" . left-char)   ; default: `left-char'
                ("<right>" . right-char) ; default: `right-char'
                ("M-i" . previous-line)  ; default: `tab-to-tab-stop' ("C-p")
                ("M-k" . next-line)      ; default: `kill-sentence' ("C-n")
                ("M-j" . left-char)      ; default: `indent-new-comment-line' ("C-f")
                ("M-l" . right-char))    ; default: `downcase-word' ("C-b")

    ;; cursor movement keys (medium, multi-character)
    (bind-keys* :map keymap
                ("C-<up>" . backward-paragraph) ; default: `backward-paragraph'
                ("C-<down>" . forward-paragraph) ; default: `forward-paragraph'
                ("C-<left>" . left-word)         ; default: `left-word'
                ("C-<right>" . right-word)       ; default: `right-word'
                ("C-M-u" . backward-paragraph) ; default: `backward-up-list'
                ("C-M-o" . forward-paragraph)  ; default: `split-line'
                ("M-u" . left-word)            ; default: `upcase-word'
                ("M-o" . right-word))          ; default: `facemenu-keymap'

    ;; cursor movement keys (long, multi-character)
    (bind-keys* :map keymap
                ("C-M-<up>" . scroll-down-command) ; default: `scroll-down-command'
                ("C-M-<down>" . scroll-up-command) ; default: `scroll-up-command'
                ("C-M-<left>" . move-beginning-of-line) ; default: `move-beginning-of-line'
                ("C-M-<right>" . move-end-of-line) ; default: `move-end-of-line'
                ("C-M-i" . scroll-down-command) ; default: `completion-at-point' ("<prior>")
                ("C-M-k" . scroll-up-command)   ; default: `kill-whole-line' ("<next>")
                ("C-M-j" . move-beginning-of-line) ; default: `default-indent-new-line' ("C-a")
                ("C-M-l" . move-end-of-line)) ; default: `reposition-window' ("C-e")

    ;; window movement keys
    (bind-keys* :map keymap
                ("C-x <up>" . windmove-up)
                ("C-x <down>" . windmove-down)
                ("C-x <left>" . windmove-left) ; default: `previous-buffer'
                ("C-x <right>" . windmove-right) ; default: `next-buffer'
                ("C-x C-<up>" . windmove-up)
                ("C-x C-<down>" . windmove-down)
                ("C-x C-<left>" . windmove-left) ; default: `previous-buffer'
                ("C-x C-<right>" . windmove-right) ; default: `next-buffer'
                ("C-x M-i" . windmove-up)
                ("C-x M-k" . windmove-down)
                ("C-x M-j" . windmove-left)
                ("C-x M-l" . windmove-right))

    ;; move line up
    (when (fboundp 'move-line-up)
      (bind-keys* :map keymap
                  ("M-[" . move-line-up)))

    ;; move line down
    (when (fboundp 'move-line-down)
      (bind-keys* :map keymap
                  ("M-]" . move-line-down)))))

(init-message 3 "custom-key-bindings-movement-keys")
(custom-key-bindings-movement-keys)
;; Movement Keys:1 ends here

;; [[file:init-emacs.org::*Standard Keys][Standard Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Standard Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Standard Keys")

(defun custom-key-bindings-standard-keys ()
  "Set custom standard key bindings."

  ;; newline
  (when (fboundp 'insert-line-below)
    (bind-keys ("C-M-<return>" . insert-line-below)))

  ;; set mark
  (bind-keys ("C-SPC" . set-mark-command)) ; default: `set-mark-command'

  ;; set rectangle mark
  (when (fboundp 'cua-set-rectangle-mark)
    (bind-keys ("C-x rm" . cua-set-rectangle-mark)
               ("C-M-SPC" . cua-set-rectangle-mark))) ; default: `mark-sexp'

  ;; yank as rectangle
  (when (fboundp 'yank-as-rectangle)
    (bind-keys ("C-x r C-y" . yank-as-rectangle)
               ("C-M-y" . yank-as-rectangle)))

  ;; just one space
  (when (fboundp 'just-one-space)
    (bind-keys ("C-x C-SPC" . just-one-space))) ; default: `pop-global-mark'

  ;; help
  (bind-keys ("C-x C-h" . help-command)
             ("C-x ?" . help))

  ;; describe function or variable at point
  (when (fboundp 'describe-function-or-variable-at-point)
    (bind-keys ("C-h z" . describe-function-or-variable-at-point)
               ("C-x C-h z" . describe-function-or-variable-at-point)))

  ;; ;; smart M-x
  ;; (when (fboundp 'smex)
  ;;   (bind-keys ("M-x" . smex))) ; default: `execute-extended-command'
  ;; (when (fboundp 'smex-major-mode-commands)
  ;;   (bind-keys ("M-X" . smex-major-mode-commands))) ; default: `execute-extended-command'

  ;; ;; alternates for M-x
  ;; (bind-keys ("C-x C-m" . execute-extended-command)) ; default: Prefix Command

  ;; menubar
  (when (fboundp 'tmm-menubar)
    (bind-keys ("C-M-z" . tmm-menubar)))

  ;; force save maybe
  (when (fboundp 'save-buffer-always-maybe)
    (bind-keys ("C-x C-s" . save-buffer-always-maybe))) ; default: `save-buffer'

  ;; ;; bs-show (buffer select)
  ;; (when (fboundp 'bs-show)
  ;;   (bind-keys ("C-x C-b" . bs-show))) ; default: `list-buffers'

  ;; bury buffer
  (bind-keys ("C-c y" . bury-buffer))
  (bind-keys ("C-c C-y" . bury-buffer)) ; default: `org-evaluate-time-range'

  ;; revert buffer
  (bind-keys ("C-c r" . revert-buffer))
  (bind-keys ("C-c C-r" . revert-buffer))

  ;; diff buffer
  (bind-keys ("C-c d" . diff-current-buffer))

  ;; mark full word
  (when (fboundp 'mark-full-word)
    (bind-keys ("M-@" . mark-full-word))) ; default: `mark-word'

  ;; ;; expand region
  ;; (when (fboundp 'er/expand-region)
  ;;   (bind-keys ("M-=" . er/expand-region)   ; default: `count-lines-region'
  ;;              ("C-=" . er/contract-region) ; default: `count-lines-region'
  ;;              ("C-M-SPC" . er/expand-region) ; default: `mark-sexp'
  ;;              ("C-M-S-SPC" . er/contract-region)))

  ;; regexp replace
  (bind-keys ("M-&" . replace-regexp)) ; default: `async-shell-command'

  ;; insert menu prompt
  (when (fboundp 'insert-menu-prompt)
    (bind-keys ("C-x i" . insert-menu-prompt))) ; default: `insert-file'

  ;; split windows
  (bind-keys ("M-1" . delete-other-windows)) ; default: `digit-argument'
  (bind-keys ("M-2" . split-window-horizontally)) ; default: `digit-argument'
  (bind-keys ("M-3" . split-window-vertically)) ; default: `digit-argument'
  (unbind-key "M-4")                          ; default: `digit-argument'
  (if (fboundp 'swap-windows)
      (bind-keys ("M-4" . swap-windows))) ; default: `digit-argument'
  (unbind-key "M-5")                    ; default: `digit-argument'
  (when (fboundp 'toggle-window-split)
    (bind-keys ("M-5" . toggle-window-split))) ; default: `split-line'
  (bind-keys ("M-6" . switch-to-buffer-other-window)) ; default: `digit-argument'
  (unbind-key "M-7")                    ; default: `digit-argument'
  (unbind-key "M-8")                    ; default: `digit-argument'
  (unbind-key "M-8")                    ; default: `digit-argument'
  (when (fboundp 'kill-other-window-buffer)
    (bind-keys ("M-8" . kill-other-window-buffer))) ; default: `digit-argument'
  (unbind-key "M-9")                    ; default: `digit-argument'
  (when (fboundp 'kill-other-window-buffer-and-delete-window)
    (bind-keys ("M-9" . kill-other-window-buffer-and-delete-window))) ; default: `digit-argument'
  (bind-keys ("M-0" . delete-window)) ; default: `digit-argument'

  ;; switch windows
  ;; (bind-keys ("M-o" . other-window)) ; default: `facemenu-keymap'
  (when (fboundp 'ace-window)
    (bind-keys ("C-x o" . ace-window))) ; default: `other-window'

  ;; swap windows
  (when (fboundp 'swap-windows)
    (bind-keys ("C-x C-o" . swap-windows))) ; default: `delete-blank-lines'

  ;; toggle window split
  (when (fboundp 'toggle-window-split)
    (bind-keys ("C-x M-o" . toggle-window-split))) ; default: `split-line'

  ;; toggle truncate lines
  (bind-keys ("C-$" . toggle-truncate-lines))

  ;; kill current buffer
  (bind-keys ("C-x C-k" . kill-current-buffer)) ; default: `kmacro-keymap'

  ;; delete to end of line
  (when (fboundp 'delete-to-end-of-line)
    (bind-keys ("C-k" . delete-to-end-of-line))) ; default: `kill-line'

  ;; delete line
  (when (fboundp 'delete-line)
    (bind-keys ("C-M-d" . delete-line))) ; default: `down-list'

  ;; delete word
  (when (fboundp 'delete-word)
    (bind-keys ("M-d" . delete-word))) ; default: `kill-word'
  (when (fboundp 'backward-delete-word)
    (bind-keys ("C-<backspace>" . backward-delete-word))) ; default: `backward-kill-word'

  ;; copy line
  (when (fboundp 'copy-line)
    (bind-keys ("C-x C-y" . copy-line)))

  ;; cut line
  (when (fboundp 'cut-line)
    (bind-keys ("C-x M-y" . cut-line)))

  ;; duplicate line
  (when (fboundp 'duplicate-line)
    (bind-keys ("C-x C-d" . duplicate-line))) ; default: `list-directory'

  ;; ;; kill ring browser
  ;; (when (fboundp 'browse-kill-ring)
  ;;   (bind-keys ("C-M-y" . browse-kill-ring)))

  ;; join line
  ;;(bind-keys ("C-M-j" . join-line))  ; default: `comment-indent-new-line'
  (bind-keys ("C-x C-j" . join-line))

  ;; join next line
  (when (fboundp 'join-next-line)
    ;;(bind-keys ("M-j" . join-next-line))) ; default: `indent-new-comment-line'
    (bind-keys ("C-x j" . join-next-line))) ; also: `dired-jump'

  ;; enhanced titleize-word
  (when (fboundp 'titleize-word-enhanced)
    (bind-keys ("M-t" . titleize-word-enhanced)) ; default: `transpose-words'
    (bind-keys ("M-T" . titleize-line-or-region)))

  ;; enlarge window by 5
  (when (fboundp 'enlarge-window-5)
    (bind-keys ("C-x &" . enlarge-window-5)))

  ;; shrink window by 5
  (when (fboundp 'shrink-window-5)
    (bind-keys ("C-x %" . shrink-window-5))) ; default: `View-goto-percent'

  ;; describe text properties
  (bind-keys ("C-x M-p" . describe-text-properties))

  ;; goto last change (undo)
  (when (fboundp 'goto-last-change)
    (bind-keys ("C-x C-_" . goto-last-change)))

  ;; jump to matching parenthesis
  (when (fboundp 'match-paren)
    (bind-keys ("M-(" . match-paren)))

  ;; evaluate current sexp
  (bind-keys ("C-x C-e" . eval-current-sexp)) ; default: `eval-last-sexp'
  ;; \C-\M-x defaults to `eval-defun'

  ;; evaluate all sexp's in current buffer
  (when (fboundp 'eval-sexp-buffer)
    (bind-keys ("C-x M-e" . eval-sexp-buffer)))

  ;; ;; indent current sexp
  ;; (bind-keys ("C-M-q" . indent-current-sexp)) ; default: `indent-sexp' or `indent-pp-sexp'

  ;; ;; indent all sexp's in current buffer
  ;; (when (fboundp 'indent-sexp-buffer)
  ;;   (bind-keys ("C-x M-q" . indent-sexp-buffer)))

  ;; comment and uncomment sexp's
  (when (fboundp 'comment-or-uncomment-sexp)
    (define-key emacs-lisp-mode-map (kbd "C-M-;") 'comment-or-uncomment-sexp))

  ;; indent region or thing
  (when (fboundp 'indent-region-or-thing)
    (bind-keys ("C-M-\\" . indent-region-or-thing))) ; default: `indent-region'

  ;; append equal characters up to column 80
  (when (fboundp 'append-equal-to-column-80)
    (bind-keys ("C-c =" . append-equal-to-column-80)))

  ;; append dash characters up to column 80
  (when (fboundp 'append-dash-to-column-80)
    (bind-keys ("C-c -" . append-dash-to-column-80)))

  ;; append asterisk characters up to column 80
  (when (fboundp 'append-asterisk-to-column-80)
    (bind-keys ("C-c 8" . append-asterisk-to-column-80))
    (bind-keys ("C-c *" . append-asterisk-to-column-80)))

  ;; add lisp comment block (equal)
  (when (fboundp 'insert-lisp-comment-block-equal)
    (bind-keys ("C-c C-=" . insert-lisp-comment-block-equal)))

  ;; add lisp comment block (dash)
  (when (fboundp 'insert-lisp-comment-block-dash)
    (bind-keys ("C-c C--" . insert-lisp-comment-block-dash)))

  ;; align commands
  (bind-keys ("C-c |" . align-current))

  ;; ;; hippie expand
  ;; (when (fboundp 'hippie-expand)
  ;;   (bind-keys ("M-/" . hippie-expand))) ; default: `dabbrev-expand'

  ;; complete tag
  (when (fboundp 'complete-tag)
    (bind-keys ("M-C-/" . complete-tag)))

  ;; unset set-fill-column
  (unbind-key "C-x f")                  ; default: `set-fill-column'

  ;; compare windows
  (when (fboundp 'compare-windows)
    (bind-keys ("C-c C-w" . compare-windows)))

  ;; unfill paragraph
  (when (fboundp 'unfill-paragraph)
    (bind-keys ("M-Q" . unfill-paragraph))) ; default: `fill-paragraph'

  ;; ;; double-space punctuation
  ;; (when (fboundp 'double-space-punctuation)
  ;;   (bind-keys ("C-M-q" . double-space-punctuation)))

  ;; toggle pop-up shell
  (when (and (fboundp 'pop-up-shell) (fboundp 'pop-up-shell-toggle))
    (bind-keys ("C-x C-]" . pop-up-shell-toggle))))

(init-message 3 "custom-key-bindings-standard-keys")
(custom-key-bindings-standard-keys)
;; Standard Keys:1 ends here

;; [[file:init-emacs.org::*Modes and Module Keys][Modes and Module Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Modes and Module Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Modes and Module Keys")

(defun custom-key-bindings-modes-and-modules-keys ()
  "Set custom modes and modules key bindings."

  ;; diary
  (when (fboundp 'diary)
    (bind-keys ("C-x y" . diary)))

  ;; imenu
  (bind-keys ("C-M-'" . imenu))

  ;; isearch
  ;; ;; regular expression searches
  ;; (when (fboundp 'isearch-forward-regexp)
  ;;   (bind-keys ("C-S" . isearch-forward-regexp)))
  ;; (when (fboundp 'isearch-backward-regexp)
  ;;   (bind-keys ("C-R" . isearch-backward-regexp)))
  ;; activate `occur' from isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda ()
      (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

  ;; line number mode
  (bind-keys ("C-x M-n" . display-line-numbers-mode))
  (when (fboundp 'display-line-numbers-type-toggle)
    (bind-keys ("C-x M-N" . display-line-numbers-type-toggle)))

  ;; occur
  (when (fboundp 'occur)
    ;;(bind-keys ("C-x u" . occur)) ; default: `undo'
    (bind-keys ("C-c o" . occur)))

  ;; shell
  (bind-keys ("C-x !" . shell)))

(init-message 3 "custom-key-bindings-modes-and-modules-keys")
(custom-key-bindings-modes-and-modules-keys)
;; Modes and Module Keys:1 ends here

;; [[file:init-emacs.org::*Grouped Prefix Keys][Grouped Prefix Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Grouped Prefix Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Grouped Prefix Keys")

(defun custom-key-bindings-grouped-prefix-keys ()
  "Set custom grouped prefix key bindings."

  ;; help find commands
  ;; "C-h e" defaults to `view-echo-area-messages'
  (unbind-key "C-h e")
  (define-prefix-command 'help-find-map nil "Help Find Commands")
  (bind-keys* :prefix "C-h e"
              :prefix-map help-find-map
              :menu-name "Help Find Commands"
              ("e" . view-echo-area-messages)
              ("f" . find-function)
              ("k" . find-function-on-key)
              ("l" . find-library)
              ("v" . find-variable)
              ("V" . apropos-value))

  ;; custom prefix launching point (M-space)
  (unbind-key "M-SPC")
  (define-prefix-command 'space-map nil "Space Prefix Launching Point")
  (bind-keys* :prefix "M-SPC"
              :prefix-map space-map
              :menu-name "Space Prefix Launching Point")
  (bind-keys* ("C-." . space-map))      ; in case the OS consumes M-SPC

  ;; menu
  (bind-keys :map space-map ("M-SPC" . tmm-menubar))

  ;; buffer commands
  (bind-keys :map space-map
             :prefix "b"
             :prefix-map space-buffer-map
             :menu-name "Buffer Commands")
  (when (fboundp 'switch-to-messages)
    (bind-keys :map space-buffer-map ("m" . switch-to-messages)))
  (when (fboundp 'new-scratch)
    (bind-keys :map space-buffer-map ("n" . new-scratch)))
  (when (fboundp 'new-emacs-lisp-scratch)
    (bind-keys :map space-buffer-map ("e" . new-emacs-lisp-scratch)))
  (when (fboundp 'switch-to-scratch)
    (bind-keys :map space-buffer-map ("s" . switch-to-scratch)))
  (when (fboundp 'switch-to-scratch-for-current-mode)
    (bind-keys :map space-buffer-map ("c" . switch-to-scratch-for-current-mode)))

  ;; command log commands
  (bind-keys :map space-map
             :prefix "c"
             :prefix-map space-command-log-map
             :menu-name "Command Log Commands"
             ("c" . command-log-mode-on)
             ("k" . command-log-mode-off)
             ("l" . clm/command-log-clear))

  ;; git commands
  (bind-keys :map space-map
             :prefix "g"
             :prefix-map space-git-map
             :menu-name "Git Commands"
             ("b" . magit-branch)
             ("c" . magit-branch-or-checkout)
             ("d" . magit-diff-unstaged)
             ("f" . magit-fetch)
             ("F" . magit-fetch-all)
             ("p" . magit-pull-branch)
             ("P" . magit-push-current)
             ("r" . magit-rebase)
             ("s" . magit-status))
  ;; git log commands
  (bind-keys :map space-git-map
             :prefix "l"
             :prefix-map space-git-log-map
             :menu-name "Git Log Commands"
             ("l" . magit-log-current)
             ("f" . magit-log-buffer-file))

  ;; grep commands
  (bind-keys :map space-map
             :prefix "G"
             :prefix-map space-grep-map
             :menu-name "Grep Commands")
  (when (fboundp 'grep-bin)
    (bind-keys :map space-grep-map ("b" . grep-bin)))
  (when (fboundp 'grep-clojure)
    (bind-keys :map space-grep-map ("c" . grep-clojure)))
  (when (fboundp 'grep-clisp)
    (bind-keys :map space-grep-map ("l" . grep-clisp)))
  (when (fboundp 'grep-elisp)
    (bind-keys :map space-grep-map ("e" . grep-elisp)))
  (when (fboundp 'grep-elisp-extended)
    (bind-keys :map space-grep-map ("E" . grep-elisp-extended)))
  (when (fboundp 'grep-emacs-init)
    (bind-keys :map space-grep-map ("i" . grep-emacs-init)))
  (when (fboundp 'grep-home-init)
    (bind-keys :map space-grep-map ("h" . grep-home-init)))
  (when (fboundp 'grep-org)
    (bind-keys :map space-grep-map ("o" . grep-org)))
  (when (fboundp 'grep-python)
    (bind-keys :map space-grep-map ("p" . grep-python)))
  (when (fboundp 'grep-racket)
    (bind-keys :map space-grep-map ("r" . grep-racket)))
  (when (fboundp 'grep-web)
    (bind-keys :map space-grep-map ("w" . grep-web)))

  ;; insert commands
  (bind-keys :map space-map
             :prefix "i"
             :prefix-map space-insert-map
             :menu-name "Insert Commands"
             ("d" . insert-date)
             ("t" . insert-datetime)
             ("u" . insert-uuid))
  ;; insert org-babel commands
  (bind-keys :map space-insert-map
             :prefix "o"
             :prefix-map space-insert-babel-map
             :menu-name "Insert Org-Babel Commands"
             ("b" . org-insert-literate-programming-src)
             ("c" . org-insert-literate-programming-code-block)
             ("e" . org-insert-literate-programming-src-emacs-lisp)
             ("i" . org-insert-literate-programming-init-emacs-block)
             ("k" . org-insert-literate-programming-src-kotlin)
             ("n" . org-insert-literate-programming-name)
             ("p" . org-insert-literate-programming-project-euler-problem-block)
             ("r" . org-insert-literate-programming-src-racket)
             ("s" . org-insert-literate-programming-src-sh))

  ;; miscellaneous commands
  (bind-keys :map space-map
             :prefix "m"
             :prefix-map space-miscellaneous-map
             :menu-name "Miscellaneous Commands"
             ("c" . emacs-lisp-byte-compile)
             ("d" . toggle-debug-on-error)
             ("m" . macrostep-mode)
             ("s" . toggle-case-fold-search)
             ("t" . toggle-truncate-lines)
             ("x" . regexp-builder)
             ("v" . visual-line-mode))
  ;; miscellaneous eval commands
  (bind-keys :map space-miscellaneous-map
             :prefix "e"
             :prefix-map space-miscellaneous-eval-map
             :menu-name "Eval Commands"
             ("b" . eval-buffer)
             ("r" . eval-region))
  ;; miscellaneous format commands
  (bind-keys :map space-miscellaneous-map
             :prefix "f"
             :prefix-map space-miscellaneous-format-map
             :menu-name "Format Commands"
             ("j" . json-pretty-print-buffer)
             ("x" . xml-format))

  ;; package commands
  (bind-keys :map space-map
             :prefix "p"
             :prefix-map space-package-map
             :menu-name "Package Commands"
             ("i" . package-install)
             ("l" . package-list-packages-no-fetch)
             ("L" . package-list-packages)
             ("P" . straight-pull-recipe-repositories)
             ("F" . straight-fetch-all))

  ;; run commands
  (bind-keys :map space-map
             :prefix "r"
             :prefix-map space-run-map
             :menu-name "Run Commands")
  (when (and (fboundp 'safe-load) (boundp 'emacs-home-dir))
    (defun safe-load-init-elisp ()
      (safe-load (file-truename (expand-file-name "init.el" emacs-home-dir))))
    (bind-keys :map space-run-map ("i" . safe-load-init-elisp)))

  ;; terminal commands
  (bind-keys :map space-map
             :prefix "t"
             :prefix-map space-terminal-map
             :menu-name "Terminal Commands"
             ("a" . ansi-term)
             ("e" . eshell)
             ("s" . shell)
             ("t" . term))
  (when (fboundp 'term-bash)
    (bind-keys :map space-terminal-map ("b" . term-bash)))
  (when (fboundp 'term-zsh)
    (bind-keys :map space-terminal-map ("z" . term-zsh)))
  (when (fboundp 'vterm)
    (bind-keys :map space-terminal-map ("v" . vterm)))

  ;; browse-url commands
  (bind-keys :map space-map
             :prefix "z"
             :prefix-map space-browse-url-map
             :menu-name "Browse URL Commands"
             ("." . browse-url-at-point)
             ("b" . browse-url-of-buffer)
             ("r" . browse-url-of-region)
             ("u" . browse-url)
             ("v" . browse-url-of-file)))

(init-message 3 "custom-key-bindings-grouped-prefix-keys")
(custom-key-bindings-grouped-prefix-keys)
;; Grouped Prefix Keys:1 ends here

;; [[file:init-emacs.org::*Set All Custom Key Bindings][Set All Custom Key Bindings:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Set All Custom Key Bindings
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Set All Custom Key Bindings")

(defun custom-key-bindings-set-all ()
  "Set all custom key bindings."

  (init-message 3 "custom-key-bindings-system-keys")
  (custom-key-bindings-system-keys)

  (init-message 3 "custom-key-bindings-function-keys")
  (custom-key-bindings-function-keys)

  (init-message 3 "custom-key-bindings-extended-keys")
  (custom-key-bindings-extended-keys)

  (init-message 3 "custom-key-bindings-movement-keys")
  (custom-key-bindings-movement-keys)

  (init-message 3 "custom-key-bindings-standard-keys")
  (custom-key-bindings-standard-keys)

  (init-message 3 "custom-key-bindings-modes-and-modules-keys")
  (custom-key-bindings-modes-and-modules-keys)

  (init-message 3 "custom-key-bindings-grouped-prefix-keys")
  (custom-key-bindings-grouped-prefix-keys))
;; Set All Custom Key Bindings:1 ends here

;; [[file:init-emacs.org::*Org Mode][Org Mode:1]]
;;==============================================================================
;;; Org Mode
;;==============================================================================

(init-message 1 "Org Mode")
;; Org Mode:1 ends here

;; [[file:init-emacs.org::*Setup][Setup:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Setup
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Setup")

(init-message 3 "org")

(use-package org
  :straight (:type built-in)
  :demand t
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode)
         ;;("\\.org\\.enc\\'" . org-mode)
         ("\\.org\\.cpt\\'" . org-mode))
  ;; :functions (org-insert-heading--fix-newline-bug)
  :bind* (("C-c a" . org-agenda)
          ("C-c c" . org-capture)
          ;;("C-c l" . org-store-link)
          ("C-c j" . org-babel-tangle-jump-to-org))
  :config
  (defun org-insert-heading--fix-newline-bug (orig-fun &rest args)
    "Fix extra newline bug in org."
    ;; make sure empty lines above new headline are not removed
    (if (= (point) (line-beginning-position))
        (let ((start (point)))
          (apply orig-fun args)
          (goto-char (line-beginning-position))
          (while (< (point) start)
            (newline))
          (goto-char (line-end-position)))
      (apply orig-fun args)))
  ;; advise `org-insert-heading' to fix extra newline bug
  (advice-add 'org-insert-heading :around #'org-insert-heading--fix-newline-bug)

  ;; (defun org-fixup-indentation--unindent (diff)
  ;;   "Unindent org begin/end blocks, keywords, and paragraphs."
  ;;   (save-window-excursion
  ;;     (save-mark-and-excursion
  ;;       (save-match-data
  ;;         (when (org-with-limited-levels (org-at-heading-p))
  ;;           (org-with-wide-buffer
  ;;            (narrow-to-region (line-beginning-position)
  ;;                              (save-mark-and-excursion
  ;;                                (org-with-limited-levels (outline-next-heading))
  ;;                                (point)))
  ;;            (forward-line 0)
  ;;            (org-beginning-of-line)
  ;;            (let* ((case-fold-search t)
  ;;                   (indentation (- (point) (line-beginning-position)))
  ;;                   (spacing (make-string (abs diff) ? ))
  ;;                   (indented-regexp (concat "^" spacing "[ \t]*"))
  ;;                   (text-indent t))
  ;;              (forward-line 1)
  ;;              (while (not (eobp))
  ;;                (when (re-search-forward indented-regexp (line-end-position) :noerror)
  ;;                  (let ((line-indentation (- (point) (line-beginning-position))))
  ;;                    (forward-line 0)
  ;;                    (cl-case (org-element-type (org-element-at-point))
  ;;                      ('src-block
  ;;                       (when (> diff 0)
  ;;                         (delete-char diff)))
  ;;                      ((paragraph table table-row)
  ;;                       (if (> diff 0)
  ;;                           (when (or (not text-indent)
  ;;                                     (< line-indentation indentation))
  ;;                             (delete-char diff)
  ;;                             (setq text-indent nil))
  ;;                         (if (and text-indent
  ;;                                  (>= line-indentation (- indentation diff)))
  ;;                             (delete-char (abs diff))
  ;;                           (setq text-indent nil)))))))
  ;;                (forward-line 1)))))))))
  ;; ;; advise `org-fixup-indentation' to unindent as needed
  ;; (advice-add 'org-fixup-indentation :after #'org-fixup-indentation--unindent)

  ;; define some needed, but deprecated functions

  (unless (fboundp 'org-outline-overlay-data)
    (defun org-outline-overlay-data (&optional use-markers)
      "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
      (let (beg end)
        (org-with-wide-buffer
         (delq nil
               (mapcar (lambda (x)
                         (when (eq (overlay-get x 'invisible) 'outline)
                           (setq beg (overlay-start x)
                                 end (overlay-end x))
                           (and beg end (> end beg)
                                (if use-markers
                                    (cons (copy-marker beg)
                                          (copy-marker end t))
                                  (cons beg end)))))
                       (overlays-in (point-min) (point-max))))))))

  (unless (fboundp 'org-set-outline-overlay-data)
    (defun org-set-outline-overlay-data (data)
      "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-outline-overlay-data'."
      (org-with-wide-buffer
       (org-show-all)
       (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))))

(init-message 3 "outline")

(use-package outline
  :straight (:type built-in)
  :after (org)
  :commands (outline-up-heading
             outline-forward-same-level
             outline-show-subtree)
  :config
  ;; faces
  (custom-set-faces
   `(outline-1 ((t (:foreground ,color-1))))
   `(outline-2 ((t (:foreground ,color-2))))
   `(outline-3 ((t (:foreground ,color-3))))
   `(outline-4 ((t (:foreground ,color-4))))
   `(outline-5 ((t (:foreground ,color-5))))
   `(outline-6 ((t (:foreground ,color-6))))
   `(outline-7 ((t (:foreground ,color-7))))
   `(outline-8 ((t (:foreground ,color-8)))))

  ;; advise `outline-up-heading' to suppress errors
  (advice-add 'outline-up-heading :around #'advice--ignore-errors))

;; (init-message 3 "org-pdfview")

;; needs pdftools, which annoyingly recompiles on every boot
;; (use-package org-pdfview
;;   :straight t
;;   :after (org))
;; Setup:1 ends here

;; [[file:init-emacs.org::*Configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Configuration")

(use-package org
  :straight (:type built-in)
  :custom
  ;; org directory
  (org-directory (file-truename (expand-file-name "~/org")))
  ;; indent blocks to outline node level
  (org-adapt-indentation t)
  ;; ;; do not indent blocks to outline node level
  ;; (org-adapt-indentation nil)
  ;; startup in "overview" (folded) by default
  (org-startup-folded t)
  ;; skip levels
  (org-odd-levels-only t)
  ;; hide leading stars
  (org-hide-leading-stars t)
  ;; remap disputed keys (see `org-disputed-keys')
  (org-replace-disputed-keys t)
  ;; display inline images
  (org-startup-with-inline-images t)
  ;; do not insert empty lines around headings
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  ;; use a different ellipsis indicator (than '...')
  ;;(org-ellipsis "…")
  ;;(org-ellipsis " ⤵")
  ;;(org-ellipsis " ")
  ;;(org-ellipsis " ")
  ;;(org-ellipsis " ")
  (org-ellipsis " ")
  ;; return follows links
  ;;(org-return-follows-link t)
  ;; reverse `org-beginning-of-line' and `org-end-of-line' toggle order
  ;; HOME toggles between line start and headline start
  ;; END toggles between end of tags and headline end
  (org-special-ctrl-a/e 'reversed)
  ;; show and error on invisible edits
  (org-catch-invisible-edits 'show-and-error)
  ;; use todo key selection
  (org-use-fast-todo-selection t)
  ;; use shift-left/right to change todo status without a state change
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; time stamp settings
  (org-display-custom-times t)
  (org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
  ;; todo keywords
  ;;(org-todo-keywords '("TODO(t)" "NEXT(n)" "|" "DONE(d!)"))
  ;; log note settings
  (org-log-note-headings
   '((done . "CLOSING NOTE %t")
     ;;(state . "State %-12s from %-12S %t")
     (state . "State %-12s %t")
     (note . "Note taken on %t")
     (reschedule . "Rescheduled from %S on %t")
     (delschedule . "Not scheduled, was %S on %t")
     (redeadline . "New deadline from %S on %t")
     (deldeadline . "Removed deadline, was %S on %t")
     (refile . "Refiled on %t")
     (clock-out . "")))
  :config
  ;; faces
  (custom-set-faces
   `(org-ellipsis ((t (:underline nil))))
   `(org-block ((t (:inherit shadow :foreground ,color-foreground))))
   `(org-level-1 ((t (:foreground ,color-1))))
   `(org-level-2 ((t (:foreground ,color-2))))
   `(org-level-3 ((t (:foreground ,color-3))))
   `(org-level-4 ((t (:foreground ,color-4))))
   `(org-level-5 ((t (:foreground ,color-5))))
   `(org-level-6 ((t (:foreground ,color-6))))
   `(org-level-7 ((t (:foreground ,color-7))))
   `(org-level-8 ((t (:foreground ,color-8)))))

  ;; set file apps
  (when window-system
    (setq org-file-apps (quote ((auto-mode . emacs)
                                ("\\.mm\\'" . default)
                                ("\\.x?html?\\'" . default)
                                ("\\.pdf\\'" . emacs))))) ; use internal PDF viewer

  ;; remember settings
  ;;(setq remember-handler-functions '(org-remember-handler)
  ;;      remember-annotation-functions '(org-remember-annotation))
  ;;(add-hook 'remember-mode-hook #'org-remember-apply-template)

  ;; todo keywords
  ;; (org-todo-keywords
  ;;       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
  ;;         (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELED(k@)"))

  ;; ;; add todo state map
  ;; (define-prefix-command 'org-todo-state-map)
  ;; (define-key org-mode-map (kbd "C-c x") 'org-todo-state-map)
  ;; (define-key org-todo-state-map (kbd "s") (lambda () (interactive) (org-todo "STARTED")))
  ;; (define-key org-todo-state-map (kbd "w") (lambda () (interactive) (org-todo "WAITING")))
  ;; ;;(define-key org-todo-state-map (kbd "f") (lambda () (interactive) (org-todo "DEFERRED")))
  ;; ;;(define-key org-todo-state-map (kbd "l") (lambda () (interactive) (org-todo "DELEGATED")))
  ;; (define-key org-todo-state-map (kbd "d") (lambda () (interactive) (org-todo "DONE")))
  ;; (define-key org-todo-state-map (kbd "c") (lambda () (interactive) (org-todo "CANCELED")))

  ;; save buffers after refile
  (advice-add 'org-refile :after #'org-save-all-org-buffers)

  (defun pabbrev-global-mode--org-disable (orig-fun &rest args)
    "Turn off pabbrev mode (it interferes with org mode)."
    (unless (eq major-mode 'org-mode)
      (apply orig-fun args)))
  ;; advise `pabbrev-global-mode'
  (advice-add 'pabbrev-global-mode :around #'pabbrev-global-mode--org-disable)

  ;; make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftright-final-hook #'windmove-right)

  ;; mobile org settings
  (setq org-mobile-directory (file-truename (expand-file-name "~/Dropbox/MobileOrg")))
  (setq org-mobile-inbox-for-pull (file-truename (expand-file-name "index.org" org-mobile-directory)))

  ;; ;; latex settings
  ;; ;; (minted is installed with the Ubuntu texlive-latex-extra package)
  ;; (setq org-export-latex-listings 'minted)
  ;; (add-to-list 'org-export-latex-packages-alist '("" "minted") t)

  (defun org-update-parent-cookie ()
    "Update parent TODO cookies when children entries are killed."
    (when (eq major-mode 'org-mode)
      (save-mark-and-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  ;; ;; too slow
  ;; (defun org-kill-line--fix-cookies (&optional arg)
  ;;   "Update parent todo cookies after `org-kill-line'."
  ;;   (org-update-parent-cookie))
  ;; ;; advise `org-kill-line'
  ;; (advice-add 'org-kill-line :after #'org-kill-line--fix-cookies)

  ;; ;; too slow
  ;; (defun kill-whole-line--fix-cookies (&optional arg)
  ;;   "Update parent todo cookies after `kill-whole-line'."
  ;;   (org-update-parent-cookie))
  ;; ;; advise `kill-whole-line'
  ;; (advice-add 'kill-whole-line :after #'kill-whole-line--fix-cookies)

  (defun org-kill-src-buffers (&rest args)
    "Kill temporary buffers created by `org-src-font-lock-fontify-block'."
    (dolist (buffer (buffer-list))
      (let ((buffer-name (buffer-name buffer)))
        (when (string-prefix-p " org-src-fontification:" buffer-name)
          (kill-buffer buffer)))))

  (defun org-src-font-lock-fontify-block--kill-src-buffers (lang start end)
    "Kill temporary buffers created by `org-src-font-lock-fontify-block'."
    (org-kill-src-buffers))
  ;; advise `org-src-font-lock-fontify-block'
  (advice-add 'org-src-font-lock-fontify-block :after #'org-src-font-lock-fontify-block--kill-src-buffers)

  (defun before-save-hook--update-last-modified-property ()
    "Hook to update last modified property on save."
    (when (and buffer-file-name
               (string= (file-name-extension buffer-file-name) "org"))
      (org-update-last-modified-property)))
  (add-hook 'before-save-hook #'before-save-hook--update-last-modified-property)

  (defun after-save-hook--generate-init-emacs-elisp-file ()
    "Hook to generate init-emacs.el file on save."
    (when (and buffer-file-name
               (string= (file-truename buffer-file-name) init-emacs-true-file-name))
      ;;(org-babel-generate-elisp-file init-emacs-true-file-name nil t)
      (if (fboundp 'org-babel-tangle-file-async)
          (org-babel-tangle-file-async init-emacs-true-file-name)
        (org-babel-tangle-file init-emacs-true-file-name))))
  (add-hook 'after-save-hook #'after-save-hook--generate-init-emacs-elisp-file :append))
;; Configuration:1 ends here

;; [[file:init-emacs.org::*Agenda][Agenda:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Agenda
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Agenda")

(use-package org
  :when (file-exists-p org-directory)
  :straight (:type built-in)
  :config
  ;; agenda key bindings
  ;;(define-key org-agenda-mode-map (kbd "C-n") 'next-line)
  ;;(define-key org-agenda-keymap (kbd "C-n") 'next-line)
  ;;(define-key org-agenda-mode-map (kbd "C-p") 'previous-line)
  ;;(define-key org-agenda-keymap (kbd "C-p") 'previous-line)

  ;; record time when todo's are marked done
  (setq org-log-done 'time)

  ;; agenda files
  (setq org-agenda-file-regexp "agenda.*\\.org\\'"
        org-agenda-files (mapcar (lambda (x) (expand-file-name x (file-name-as-directory org-directory)))
                                 (cl-remove-if-not (lambda (x) (string-match org-agenda-file-regexp x))
                                                   (directory-files org-directory))))

  ;; show 7 days in agenda view
  (setq org-agenda-span 7)

  ;; show deadlines within 14 days
  (setq org-deadline-warning-days 14)

  ;; show all dates (even empty ones)
  (setq org-agenda-show-all-dates t)

  ;; do not show entries marked done
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; start agenda view on current day
  (setq org-agenda-start-on-weekday nil)

  ;; store new notes at the beginning of files
  (setq org-reverse-note-order t)

  ;; quick tag selection
  (setq org-fast-tag-selection-single-key 'expert)

  ;; quick tag keys
  (setq org-agenda-custom-commands
        (quote (("d" todo "DONE" nil)
                ;;("d" todo "DONE|DEFERRED|CANCELLED" nil)
                ("w" todo "WAITING" nil)
                ;;("l" todo "DELEGATED" nil)
                ("W" agenda "" ((org-agenda-ndays 21)))
                ("A" agenda ""
                 ((org-agenda-skip-function
                   (lambda ()
                     (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
                  (org-agenda-ndays 1)
                  (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                ("u" alltodo ""
                 ((org-agenda-skip-function
                   (lambda ()
                     (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^>\n]+>")))
                  (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

  ;; fix bug
  (when (not (boundp 'org-called-interactively-p))
    (defalias 'org-called-interactively-p 'called-interactively-p))

  ;; archive done tasks
  (defun org-agenda-archive-done-tasks ()
    "Archive DONE tasks."
    (interactive)
    (org-map-entries #'org-archive-subtree "/DONE" 'file))

  ;; auto-save archive files
  (when (boundp 'org-archive-subtree-save-file-p)
    (setq org-archive-subtree-save-file-p t)))
;; Agenda:1 ends here

;; [[file:init-emacs.org::*Alerts][Alerts:1]]
(init-message 2 "Org Mode: Alerts")

;; (use-package org-alert
;;   :straight t
;;   :after (org)
;;   :custom (alert-default-style 'notifications)
;;   :config
;;   (setq org-alert-interval 300
;;         org-alert-notification-title "Org Reminder"))

;; throws error: void-function -orfn
;; (use-package org-wild-notifier
;;   :straight t
;;   :after (org)
;;   :custom
;;   (alert-default-style 'notifications)
;;   (org-wild-notifier-alert-time '(1 10 30))
;;   (org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
;;   (org-wild-notifier-notification-title "Org Reminder")
;;   :init (org-wild-notifier-mode 1))

;; (use-package org-notify
;;   :straight t
;;   :after (org)
;;   :init (org-notify-start))
;; Alerts:1 ends here

;; [[file:init-emacs.org::*Modules][Modules:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Modules
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Modules")

(org-load-modules-maybe t)
;; Modules:1 ends here

;; [[file:init-emacs.org::*Functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::*org-get-property-list][org-get-property-list:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-get-property-list
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-get-property-list")

(defun org-get-property-list (&optional property buffer)
  "Return an association list of org properties matching PROPERTY in BUFFER.

PROPERTY is used to `string-match' for properties to
return (defaults to \"PROPERTY\"). Multiple properties can be
queried in one call by using a regular expression. (E.g.
\"\\(AUTHOR\\|EMAIL\\|TITLE\\)\")

If BUFFER is nil, current buffer is used."
  (let ((property (or property "PROPERTY")))
    (with-current-buffer (or buffer (current-buffer))
      (org-element-map (org-element-parse-buffer) 'keyword
        (lambda (x) (let ((key (org-element-property :key x)))
                      (when (string-match property key)
                        (cons key (org-element-property :value x)))))))))
;; org-get-property-list:1 ends here

;; [[file:init-emacs.org::*org-get-element-tree][org-get-element-tree:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-get-element-tree
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-get-element-tree")

(defun org-get-element-tree (types &optional buffer)
  "Return a tree structure representing the org levels found in
BUFFER for given TYPES.

TYPES is the same symbol or list of symbols used with
`org-element-map'.

If BUFFER is nil, current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) types
      (lambda (x) (cons (org-element-property :level x)
                        (org-element-property :raw-value x))))))
;; org-get-element-tree:1 ends here

;; [[file:init-emacs.org::*org-get-file-data][org-get-file-data:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-get-file-data
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-get-file-data")

(defun org-get-file-data (file &optional path)
  "Return tree structure version of given Org FILE.

PATH is an optional list of headlines to match starting from the
top level.

Output format:

  (((\"KEY1\" . VALUE1)
    (\"KEY2\" . VALUE2)
    (\"KEY3\" . VALUE3))
   ((HEADLINE1)
    (HEADLINE2
     (HEADLINE21 . BODY21))
    (HEADLINE3
     (HEADLINE31
      (HEADLINE311 . BODY311)
      (HEADLINE312 . BODY312))
     (HEADLINE32
      (HEADLINE321 . BODY321)
      (HEADLINE322 . BODY322)))))"
  (let* ((property-headline-regexp "^[ \t]*\\** Org$")
         (property-regexp "^[ \t]*#\\+\\(.*\\): \\(.*\\)$")
         (property-drawer-regexp "[ \t]*:PROPERTIES:.*:END:[ \t]*")
         (headline-regexp "^\\(\*+ \\)\\(.*\\)$")
         (property-alist nil)
         (property-section t)
         (level 0)
         (tree (cons nil nil))
         (start tree)
         (stack nil)
         (matches path)
         (path-level (length path)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        ;;(message "%S" tree)
        (cond
         ;; ignore property folder
         ((and (bobp)
               (looking-at property-headline-regexp))
          nil)
         ;; add properties
         ((and property-section
               (looking-at property-regexp))
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (push (cons key value) property-alist)))
         ;; add folders
         ((looking-at headline-regexp)
          (setq property-section nil)
          (let ((headline-level (/ (length (match-string-no-properties 1))
                                   (if org-odd-levels-only 2 1)))
                (headline-value (match-string-no-properties 2))
                (segment (car path)))
            (when (and path
                       (not matches)
                       (>= path-level headline-level))
              (setq matches path))
            (when (or (and (not matches)
                           (> headline-level path-level))
                      (string= headline-value (car matches)))
              (when (string= headline-value (car matches))
                (setq matches (cdr matches)))
              (cond
               ((> headline-level level)
                (setcdr tree (cons (cons headline-value nil) nil))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree))
                (setq level headline-level))
               ((= headline-level level)
                (setq tree (pop stack))
                (setcdr tree (cons (cons headline-value nil) nil))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree)))
               ((< headline-level level)
                (while (< headline-level level)
                  (setq tree (pop stack))
                  (setq level (1- level)))
                (setq tree (pop stack))
                (setcdr tree (cons (cons headline-value nil) nil))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree))
                (setq level headline-level))))))
         ;; add body sections
         ((and (not matches)
               (>= level path-level))
          (setq property-section nil)
          (when (> (length start) 1)
            (let ((body "")
                  (point (point)))
              (while (and (not (eobp))
                          (not (looking-at property-regexp))
                          (not (looking-at headline-regexp)))
                (when (> (length body) 0)
                  (setq body (concat body "\n")))
                (setq body (concat body
                                   (replace-regexp-in-string "^[ \t\n]*" ""
                                                             (buffer-substring-no-properties
                                                              (line-beginning-position)
                                                              (line-end-position)))))
                (forward-line 1))
              ;; add unless body is drawer properties
              (when (>
                     (length
                      (replace-regexp-in-string property-drawer-regexp ""
                                                (replace-regexp-in-string "\n" "" body)))
                     0)
                (setcdr tree (cons (replace-regexp-in-string "[ \t]*$" "" body) nil))
                (setq tree (cdr tree)))
              (forward-line 0)
              (when (> (point) point)
                (forward-line -1)))))
         (t
          (setq property-section nil)))
        (forward-line 1))
      (cons property-alist (cdr start)))))
;; org-get-file-data:1 ends here

;; [[file:init-emacs.org::*org-get-buffer-data][org-get-buffer-data:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-get-buffer-data
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-get-buffer-data")

(defun org-get-buffer-data (buffer &optional path with-markers)
  "Return tree structure version of given Org BUFFER.

PATH is an optional list of headlines to match starting from the
top level.

If WITH-MARKERS is non-nil, include `point-marker' after HEADLINE
values in output.

Output format:

  (((\"KEY1\" . VALUE1)
    (\"KEY2\" . VALUE2)
    (\"KEY3\" . VALUE3))
   ((HEADLINE1)
    (HEADLINE2
     (HEADLINE21 . BODY21))
    (HEADLINE3
     (HEADLINE31
      (HEADLINE311 . BODY311)
      (HEADLINE312 . BODY312))
     (HEADLINE32
      (HEADLINE321 . BODY321)
      (HEADLINE322 . BODY322)))))

Output format if WITH-MARKERS is non-nil:

  (((\"KEY1\" . VALUE1)
    (\"KEY2\" . VALUE2)
    (\"KEY3\" . VALUE3))
   ((HEADLINE1 . MARKER1)
    (HEADLINE2 . MARKER2
     (HEADLINE21 . MARKER21 . BODY21))
    (HEADLINE3 . MARKER3
     (HEADLINE31 . MARKER31
      (HEADLINE311 . MARKER311 . BODY311)
      (HEADLINE312 . MARKER312 . BODY312))
     (HEADLINE32 . MARKER32
      (HEADLINE321 . MARKER321 . BODY321)
      (HEADLINE322 . MARKER322 . BODY322)))))"
  (let* ((property-headline-regexp "^[ \t]*\\** Org$")
         (property-regexp "^[ \t]*#\\+\\(.*\\): \\(.*\\)$")
         (property-drawer-regexp "[ \t]*:PROPERTIES:.*:END:[ \t]*")
         (headline-regexp "^\\(\*+ \\)\\(.*\\)$")
         (property-alist nil)
         (property-section t)
         (level 0)
         (tree (cons nil nil))
         (start tree)
         (stack nil)
         (matches path)
         (path-level (length path)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (eobp))
        ;;(message "%S" tree)
        (cond
         ;; ignore property folder
         ((and (bobp)
               (looking-at property-headline-regexp))
          nil)
         ;; add properties
         ((and property-section
               (looking-at property-regexp))
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (push (cons key value) property-alist)))
         ;; add folders
         ((looking-at headline-regexp)
          (setq property-section nil)
          (let ((headline-level (/ (length (match-string-no-properties 1))
                                   (if org-odd-levels-only 2 1)))
                (headline-value (match-string-no-properties 2))
                (segment (car path)))
            (when (and path
                       (not matches)
                       (>= path-level headline-level))
              (setq matches path))
            (when (or (and (not matches)
                           (> headline-level path-level))
                      (string= headline-value (car matches)))
              (when (string= headline-value (car matches))
                (setq matches (cdr matches)))
              (cond
               ((> headline-level level)
                (if with-markers
                    (setcdr tree (cons (cons (cons headline-value (point-marker)) nil) nil))
                  (setcdr tree (cons (cons headline-value nil) nil)))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree))
                (setq level headline-level))
               ((= headline-level level)
                (setq tree (pop stack))
                (if with-markers
                    (setcdr tree (cons (cons (cons headline-value (point-marker)) nil) nil))
                  (setcdr tree (cons (cons headline-value nil) nil)))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree)))
               ((< headline-level level)
                (while (< headline-level level)
                  (setq tree (pop stack))
                  (setq level (1- level)))
                (setq tree (pop stack))
                (if with-markers
                    (setcdr tree (cons (cons (cons headline-value (point-marker)) nil) nil))
                  (setcdr tree (cons (cons headline-value nil) nil)))
                (setq tree (cdr tree))
                (push tree stack)
                (setq tree (car tree))
                (setq level headline-level))))))
         ;; add body sections
         ((and (not matches)
               (>= level path-level))
          (setq property-section nil)
          (when (> (length start) 1)
            (let ((body "")
                  (point (point)))
              (while (and (not (eobp))
                          (not (looking-at property-regexp))
                          (not (looking-at headline-regexp)))
                (when (> (length body) 0)
                  (setq body (concat body "\n")))
                (setq body (concat body
                                   (replace-regexp-in-string "^[ \t\n]*" ""
                                                             (buffer-substring-no-properties
                                                              (line-beginning-position)
                                                              (line-end-position)))))
                (forward-line 1))
              ;; add unless body is drawer properties
              (when (>
                       (length
                        (replace-regexp-in-string property-drawer-regexp ""
                                                  (replace-regexp-in-string "\n" "" body)))
                       0)
                (setcdr tree (cons (replace-regexp-in-string "[ \t]*$" "" body) nil))
                (setq tree (cdr tree)))
              (forward-line 0)
              (when (> (point) point)
                (forward-line -1)))))
         (t
          (setq property-section nil)))
        (forward-line 1))
      (cons property-alist (cdr start)))))
;; org-get-buffer-data:1 ends here

;; [[file:init-emacs.org::*org-safe-meta][org-safe-meta:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-safe-meta
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-safe-meta")

(defmacro org-safe-meta-function (org-meta-function safe-function)
  "Return SAFE-FUNCTION version of ORG-META-FUNCTION."
  `(defun ,(intern safe-function) (&optional arg)
     ,(concat "Call `" org-meta-function "' ignoring any errors.")
     (interactive)
     (let ((pos (point)))
       (condition-case nil
           (,(intern org-meta-function) arg)
         ('error
          (goto-char pos))))))

(defmacro org-safe-shiftmeta-function (org-shiftmeta-function safe-function)
  "Return SAFE-FUNCTION version of ORG-SHIFTMETA-FUNCTION."
  `(defun ,(intern safe-function) ()
     ,(concat "Call `" org-shiftmeta-function "' ignoring any errors.")
     (interactive)
     (let ((pos (point)))
       (condition-case nil
           (,(intern org-shiftmeta-function))
         ('error
          (goto-char pos))))))

;; safe versions of org-meta functions
(org-safe-meta-function "org-metaleft" "org-safe-metaleft")
(org-safe-meta-function "org-metaright" "org-safe-metaright")
(org-safe-meta-function "org-metadown" "org-safe-metadown")
(org-safe-meta-function "org-metaup" "org-safe-metaup")

;; safe versions of org-shiftmeta functions
(org-safe-shiftmeta-function "org-shiftmetaleft" "org-safe-shiftmetaleft")
(org-safe-shiftmeta-function "org-shiftmetaright" "org-safe-shiftmetaright")
(org-safe-shiftmeta-function "org-shiftmetadown" "org-safe-shiftmetadown")
(org-safe-shiftmeta-function "org-shiftmetaup" "org-safe-shiftmetaup")
;; org-safe-meta:1 ends here

;; [[file:init-emacs.org::*org-sort-multi][org-sort-multi:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-sort-multi
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-sort-multi")

(defun org-sort-multi (&rest sort-types)
  "Multiple sorts on a certain level of an outline tree, or plain list items.

SORT-TYPES is a list where each entry is either a character or a
cons pair (BOOL . CHAR), where BOOL is whether or not to sort
case-sensitively, and CHAR is one of the characters defined in
`org-sort-entries-or-items'. Entries are applied in back to front
order.

Example: To sort first by TODO status, then by priority, then by
date, then alphabetically (case-sensitive) use the following
call:

  (org-sort-multi '(?o ?p ?t (t . ?a))"
  (interactive)
  (dolist (x (nreverse sort-types))
    (when (characterp x)
      (setq x (cons nil x)))
    (ignore-errors
      (org-sort-entries (car x) (cdr x)))))
;; org-sort-multi:1 ends here

;; [[file:init-emacs.org::*org-sort-current][org-sort-current:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-sort-current
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-sort-current")

(defun org-sort-current (&rest sort-types)
  "Sort the current org level.

SORT-TYPES is a list where each entry is either a character or a
cons pair (BOOL . CHAR), where BOOL is whether or not to sort
case-sensitively, and CHAR is one of the characters defined in
`org-sort-entries-or-items'. Entries are applied in back to front
order.

If entry at point has TODO and PRIORITY tags, then default
SORT-TYPE is \"?o ?p\" which is to sort by TODO status, then by
priority. Otherwise, default SORT-TYPE is \"?a\" which is to sort
alphabetically."
  (interactive)
  (when (string= mode-name "Org")
    (let ((sort-types (or sort-types
                          (if (and (org-entry-get nil "TODO")
                                   (org-entry-get nil "PRIORITY"))
                              '(?o ?p)
                            '((nil . ?a))))))
      (save-mark-and-excursion
        (goto-char (line-beginning-position))
        (cl-case (car (org-element-at-point))
          ('headline
           (condition-case nil
               (outline-up-heading 1)
             ('error (forward-line 0)))
           (let ((beg (point)))
             (while (and (not (bobp)) (not (eobp)) (<= (point) beg))
               (condition-case nil
                   (outline-forward-same-level 1)
                 ('error
                  (condition-case nil
                      (outline-up-heading 1)
                    ('error (goto-char (point-max)))))))
             (unless (> (point) beg)
               (goto-char (point-max)))
             (let ((end (point)))
               (goto-char beg)
               (apply #'org-sort-multi sort-types)
               (goto-char end)
               (when (eobp)
                 (forward-line -1))
               (when (looking-at "^\\s-*$")
                 (delete-line))
               (goto-char beg)
               (dotimes (x 2) (org-cycle)))))
          ('paragraph
           (let* ((plist (cadr (org-element-at-point)))
                  (beg (plist-get plist :contents-begin))
                  (end (plist-get plist :contents-end)))
             (sort-lines nil beg end)))
          (t
           (org-sort-list nil ?a)))))))
;; org-sort-current:1 ends here

;; [[file:init-emacs.org::*org-fill-element--adapt-indentation][org-fill-element--adapt-indentation:1]]
(defun org-fill-element--adapt-indentation (orig-fun &rest args)
  "Modify `fill-column' based on current org block indentation."
  (with-syntax-table org-mode-transpose-word-syntax-table
    (let* ((element (save-excursion (end-of-line) (org-element-at-point)))
           (type (org-element-type element)))
      (if (or (eq type 'paragraph)
              (eq type 'comment-block)
              (eq type 'comment))
          (let ((indent-min fill-column)
                (fc fill-column)
                (point (point)))
            ;; goto start of element block
            (while (and (not (bobp))
                        (eq type (org-element-type (org-element-at-point)))
                        (forward-line -1)))
            (unless (eq type (org-element-type (org-element-at-point)))
              (forward-line 1))
            ;; find minimum indent of entire element block
            (while (and (not (eobp))
                        (eq type (org-element-type (org-element-at-point))))
              (when (and (not (= (point-at-bol) (point-at-eol)))
                         (re-search-forward "^ *" (line-end-position) :noerror)
                         (< (current-column) indent-min))
                (setq indent-min (current-column)))
              (forward-line 1))
            (goto-char point)
            ;; set temporary `fill-column'
            (let ((fill-column (+ fill-column indent-min)))
              (apply orig-fun args)))
        (apply orig-fun args)))))
;; advise `org-fill-element' to set `fill-column' correctly
(advice-add 'org-fill-element :around #'org-fill-element--adapt-indentation)
;; org-fill-element--adapt-indentation:1 ends here

;; [[file:init-emacs.org::*org-copy-to-clipboard][org-copy-to-clipboard:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-copy-to-clipboard
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-copy-to-clipboard")

(defun org-copy-to-clipboard (&optional beg end)
  "Copy `org-mode' region (or entire buffer) to the `kill-ring'
and X clipboard, indenting and cleaning up links."
  (interactive)
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        (buffer (current-buffer)))
    (deactivate-mark)
    (with-temp-buffer
      (insert-buffer-substring buffer beg end)
      (untabify (point-min) (point-max))
      (let ((indent-min fill-column))
        ;; replace asterisks with spaces and determine minimum indent
        (goto-char (point-min))
        (while (re-search-forward "^\\(*+ \\| *- \\)" nil :noerror)
          (replace-match (concat (make-string (- (match-end 0) (match-beginning 0) 2) ? ) "- "))
          (when (< (- (current-column) 2) indent-min)
            (setq indent-min (- (current-column) 2)))
          (goto-char (line-end-position)))
        ;; if there are no headings in block, find minimum indent
        (when (= indent-min fill-column)
          (goto-char (point-min))
          (while (not (eobp))
            (when (and (not (= (point-at-bol) (point-at-eol)))
                       (re-search-forward "^ *" (line-end-position) :noerror)
                       (< (current-column) indent-min))
              (setq indent-min (current-column)))
            (forward-line 1)))
        ;; indent to minimum indent
        (when (> indent-min 0)
          (goto-char (point-min))
          (while (re-search-forward (concat "^" (make-string indent-min ? )) nil :noerror)
            (replace-match "")
            (goto-char (line-end-position))))
        ;; replace links
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" nil :noerror)
          (replace-match (match-string 1)))
        (clipboard-kill-region (point-min) (point-max))))))
;; org-copy-to-clipboard:1 ends here

;; [[file:init-emacs.org::*org-fix-custom-ids][org-fix-custom-ids:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-fix-custom-ids
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-fix-custom-ids")

(defun org-fix-custom-ids (&optional beg end)
  "Fix CUSTOM_ID tags in region (or entire buffer), by lower casing them and
replacing spaces with dashes."
  (interactive)
  (let ((case-fold-search t)
        (beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        (buffer (current-buffer)))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:PROPERTIES:$" nil :noerror)
            (goto-char (line-beginning-position))
            (forward-char 1)
            (while (and (not (looking-at "^[ \t]*:END:$"))
                        (re-search-forward "^[ \t]*:CUSTOM_ID: " (line-end-position) :noerror))
              (let ((pos (point)))
                (downcase-region pos (line-end-position))
                (while (re-search-forward " " (line-end-position) :noerror)
                  (replace-match "-"))
                (goto-char pos)
                (while (re-search-forward ":" (line-end-position) :noerror)
                  (replace-match "-"))
                (goto-char (line-beginning-position))
                (forward-line 1)))))))))
;; org-fix-custom-ids:1 ends here

;; [[file:init-emacs.org::*org-update-last-modified-property][org-update-last-modified-property:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-update-last-modified-property
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-update-last-modified-property")

(defun org-update-last-modified-property ()
  "Update value of LAST-PROPERTY property to current timestamp,
if found and buffer has been modified."
  (when (buffer-modified-p)
    (save-mark-and-excursion
      (save-match-data
        (let ((case-fold-search t))
          (goto-char (point-min))
          (when (re-search-forward "^[ \t]*#\\+LAST_MODIFIED: \\(.*\\)$" nil :noerror)
            (replace-match (format-time-string "%Y-%m-%d %H:%M" nil t) t t nil 1)))))))
;; org-update-last-modified-property:1 ends here

;; [[file:init-emacs.org::*org-export-to-json][org-export-to-json:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-export-to-json
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-export-to-json")

;; adapted from https://github.com/mattduck/org-toggl-py/blob/master/org-export-json.el
(use-package json
  :straight (:type built-in)
  :commands (json-encode)
  :config
  (defun org-export-to-json (&optional output beg end)
    "Export the outline as JSON.

IF OUTPUT is nil, a default output buffer will be created and exported into.
If OUTPUT is non-nil, create a buffer with that name and export to it.
If OUTPUT is the symbol 'string, exported data is returned as a string.
If BEG and END are given, only that region is exported."
    (interactive)
    (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
          (end (or end (if (use-region-p) (region-end) (point-max)))))
      (deactivate-mark)
      (save-mark-and-excursion
        (save-restriction
          (narrow-to-region beg end)
          (let ((tree (org-element-parse-buffer)))
            (org-element-map tree (append org-element-all-elements
                                          org-element-all-objects
                                          '(plain-text))
              (lambda (x)
                (when (org-element-property :parent x)
                  (org-element-put-property x :parent "none"))
                (when (org-element-property :structure x)
                  (org-element-put-property x :structure "none"))))
            (if (eq output 'string)
                (json-encode tree)
              (let ((buffer (generate-new-buffer (or output (concat buffer-file-name ".json")))))
                (set-buffer buffer)
                (insert (json-encode tree))
                (switch-to-buffer buffer)))))))))
;; org-export-to-json:1 ends here

;; [[file:init-emacs.org::*org-toggle-headline-checkbox][org-toggle-headline-checkbox:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-toggle-headline-checkbox
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-toggle-headline-checkbox")

(defun org-toggle-headline-checkbox (&optional beg end)
  "Toggle between an Org headline and checkbox on current line or region."
  (interactive)
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point))))
        (end (or end (if (use-region-p) (region-end) (point)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-match-data
        (goto-char beg)
        (while (<= (point) end)
          (goto-char (line-beginning-position))
          (if (re-search-forward "^\\*+ " (line-end-position) :noerror)
              (replace-match (concat
                              (make-string (- (point) (line-beginning-position) 2) ? )
                              "- [ ] "))
            (if (re-search-forward "^ *- \\[ \\] " (line-end-position) :noerror)
                (replace-match (concat
                                (make-string (- (point) (line-beginning-position) 5) ?*)
                                " "))))
          (forward-line 1))))))
;; org-toggle-headline-checkbox:1 ends here

;; [[file:init-emacs.org::*org-table-remove-commas][org-table-remove-commas:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-table-remove-commas
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-table-remove-commas")

(defun org-table-remove-commas ()
  "Remove all commas in current Org table."
  (interactive)
  (save-mark-and-excursion
    (save-match-data
      (goto-char (org-table-begin))
      (while (re-search-forward "," (org-table-end) :noerror)
        (replace-match "")))))
;; org-table-remove-commas:1 ends here

;; [[file:init-emacs.org::*org-days-between-dates][org-days-between-dates:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-days-between-dates
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-days-between-dates")

(defun org-days-between-dates (beg end)
  "Return the number of days between BEG (inclusive) and END (exclusive).

Where BEG and END dates are in one of these formats:

  YYYY
  YYYYMM
  YYYYMMDD
  YYYY-MM
  YYYY-MM-DD"
  (cl-labels
      ((convert-date (date)
                     (calendar-absolute-from-gregorian (org-date-to-gregorian date)))
       (pad-date (date)
                 (let ((date (if (stringp date) date (number-to-string date))))
                   (cl-case (length date)
                     (4 (concat date "-01-01"))
                     (6 (concat (substring date 0 4) "-" (substring date 4) "-01"))
                     (7 (concat date "-01"))
                     (8 (concat (substring date 0 4) "-" (substring date 4 6) "-" (substring date 6)))
                     (10 date)))))
    (let ((beg (convert-date (pad-date beg)))
          (end (convert-date (pad-date end)))
          x)
      (when (> beg end)
        (setq x beg
              beg end
              end x))
      (- end beg))))
;; org-days-between-dates:1 ends here

;; [[file:init-emacs.org::*org-babel-tangle-block][org-babel-tangle-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-babel-tangle-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-babel-tangle-block")

(defun org-babel-tangle-block ()
  "Tangle blocks for the tangle file of the block at point."
  (interactive)
  (org-babel-tangle '(16)))
;; org-babel-tangle-block:1 ends here

;; [[file:init-emacs.org::*org-babel-tangle-file-async][org-babel-tangle-file-async:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-babel-tangle-file-async
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-babel-tangle-file-async")

(defun org-babel-tangle-file-async (file &optional target-file lang-re attempt)
  "Asynchronous version of `org-babel-tangle-file'.

ATTEMPT is used internally to determine how many tangle attempts have been made."
  (interactive)
  (let ((attempt (if attempt (1+ attempt) 1))
        (lock-file (expand-file-name "emacs-tangle-file-async-lock-file"
                                     temporary-file-directory))
        (run-file (expand-file-name "emacs-tangle-file-async-run-file"
                                    temporary-file-directory)))
    (if (file-exists-p lock-file)
        (progn
          (message "Tangle running: %s" file)
          (when (not (file-exists-p run-file))
            (make-empty-file run-file)))
      (progn
        (message "Tangle started: %s" file)
        (make-empty-file lock-file)
        (eval
         `(async-spinner
           (lambda ()
             (require 'ob-tangle)
             (when (not (file-exists-p ,run-file))
               (make-empty-file ,run-file))
             (while (file-exists-p ,run-file)
               (delete-file ,run-file)
               (org-babel-tangle-file ,file ,target-file ,lang-re)))
           (lambda (result)
             (message "Tangle finished: %s" ,file)
             (delete-file ,lock-file))))))))
;; org-babel-tangle-file-async:1 ends here

;; [[file:init-emacs.org::*org-copy-tangled-sections][org-copy-tangled-sections:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-copy-tangled-sections
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-copy-tangled-sections")

(defun org-copy-tangled-sections (source-file target-file sections &optional prefix)
  "Copy section blocks from tangled SOURCE-FILE to TARGET-FILE
that match the names in SECTIONS.

If PREFIX is non-nil, insert it verbatim at the top of the
TARGET-FILE."
  ;; create buffer for target-file
  (let ((buffer (find-file-noselect target-file)))
    (set-buffer buffer)
    (erase-buffer)
    ;; insert header
    (insert ";; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-\n")
    (insert ";;==============================================================================\n")
    (insert ";;; " (file-name-nondirectory target-file) "\n")
    (insert ";;\n")
    (insert ";; This file was generated from " (file-name-nondirectory source-file) ".\n")
    (insert ";;==============================================================================\n\n")
    ;; insert optional prefix
    (when prefix
      (insert (concat prefix "\n")))
    ;; insert section blocks
    (with-temp-buffer
      (insert-file-contents source-file)
      (dolist (section sections)
        (goto-char (point-min))
        (re-search-forward (concat "^[ \t]*" section "$"))
        (goto-char (line-beginning-position))
        (forward-line -1)
        (unless (looking-at "^[ \t]*;;--")
          (forward-line 1))
        (let ((beg (point))
              (end (progn
                     (re-search-forward (concat "^[ \t]*;; .* ends here$"))
                     (goto-char (line-beginning-position))
                     (forward-line 1)
                     (point))))
          (append-to-buffer buffer beg end))))
    ;; remove all `init-message' calls
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*(init-message [0-9] \"\\(.*\\)\")$" nil :noerror)
      (delete-region (1- (line-beginning-position)) (1+ (line-end-position))))
    ;; remove all ';; ... ends here' lines
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*;; .* ends here$" nil :noerror)
      (goto-char (line-beginning-position))
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char (point-max))
    ;; insert footer
    (insert ";;==============================================================================\n")
    (insert ";;; " (file-name-nondirectory target-file) " ends here\n")
    (insert ";;==============================================================================\n")
    ;; save and close buffer
    (save-buffer)
    (kill-buffer buffer)))
;; org-copy-tangled-sections:1 ends here

;; [[file:init-emacs.org::*org-screenshot][org-screenshot:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-screenshot
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-screenshot")

;; take a screenshot and insert org link
(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; (let ((file-name
  ;;        (concat
  ;;         (make-temp-name
  ;;          (concat (buffer-file-name)
  ;;                  "_"
  ;;                  (format-time-string "%Y%m%d_%H%M%S_")))
  ;;         ".png")))
  (let ((file-name
         (concat (buffer-file-name)
                 "_"
                 (format-time-string "%Y%m%d_%H%M%S")
                 ".png")))
    (call-process "import" nil nil nil file-name)
    (insert (concat "[[" file-name "]]\n"))))
;; org-screenshot:1 ends here

;; [[file:init-emacs.org::*Hook][Hook:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Hook
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Hook")

(defun custom-org-mode-hook ()
  ;; remappings
  (bind-keys :map org-mode-map
             ([remap org-metaleft] . org-safe-metaleft)
             ([remap org-metaright] . org-safe-metaright)
             ([remap org-metadown] . org-safe-metadown)
             ([remap org-metaup] . org-safe-metaup)
             ([remap org-shiftmetaleft] . org-safe-shiftmetaleft)
             ([remap org-shiftmetaright] . org-safe-shiftmetaright)
             ([remap org-shiftmetadown] . org-safe-shiftmetadown)
             ([remap org-shiftmetaup] . org-safe-shiftmetaup))
  ;; local key bindings
  (bind-keys :map org-mode-map
             ("M-n" . scroll-up)
             ("M-p" . scroll-down)
             ("M-W" . org-copy-to-clipboard)
             ("C-M-b" . org-metaleft)
             ("C-M-f" . org-metaright)
             ("C-M-n" . org-metadown)
             ("C-M-p" . org-metaup)
             ("C-M-B" . org-shiftmetaleft)
             ("C-M-F" . org-shiftmetaright)
             ("C-M-N" . org-shiftmetadown)
             ("C-M-P" . org-shiftmetaup)
             ;;("C-c C-<return>" . org-insert-heading)
             ("C-c a" . org-agenda)
             ("C-c l" . org-store-link)
             ("C-c m" . org-insert-todo-heading)
             ("C-c p" . org-priority) ; "C-c ," gets overridden by `semantic-complete-analyze-inline'
             ("C-c s" . org-sort-current) ; sort current level
             ("C-c z" . org-agenda-archive-done-tasks) ; archive done tasks
             ;;("C-c C-j" . counsel-org-goto)            ; default: `org-goto'
             ("C-c C-j" . consult-outline)             ; default: `org-goto'
             ;;("C-c C-z" . switch-to-lisp) ; default: `org-add-note'
             ("C-c C-z" . geiser-mode-switch-to-repl) ; default: `org-add-note'
             ("C-c C-x C-l" . org-toggle-link-display) ; toggle showing or hiding links
             ("C-c C-x t" . org-toggle-headline-checkbox) ; toggle between headline and checkbox
             ("C-c C-x T" . org-toggle-literate-programming-code-block) ; toggle literate programming code block on/off
             ("C-c C-x F" . org-fix-literate-programming-heading) ; fix literate programming heading of current org section
             ("C-c C-v q" . org-babel-tangle-block) ; tangle current source block
             ("C-c C-v C-q" . org-babel-tangle-block)) ; tangle current source block
  ;; custom movement keys
  (custom-key-bindings-movement-keys org-mode-map)

  ;; ;; work functions
  ;; (when (fboundp 'work-linkify-jira-card)
  ;;   (bind-keys :map org-mode-map ("C-c C-x L" . work-linkify-jira-card)))

  ;; make sure tabs are not inserted
  (setq indent-tabs-mode nil)

  ;; turn off auto-fill mode
  (turn-off-auto-fill)

  ;; turn off auto-save mode
  (auto-save-mode nil)

  ;; turn off flyspell
  ;; (when (fboundp 'flyspell-mode-off)
  ;;   (flyspell-mode-off))
  )

(use-package org
  :straight (:type built-in)
  :hook (org-mode . custom-org-mode-hook))
;; Hook:1 ends here

;; [[file:init-emacs.org::*Babel][Babel:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Babel
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Babel")
;; Babel:1 ends here

;; [[file:init-emacs.org::*Setup][Setup:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Setup
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Setup")

;; load ob-shell
(use-package org
  :straight (:type built-in)
  :config
  (require 'ob-shell nil :no-error))

;; load ob-async
(use-package ob-async
  :straight t
  :after (org))

;; babel settings
;; (do not set `org-src-fontify-natively' to true as this breaks org-table-align->font-lock-fontify-region)
(setq org-use-property-inheritance t
      org-babel-use-quick-and-dirty-noweb-expansion t
      org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      ;;org-src-fontify-natively t
      org-src-ask-before-returning-to-edit-buffer nil
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-window-setup 'current-window
      org-confirm-babel-evaluate nil
      org-confirm-shell-link-function nil
      org-confirm-elisp-link-function nil)

;; exportable file types
(set-default 'org-export-backends '(ascii html icalendar latex md odt org))

;; ;; delete trailing white space on tangle
;; (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
;; (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)

;; update images in buffer after evaluation
(add-hook 'org-babel-after-execute-hook #'org-display-inline-images :append)

;; org-eldoc is just too buggy with org-babel
;; Examples:
;;   ":tangle-mode (identity #o600)" throws
;;   "#+BEGIN_SRC org" goes into an infinite loop

;; (use-package org-eldoc
;;   :straight (:type built-in)
;;   :commands (global-eldoc-mode)
;;   :config (progn
;;             ;; turn off eldoc mode
;;             (global-eldoc-mode -1)))

;; ;; FIXME: temporary fix for org-eldoc-documentation-function
;; (use-package org-eldoc
;;   :straight (:type built-in)
;;   :commands (org-eldoc-get-breadcrumb
;;              org-eldoc-get-mode-local-documentation-function
;;              org-eldoc-get-src-header
;;              org-eldoc-get-src-lang)
;;   :functions (c-eldoc-print-current-symbol-info
;;               css-eldoc-function
;;               eldoc-print-current-symbol-info
;;               go-eldoc--documentation-function
;;               org-eldoc-documentation-function
;;               php-eldoc-function)
;;   :config
;;   ;; fix `org-eldoc-documentation-function' so it does not recursively call in org lang babel blocks
;;   (defun org-eldoc-documentation-function ()
;;     "Return breadcrumbs when on a headline, args for src block header-line,
;;   calls other documentation functions depending on lang when inside src body.
;;   [Custom fix for recursive bug]"
;;     (or
;;      (org-eldoc-get-breadcrumb)
;;      (org-eldoc-get-src-header)
;;      (let ((lang (org-eldoc-get-src-lang)))
;;        (cond ((or
;;                (string= lang "emacs-lisp")
;;                (string= lang "elisp")) (if (fboundp 'elisp-eldoc-documentation-function)
;;                (elisp-eldoc-documentation-function)
;;                (let (eldoc-documentation-function)
;;                  (eldoc-print-current-symbol-info))))
;;              ((or
;;                (string= lang "c") ;; http://github.com/nflath/c-eldoc
;;                (string= lang "C")) (when (require 'c-eldoc nil :no-error)
;;                (c-eldoc-print-current-symbol-info)))
;;              ;; https://github.com/zenozeng/css-eldoc
;;              ((string= lang "css") (when (require 'css-eldoc nil :no-error)
;;                                      (css-eldoc-function)))
;;              ;; https://github.com/zenozeng/php-eldoc
;;              ((string= lang "php") (when (require 'php-eldoc nil :no-error)
;;                                      (php-eldoc-function)))
;;              ((or
;;                (string= lang "go")
;;                (string= lang "golang")) (when (require 'go-eldoc nil :no-error)
;;                (go-eldoc--documentation-function)))
;;              (t (let ((doc-fun (org-eldoc-get-mode-local-documentation-function lang)))
;;                   (when (and (functionp doc-fun) (not (string= doc-fun "org-eldoc-documentation-function")))
;;                     (funcall doc-fun)))))))))
;; Setup:1 ends here

;; [[file:init-emacs.org::*Structure Templates][Structure Templates:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Structure Templates
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Structure Templates")

(use-package org-tempo
  :straight (:type built-in)
  :custom
  ;; set keyword completion elements
  (org-tempo-keywords-alist
   '(
     ("A" . "ascii")
     ;;("c" . "call")
     ("H" . "html")
     ("i" . "index")
     ("L" . "latex")
     ("n" . "name")))
  ;; set block types
  (org-structure-template-alist
   '(
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("ea" . "export ascii")
     ("eh" . "export html")
     ("el" . "export latex")
     ("o" . "src org")
     ("q" . "quote")
     ("s" . "src")
     ("sel" . "src emacs-lisp")
     ("sk" . "src kotlin")
     ("spy" . "src python")
     ("sql" . "src sql")
     ("sr" . "src racket")
     ("ssh" . "src sh")
     ("ssu" . "src sh :dir /sudo::")
     ("t" . "src text")
     ("v" . "verse")))
  :config
  ;; custom `org-tempo-add-block' with upcase headers
  (defun org-tempo-add-block (entry)
    "Add block entry from `org-structure-template-alist'."
    (let* ((key (format "<%s" (car entry)))
           (name (cdr entry))
           (name-parts (split-string name " "))
           (upcase-type (upcase (car name-parts)))
           (upcase-name (mapconcat 'identity (cons upcase-type (cdr name-parts)) " "))
           (special (member name '("src" "export"))))
      (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
                             `(,(format "#+BEGIN_%s%s" upcase-name (if special " " ""))
                               ,(when special 'p) '> n '> ,(unless special 'p) n
                               ,(format "#+END_%s" upcase-type)
                               >)
                             key
                             (format "Insert a %s block" upcase-name)
                             'org-tempo-tags)))

  ;; custom `org-tempo-add-keyword' with upcase headers
  (defun org-tempo-add-keyword (entry)
    "Add keyword entry from `org-tempo-keywords-alist'."
    (let* ((key (format "<%s" (car entry)))
           (name (cdr entry))
           (upcase-name (upcase name)))
      (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
                             `(,(format "#+%s: " upcase-name) p '>)
                             key
                             (format "Insert a %s keyword" upcase-name)
                             'org-tempo-tags)))

  ;; custom `org-tempo--include-file' with upcase header
  (defun org-tempo--include-file ()
    "Add #+include: and a file name."
    (let ((inhibit-quit t))
      (unless (with-local-quit
                (prog1 t
                  (insert
                   (format "#+INCLUDE: %S "
                           (file-relative-name
                            (read-file-name "Include file: "))))))
        (insert "<I")
        (setq quit-flag nil))))

  ;; update templates
  (org-tempo-add-templates))

;; ;; upcase begin_src and end_src block headers
;; (defun org-insert-structure-template--upcase (type)
;;   "Upcase #+begin_src and #+end_src block headers."
;;   (save-mark-and-excursion
;;     (let ((case-fold-search nil))     ; case sensitive search
;;       (forward-line -1)
;;       (when (re-search-forward "^[ \t]*#\\+begin_[a-z]*" (line-end-position) :noerror)
;;         (replace-match (upcase (match-string 0)))
;;         (forward-line 2)
;;         (when (re-search-forward "^[ \t]*#\\+end_[a-z]*" (line-end-position) :noerror)
;;           (replace-match (upcase (match-string 0))))))))
;; ;; advise `org-insert-structure-template'
;; (advice-add 'org-insert-structure-template :after #'org-insert-structure-template--upcase)

;; ;; upcase include header
;; (defun org-tempo--include-file--upcase ()
;;   "Upcase #+include: header."
;;   (save-mark-and-excursion
;;     (let ((case-fold-search nil))     ; case sensitive search
;;       (forward-line 0)
;;       (when (re-search-forward "^[ \t]*#\\+include: " (line-end-position) :noerror)
;;         (replace-match (upcase (match-string 0)))))))
;; ;; advise `org-tempo--include-file'
;; (advice-add 'org-tempo--include-file :after #'org-tempo--include-file--upcase))
;; Structure Templates:1 ends here

;; [[file:init-emacs.org::*Edit Source][Edit Source:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Edit Source
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Edit Source")

(use-package org
  :straight (:type built-in)
  :commands (org-edit-src-code
             org-edit-src-exit)
  :config
  (require 'org-src)

  ;; make sure custom key bindings are set in `org-src-mode'
  (add-hook 'org-src-mode-hook #'custom-key-bindings-set-all)

  (defun org-edit-src-exit--update-last-modified ()
    "Update LAST_MODIFIED property timestamp on source update."
    (org-update-last-modified-property))
  ;; advise `org-edit-src-exit'
  (advice-add 'org-edit-src-exit :before #'org-edit-src-exit--update-last-modified)

  (defun org-edit-src--recenter (&optional arg)
    "Recenter when entering/exiting special editors."
    (recenter))
  ;; advise `org-edit-special'
  (advice-add 'org-edit-special :after #'org-edit-src--recenter)
  ;; advise `org-edit-src-exit'
  (advice-add 'org-edit-src-exit :after #'org-edit-src--recenter))
;; Edit Source:1 ends here

;; [[file:init-emacs.org::*Tangle Case-Sensitive][Tangle Case-Sensitive:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Tangle Case-Sensitive
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Tangle Case-Sensitive")

(defun org-babel-tangle-collect-blocks--case-sensitive (orig-fun &rest args)
  "Set `case-fold-search' to nil so `string-match' calls are case-sensitive."
  (let ((case-fold-search nil))
    (apply orig-fun args)))
;; advise `org-babel-tangle-collect-blocks' making it case-sensitive
(advice-add 'org-babel-tangle-collect-blocks :around #'org-babel-tangle-collect-blocks--case-sensitive)
;; Tangle Case-Sensitive:1 ends here

;; [[file:init-emacs.org::*Tangle Update Timestamps][Tangle Update Timestamps:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Tangle Update Timestamps
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Tangle Update Timestamps")

(defun org-babel-post-tangle-hook--time-stamp ()
  "Update timestamps in tangled files."
  (time-stamp)
  (save-buffer))
(add-hook 'org-babel-post-tangle-hook #'org-babel-post-tangle-hook--time-stamp)
;; Tangle Update Timestamps:1 ends here

;; [[file:init-emacs.org::*Tangle Delete Trailing Whitespace][Tangle Delete Trailing Whitespace:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Tangle Delete Trailing Whitespace
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Tangle Delete Trailing Whitespace")

(defun org-babel-post-tangle-hook--delete-trailing-whitespace ()
  "Delete trailing whitespace in tangled files."
  (delete-trailing-whitespace (point-min) (point-max))
  (save-buffer))
(add-hook 'org-babel-post-tangle-hook #'org-babel-post-tangle-hook--delete-trailing-whitespace)
;; Tangle Delete Trailing Whitespace:1 ends here

;; [[file:init-emacs.org::*Racket][Racket:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Racket
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Racket")

(use-package ob-racket
  :straight (ob-racket
             :type git :host github :repo "hasu/emacs-ob-racket"
             :files ("*.el" "*.rkt"))
  :after (org)
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
            #'ob-racket-raco-make-runtime-library)

  ;; ;; default to racket-mode for racket files
  ;; (defvar org-babel-tangle-lang-exts)
  ;; (add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))

  (defvar org-babel-racket-command "racket")
  (defvar org-babel-default-header-args:racket '())
  (defvar org-babel-header-args:racket '((package . :any)))

  (defun org-babel-execute:racket (body params)
    "Execute a block of Racket Scheme code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block.

This function is called by `org-babel-execute-src-block'."
    (let ((result
           ;; if there is a #lang line then geiser racket session wont work
           (if (with-temp-buffer
                 (insert (org-babel-expand-body:lisp body params))
                 (goto-char (point-min))
                 (re-search-forward "^[ \t]*#lang +\\([^ ]+\\)" nil :noerror))
               ;; create temporary racket file and execute it for result
               (let ((src-file (org-babel-src-file "racket-" ".rkt")))
                 (with-temp-file src-file
                   (insert (org-babel-expand-body:lisp body params)))
                 (org-babel-eval (concat org-babel-racket-command " " src-file) ""))
             ;; otherwise, eval body in geiser racket session for result
             (funcall (if (member "output" (cdr (assq :result-params params)))
                          (lambda (x) (cdr (assq (intern "output") x)))
                        (lambda (x) (cdr (assq (intern "result") x))))
                      (with-temp-buffer
                        (insert (org-babel-expand-body:lisp body params))
                        (racket-mode)
                        ;; check for non-comment lines
                        (goto-char (point-min))
                        (while (and (not (eobp))
                                    (looking-at "^[ \t]*\\(;\\|$\\)"))
                          (forward-line 1))
                        ;; only continue if there are non-comment lines
                        ;; (otherwise geiser eval commands hang)
                        (unless (eobp)
                          (if (fboundp 'geiser-eval-region)
                              (geiser-eval-region (point-min) (point-max) nil :raw)
                            (error "geiser-eval-region not defined"))))))))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assq :result-params params))
         result
         (condition-case nil
             (read (org-babel-lisp-vector-to-list result))
           (error result)))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

;; (use-package ob-scheme
;;   :straight (:type built-in)
;;   :config (progn
;;             ;; customize org/babel for scheme/racket so it works
;;             (defcustom org-babel-scheme-command "racket -f"
;;               "Name of the scheme command.
;; May be either a command in the path, like scheme
;; or an absolute path name, like /usr/local/bin/scheme
;; parameters may be used, like scheme -verbose"
;;               :group 'org-babel
;;               :type 'string)

;;             (defcustom org-babel-scheme-compiler "racket"
;;               "Name of the scheme compiler.
;; May be either a command in the path, like schemec
;; or an absolute path name, like /usr/local/bin/schemec
;; parameters may be used, like schemec -verbose"
;;               :group 'org-babel
;;               :type 'string)

;;             ;; default to scheme-mode for racket files
;;             (add-to-list 'org-babel-tangle-lang-exts '("scheme" . "rkt"))

;;             ;; redefine `org-babel-scheme-make-session-name' to use
;;             ;; implementation name if no session name is given
;;             (defun org-babel-scheme-make-session-name (buffer name impl)
;;               "Generate a name for the session buffer.

;; For a named session, the buffer name will be the session name.

;; If the session is unnamed (nil), generate a name.

;; If the session is `none', use an existing IMPL session if one exists,
;; otherwise use nil for the session name, and
;; org-babel-scheme-execute-with-geiser will use a temporary session."
;;               (cond
;;                ((not name)
;;                 (concat buffer " " (symbol-name impl) " REPL"))
;;                ((string= name "none")
;;                 (and (org-babel-scheme-get-session-buffer impl) impl))
;;                (t
;;                 name)))

;;             ;; redefine `org-babel-scheme-get-session-buffer' to add default
;;             ;; racket geiser repl to `org-babel-scheme-repl-map' if running
;;             (defun org-babel-scheme-get-session-buffer (session-name)
;;               "Look up the scheme buffer for a session; return nil if it doesn't exist."
;;               (org-babel-scheme-cleanse-repl-map) ; prune dead sessions
;;               (let* ((impl (intern "racket"))
;;                      (buffer (get-buffer "* Racket REPL *")))
;;                 (when (and buffer
;;                            (not (gethash impl org-babel-scheme-repl-map)))
;;                   (puthash impl buffer org-babel-scheme-repl-map)))
;;               (gethash session-name org-babel-scheme-repl-map))))

;;             ;; (defun org-babel-scheme-get-session-buffer--add-geiser-repl (session-name)
;;             ;;   "Add default racket geiser repl to `org-babel-scheme-repl-map' if running."
;;             ;;   (let* ((impl (intern "racket"))
;;             ;;          (buffer (get-buffer "* Racket REPL *")))
;;             ;;     (when (and buffer
;;             ;;                (not (gethash impl org-babel-scheme-repl-map)))
;;             ;;       (puthash impl buffer org-babel-scheme-repl-map))))
;;             ;; ;; advise `org-babel-scheme-get-session-buffer'
;;             ;; (advice-add 'org-babel-scheme-get-session-buffer :before #'org-babel-scheme-get-session-buffer--add-geiser-repl)))
;; Racket:1 ends here

;; [[file:init-emacs.org::*Java][Java:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Java
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Java")

(use-package org
  :straight (:type built-in)
  :commands (org-babel-execute:java)
  :config
  (when (require 'ob-java nil :no-error)
    ;; customize org/babel for java so it will handle packages correctly
    (defun org-babel-execute:java (body params)
      "Execute a block of Java code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block."
      (let* ((classname (or (cdr (assoc :classname params))
                            (error ":classname parameter is required")))
             (packagename (file-name-directory classname))
             (src-file (concat classname ".java"))
             (javacflags (or (cdr (assoc :javacflags params)) ""))
             (javaflags (or (cdr (assoc :javaflags params)) ""))
             (full-body (org-babel-expand-body:generic body params)))
        ;; created package-name directories if missing
        (unless (or (not packagename) (file-exists-p packagename))
          (make-directory packagename 'parents))
        ;; compile java source file
        (with-temp-file src-file
          (insert full-body)
          (org-babel-eval (concat org-babel-java-compiler " " javacflags " " src-file) ""))
        ;; run java code
        (let ((result (org-babel-eval (concat org-babel-java-command " " javaflags " " classname) "")))
          (org-babel-reassemble-table
           (org-babel-result-cond (cdr (assoc :result-params params))
             (org-babel-read result)
             (let ((temp-file (org-babel-temp-file "java-")))
               (with-temp-file temp-file
                 (insert result)
                 (org-babel-import-elisp-from-file temp-file))))
           (org-babel-pick-name (cdr (assoc :colname-names params))
                                (cdr (assoc :colnames params)))
           (org-babel-pick-name (cdr (assoc :rowname-names params))
                                (cdr (assoc :rownames params)))))))))
;; Java:1 ends here

;; [[file:init-emacs.org::*Kotlin][Kotlin:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Kotlin
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Kotlin")

(use-package ob-kotlin
  :straight t
  :after (org kotlin-mode)
  :commands (org-babel-execute:kotlin)
  :functions (flycheck-mode
              kotlin-send-buffer
              org-babel-kotlin-command)
  :defines (org-babel-kotlin-compiler)
  :config
  ;; customize org/babel for kotlin so it works
  (defcustom org-babel-kotlin-command "kotlin"
    "Name of the kotlin command.
May be either a command in the path, like kotlin or an absolute
path name, like /usr/local/bin/kotlin parameters may be used,
like kotlin -verbose"
    :group 'org-babel
    :type 'string)

  (defcustom org-babel-kotlin-compiler "kotlinc"
    "Name of the kotlin compiler.
May be either a command in the path, like kotlinc or an absolute
path name, like /usr/local/bin/kotlinc parameters may be used,
like kotlinc -verbose"
    :group 'org-babel
    :type 'string)

  (defun org-babel-execute:kotlin (body params)
    "If main function exists, then compile code and run jar
otherwise, run code in `kotlin-repl'."
    (let* ((classname (or (cdr (assq :classname params)) "main"))
           ;;(packagename (file-name-directory classname))
           (src-file (org-babel-temp-file classname ".kt"))
           (jar-file (concat (file-name-sans-extension src-file) ".jar"))
           (cmpflag (or (cdr (assq :cmpflag params)) ""))
           (cmdline (or (cdr (assq :cmdline params)) ""))
           (full-body (org-babel-expand-body:generic body params)))
      (if (or (string-match "fun main(args: Array<String>)" full-body)
              (string-match "fun main()" full-body))
          (progn
            (with-temp-file src-file (insert full-body))
            (org-babel-eval
             (concat org-babel-kotlin-compiler " " cmpflag " " src-file " -include-runtime -d " jar-file) "")
            (message (org-babel-eval (concat org-babel-java-command " " cmdline " -jar " jar-file) "")))
        (with-temp-buffer
          (insert body)
          (kotlin-send-buffer))))))
;; Kotlin:1 ends here

;; [[file:init-emacs.org::*Python][Python:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Python
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Python")

(use-package org
  :straight (:type built-in)
  :after (python-mode)
  :commands (org-babel-execute:python)
  :config
  (when (require 'ob-python nil :no-error)
    (defun org-babel-execute:python (body params)
      "Execute a block of Python code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block.

This function is called by `org-babel-execute-src-block'.

Note: This function only works with an iPython shell if it has
the autoindent feature turned off. Add '--no-autoindent' to
`py-ipython-command-args' to set this when an iPython process is
created."
      (let* ((org-babel-python-command (or (cdr (assq :python params))
                                           org-babel-python-command))
             (session-name (cdr (assq :session params)))
             (session (when (and session-name (not (string= session-name "none")))
                        (org-babel-python-initiate-session session)))
             (result-params (cdr (assq :result-params params)))
             (result-type (cdr (assq :result-type params)))
             (return-val (when (and (eq result-type 'value) (not session))
                           (cdr (assq :return params))))
             (preamble (cdr (assq :preamble params)))
             (process (get-process python-shell-buffer-name))
             (full-body
              (org-babel-expand-body:generic
               (concat body (if return-val (format "\nreturn %s" return-val) ""))
               params (org-babel-variable-assignments:python params)))
             (result (cond
                      (session
                       (org-babel-python-evaluate-session
                        session full-body result-type result-params))
                      (process
                       (process-send-string process (concat full-body "\n\n"))
                       (display-buffer (process-buffer process) t))
                      (t
                       (org-babel-python-evaluate-external-process
                        full-body result-type result-params preamble)))))
        (org-babel-reassemble-table
         result
         (org-babel-pick-name (cdr (assq :colname-names params))
                              (cdr (assq :colnames params)))
         (org-babel-pick-name (cdr (assq :rowname-names params))
                              (cdr (assq :rownames params))))))))
;; Python:1 ends here

;; [[file:init-emacs.org::*Rust][Rust:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Rust
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Rust")

(use-package ob-rust
  :straight t
  :after (org rust-mode)
  :commands (org-babel-execute:rust)
  :functions (flycheck-mode
              rust-send-buffer
              org-babel-rust-command)
  :config
  (when (require 'ob-rust nil :no-error)
    (defun org-babel-execute:rust (body params)
      "Execute a block of Rust code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block.

This function is called by `org-babel-execute-src-block'."
      (let* ((tmp-src-file (org-babel-temp-file "rust-src-" ".rs"))
             (tmp-run-file (org-babel-temp-file "rust-run-"))
             (processed-params (org-babel-process-params params))
             (_flags (cdr (assoc :flags processed-params)))
             (_args (cdr (assoc :args processed-params)))
             (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
             (coding-system-for-write 'utf-8)
             (wrapped-body
              (save-match-data
                (if (string-match "fn main()" body)
                    body
                  (if (string-match "fn \\(.*_test\\)()" body)
                      (concat body "\n\nfn main() {\n" (match-string 1 body) "();\n}")
                    (concat "fn main() {\n" body "\n}"))))))
        (with-temp-file tmp-src-file (insert wrapped-body))
        (let ((result
               (org-babel-eval
                (format "rustc -o %s %s && %s" tmp-run-file tmp-src-file tmp-run-file)
                "")))
          (when result
            (org-babel-reassemble-table
             (if (or (member "table" (cdr (assoc :result-params processed-params)))
                     (member "vector" (cdr (assoc :result-params processed-params))))
                 (let ((tmp-file (org-babel-temp-file "rust-")))
                   (with-temp-file tmp-file (insert (org-babel-trim result)))
                   (org-babel-import-elisp-from-file tmp-file))
               (org-babel-read (org-babel-trim result) t))
             (org-babel-pick-name
              (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
             (org-babel-pick-name
              (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))))))
;; Rust:1 ends here

;; [[file:init-emacs.org::*V][V:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: V
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: V")

;; default to v-mode for v files
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("v" . "v"))

(defvar org-babel-v-command "v")
(defvar org-babel-default-header-args:v '())
(defvar org-babel-header-args:v '((package . :any)))

(defun org-babel-execute:v (body params)
  "Execute a block of V code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block.

This function is called by `org-babel-execute-src-block'."
  (let* ((tmp-src-file (org-babel-temp-file "v-src-" ".v"))
         (tmp-run-file (org-babel-temp-file "v-run-"))
         (processed-params (org-babel-process-params params))
         (_flags (cdr (assoc :flags processed-params)))
         (_args (cdr (assoc :args processed-params)))
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8)
         (wrapped-body
          (save-match-data
            (if (string-match "fn main()" body)
                body
              (if (string-match "fn \\(test_.*\\)()" body)
                  (let ((start 0)
                        tests)
                    (while (string-match "fn \\(test_.*\\)()" body start)
                      (push (match-string 1 body) tests)
                      (setq start (match-end 0)))
                    (concat body
                            "\n\nfn main() {\n"
                            (apply #'concat (nreverse tests))
                            "()\n}"))
                body)))))
    (with-temp-file tmp-src-file (insert wrapped-body))
    (let ((result
           (org-babel-eval
            (format "v -o %s %s && %s" tmp-run-file tmp-src-file tmp-run-file)
            "")))
      (when result
        (org-babel-reassemble-table
         (if (or (member "table" (cdr (assoc :result-params processed-params)))
                 (member "vector" (cdr (assoc :result-params processed-params))))
             (let ((tmp-file (org-babel-temp-file "v-")))
               (with-temp-file tmp-file (insert (org-babel-trim result)))
               (org-babel-import-elisp-from-file tmp-file))
           (org-babel-read (org-babel-trim result) t))
         (org-babel-pick-name
          (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
         (org-babel-pick-name
          (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))))
;; V:1 ends here

;; [[file:init-emacs.org::*Basic (Commander X16)][Basic (Commander X16):1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Basic (Commander X16)
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Basic (Commander X16)")

;; default to basic-mode for basic files
(require 'basic)
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("basic" . "bas"))

;;(defvar org-babel-basic-command "~/x16emu/x16emu")
(defvar org-babel-basic-command "x16emu -rom /usr/share/x16-rom/rom.bin")

(defun org-babel-execute:basic (body params)
  "Execute a block of BASIC code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block."
  (let ((src-file (org-babel-temp-file "basic-" ".bas")))
    (with-temp-file src-file
      (insert (org-babel-expand-body:generic body params)))
    (org-babel-eval (concat org-babel-basic-command " -bas " src-file " -run &") "")))
;; Basic (Commander X16):1 ends here

;; [[file:init-emacs.org::*Assembly Language (Commander X16)][Assembly Language (Commander X16):1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Assembly Language (Commander X16)
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Assembly Language (Commander X16)")

;; default to asm-mode for assembly language files
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("asm" . "asm"))

;; ;; vasm version
;; defvar org-babel-asm-compiler "vasm6502_oldstyle")
;; defvar org-babel-asm-compiler-params "-c02 -cbm-prg -chklabels -nocase -Dvasm=1 -DBuildC64=1 -Fbin")
;; defvar org-babel-asm-command "x16emu -keymap en-us")

;; (defun org-babel-execute:asm (body params)
;;   "Execute a block of Assembly Language code with Babel.
;; BODY is the contents of the block, as a string. PARAMS is
;; a property list containing the parameters of the block."
;;   (let* ((base-file (org-babel-temp-file "asm-"))
;;          (src-file (concat base-file ".asm"))
;;          (list-file (concat base-file ".lst"))
;;          (output-file (concat base-file ".prg"))
;;          (defines (cdr (assoc :defines params))))
;;     (with-temp-file src-file
;;       (insert (org-babel-expand-body:generic body params)))
;;     (org-babel-eval (concat org-babel-asm-compiler
;;                             " " org-babel-asm-compiler-params
;;                             (if defines (concat " -D " defines) "")
;;                             " -L " list-file
;;                             " -o " output-file
;;                             " " src-file) "")
;;     (org-babel-eval (concat org-babel-asm-command " -prg " output-file " -run &") "")))

;; acme version
(defun org-babel-execute:asm (body params)
  "Execute a block of Assembly Language code with Babel.
BODY is the contents of the block, as a string. PARAMS is a
property list containing the parameters of the block."
  (let* ((base-file (org-babel-temp-file "asm-"))
         (src-file (concat base-file ".asm"))
         (list-file (concat base-file ".lst"))
         (output-file (concat base-file ".prg"))
         (defines (cdr (assoc :defines params)))
         (cmpflag (or (cdr (assq :cmpflag params)) ""))
         (cmdline (or (cdr (assq :cmdline params)) ""))
         (x16emu (string= (cdr (assq :x16emu params)) "yes"))
         (compile-command (concat "acme"
                                  (if cmpflag (concat " " cmpflag) "")
                                  (if defines (concat " -D " defines) "")
                                  " --symbollist " list-file
                                  " --outfile " output-file
                                  " " src-file))
         (run-command (when x16emu
                        (concat "x16emu -keymap en-us"
                                (if cmdline (concat " " cmdline) "")
                                " -prg " output-file " -run &"))))
    (with-temp-file src-file
      (insert (org-babel-expand-body:generic body params)))
    (message (format "Compiling: %s" compile-command))
    (org-babel-eval compile-command "")
    (when x16emu
      (message (format "Running: %s" run-command))
      (org-babel-eval run-command ""))))

;; ;; 64tass version
;; defvar org-babel-asm-compiler "64tass")
;; defvar org-babel-asm-compiler-params "-a -B -C")
;; defvar org-babel-asm-command "x16emu -keymap en-us")

;; (defun org-babel-execute:asm (body params)
;;   "Execute a block of Assembly Language code with Babel.
;; BODY is the contents of the block, as a string. PARAMS is
;; a property list containing the parameters of the block."
;;   (let* ((base-file (org-babel-temp-file "asm-"))
;;          (src-file (concat base-file ".asm"))
;;          (list-file (concat base-file ".lst"))
;;          (output-file (concat base-file ".prg"))
;;          (defines (cdr (assoc :defines params))))
;;     (with-temp-file src-file
;;       (insert (org-babel-expand-body:generic body params)))
;;     (org-babel-eval (concat org-babel-asm-compiler
;;                             " " org-babel-asm-compiler-params
;;                             (if defines (concat " -D " defines) "")
;;                             " -L " list-file
;;                             " -o " output-file
;;                             " " src-file) "")
;;     (org-babel-eval (concat org-babel-asm-command " -prg " output-file " -run &") "")))
;; Assembly Language (Commander X16):1 ends here

;; [[file:init-emacs.org::*PlantUML][PlantUML:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: PlantUML
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: PlantUML")

;; set plantuml.jar location
(setq org-plantuml-jar-path "~/dev/java/lib/plantuml.jar")
;; PlantUML:1 ends here

;; [[file:init-emacs.org::*Load Languages][Load Languages:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Load Languages
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Load Languages")

;; babel languages (usable in source blocks)
(org-babel-do-load-languages
 'org-babel-load-languages '((clojure . t)
                             (css . t)
                             (ditaa . t)
                             (dot . t)
                             (emacs-lisp . t)
                             (gnuplot . t)
                             ;;(ipython . t)
                             (java . t)
                             (js . t)
                             ;;(json . t)
                             (kotlin . t)
                             (lisp . t)
                             (lua . t)
                             (makefile . t)
                             (org . t)
                             (perl . t)
                             (plantuml . t)
                             (python . t)
                             (racket . t)
                             (ruby . t)
                             (rust . t)
                             (scheme . t)
                             (shell . t)
                             (sql . t)))

;; major modes for languages
;; (only needed if the mode name is not LANG-mode)
;;(add-to-list 'org-src-lang-modes '("java" . jdee))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(add-to-list 'org-src-lang-modes '("racket" . scheme))
;; Load Languages:1 ends here

;; [[file:init-emacs.org::*Babel Functions][Babel Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Babel Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Babel Functions")
;; Babel Functions:1 ends here

;; [[file:init-emacs.org::*org-generate-custom-id-from-title][org-generate-custom-id-from-title:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-generate-custom-id-from-title
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-generate-custom-id-from-title")

(defun org-generate-custom-id-from-title (title)
  "Generate a proper `org-mode' CUSTOM_ID property from a given TITLE."
  (replace-regexp-in-string
   (rx (seq "-" "-")) "-"
   (replace-regexp-in-string
    (rx (seq "-" eol)) ""
    (replace-regexp-in-string
     (rx (seq bol "-")) ""
     (replace-regexp-in-string
      (rx (any ".")) "-dot-"
      (replace-regexp-in-string
       (rx (any " " "_" "/")) "-"
       (replace-regexp-in-string
        (rx (not (any alnum space "-" "_" "." "/"))) ""
        (downcase title))))))))
;; org-generate-custom-id-from-title:1 ends here

;; [[file:init-emacs.org::*org-fix-literate-programming-heading][org-fix-literate-programming-heading:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-fix-literate-programming-heading
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-fix-literate-programming-heading")

(defun org-fix-literate-programming-heading ()
  "Fix 'literate programming' heading of current org section.

Reset the CUSTOM_ID property, title comment, and `init-message'."
  (interactive "*")
  (save-window-excursion
    (save-mark-and-excursion
      (save-match-data
        (org-with-wide-buffer
         ;; find heading of current position
         (goto-char (line-beginning-position))
         (while (not (looking-at "^\*+ "))
           (forward-line -1))
         (let* ((case-fold-search t)
                (start (point))
                (count (length (progn
                                 (re-search-forward "^\\(\*+\\) " (line-end-position))
                                 (match-string-no-properties 1))))
                (level (/ (1+ count) (if org-odd-levels-only 2 1)))
                headings)
           ;; get parent headings
           (goto-char start)
           (while (and (not (bobp))
                       (> count 0))
             (let ((regexp (concat "^\*\\{" (number-to-string count) "\\} \\(.*\\)$")))
               (while (and (not (bobp))
                           (not (looking-at regexp)))
                 (forward-line -1))
               (when (re-search-forward regexp (line-end-position))
                 (push (replace-regexp-in-string "+" "" (match-string-no-properties 1)) headings)
                 (setq count (- count (if org-odd-levels-only 2 1))))))
           (let ((id (mapconcat (lambda (x) (org-generate-custom-id-from-title x))
                                headings "-"))
                 (title (mapconcat #'(lambda (x) x) headings ": ")))
             (goto-char start)
             ;; set CUSTOM_ID
             (org-set-property "CUSTOM_ID" id)
             (forward-line 1)
             (indent-region (point) (re-search-forward "^[ \t]*:END:$"))
             ;; set comment
             (let ((end (if (re-search-forward "^\*+ " nil :noerror) (point) (point-max))))
               (goto-char start)
               (when (re-search-forward "^[ \t]*#\\+BEGIN_SRC" end :noerror)
                 (org-babel-do-in-edit-buffer
                  (let* ((comment-1 (substring comment-start -1))
                         (comment-2 (if (= (length comment-start) 1)
                                        (concat comment-start comment-start)
                                      comment-start))
                         (comment-3 (concat comment-2 comment-1))
                         (comment-4 (concat comment-3 comment-1)))
                    (when (looking-at (concat
                                       comment-2 "[=-]+\n"
                                       comment-2 "+ .*\n"
                                       comment-2 "[=-]+$"))
                      (replace-match
                       (concat
                        comment-2 (make-string 78 (if (> level 1) ?- ?=)) "\n"
                        (if (> level 2) comment-4 comment-3) " " title "\n"
                        comment-2 (make-string 78 (if (> level 1) ?- ?=))))))
                  (goto-char (line-beginning-position))
                  ;; set `init-message' text
                  (when (re-search-forward "(init-message .*$" nil :noerror)
                    (replace-match (concat "(init-message " (number-to-string level) " \"" title "\")")))))))))))))
;; org-fix-literate-programming-heading:1 ends here

;; [[file:init-emacs.org::*org-fix-literate-programming-heading-region][org-fix-literate-programming-heading-region:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-fix-literate-programming-heading-region
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-fix-literate-programming-heading-region")

(defun org-fix-literate-programming-heading-region (&optional beg end)
  "Fix 'literate programming' headings contained with given region.

Reset the CUSTOM_ID property, title comment, and `init-message'."
  (interactive "*")
  (let ((case-fold-search t)
        (beg (or beg (if (use-region-p) (region-beginning) (progn (beginning-of-line-text) (point)))))
        (end (or end (if (use-region-p) (region-end) (line-end-position)))))
    (save-window-excursion
      (save-mark-and-excursion
        (save-match-data
          (org-with-wide-buffer
           (goto-char beg)
           (while (and (re-search-forward "^[ \t]*:CUSTOM_ID:" nil :noerror)
                       (< (line-end-position) end))
             (org-fix-literate-programming-heading)
             (forward-line 1))))))))
;; org-fix-literate-programming-heading-region:1 ends here

;; [[file:init-emacs.org::*org-toggle-literate-programming-code-block][org-toggle-literate-programming-code-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-toggle-literate-programming-code-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-toggle-literate-programming-code-block")

(defun org-toggle-literate-programming-code-block ()
  "Toggle 'literate programming' heading and code block on/off."
  (interactive)
  (save-window-excursion
    (save-mark-and-excursion
      (save-match-data
        (org-with-wide-buffer
         (org-back-to-heading t)
         (let ((case-fold-search t)
               (beg (point))
               (end (progn
                      (forward-line 1)
                      (re-search-forward "^\*" nil 'move)
                      (1- (point)))))
           (goto-char beg)
           (re-search-forward " " (line-end-position))
           (let ((toggle-on (looking-at "\+"))) ; whether toggling on or off
             ;; toggle source blocks (in reverse as END will move)
             (goto-char end)
             (while (re-search-backward "^[ \t]*#\\+BEGIN_" beg :noerror)
               (if toggle-on
                   (when (re-search-forward " :tangle no" (line-end-position) :noerror)
                     (replace-match ""))
                 (progn
                   (goto-char (line-end-position))
                   (insert " :tangle no")))
               (forward-line 1)
               (org-babel-do-in-edit-buffer
                (if toggle-on
                    (uncomment-region (point-min) (point-max))
                  (comment-region (point-min) (point-max))))
               (forward-line -2))
             ;; toggle header text
             (goto-char beg)
             (re-search-forward " " (line-end-position))
             (if toggle-on
                 (dotimes (x 2)
                   (when (re-search-forward "\+" (line-end-position))
                     (replace-match "")))
               (progn
                 (insert "+")
                 (goto-char (line-end-position))
                 (insert "+"))))))))))
;; org-toggle-literate-programming-code-block:1 ends here

;; [[file:init-emacs.org::*org-insert-literate-programming-statics][org-insert-literate-programming-statics:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-insert-literate-programming-statics
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-insert-literate-programming-statics")

(defun org-insert-literate-programming-name ()
  "Insert `org-babel' block NAME"
  (interactive "*")
  (org-indent-line)
  (insert "#+NAME: "))

(defmacro org-insert-literate-programming-src-gen (name &optional lang)
  "Generate org babel insert function from NAME and optional LANG.

LANG is only needed if the language section of the block is
different from NAME."
  (let* ((lang (or lang name))
         (funct (intern (concat "org-insert-literate-programming-src"
                                (if name (concat "-" name) ""))))
         (doc (concat "Insert `org-babel'"
                      (if lang (concat " " name) "")
                      " source block"))
         (block (concat "#+BEGIN_SRC"
                        (if lang (concat " " lang) "")
                        "\n\n#+END_SRC\n")))
    `(defun ,funct ()
       ,doc
       (interactive "*")
       (let ((point (point)))
         (insert ,block)
         (indent-region point (point))
         (forward-line -2)))))

;; org-insert-literate-programming-src
(org-insert-literate-programming-src-gen nil)

;; org-insert-literate-programming-src-sh
(org-insert-literate-programming-src-gen "sh")

;; org-insert-literate-programming-src-sh-sudo
(org-insert-literate-programming-src-gen "sh-sudo" "sh :dir /sudo::")

;; org-insert-literate-programming-src-emacs-lisp
(org-insert-literate-programming-src-gen "emacs-lisp")

;; org-insert-literate-programming-src-racket
(org-insert-literate-programming-src-gen "racket")

;; org-insert-literate-programming-src-kotlin
(org-insert-literate-programming-src-gen "kotlin")
;; org-insert-literate-programming-statics:1 ends here

;; [[file:init-emacs.org::*org-insert-literate-programming-init-emacs-block][org-insert-literate-programming-init-emacs-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-insert-literate-programming-init-emacs-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-insert-literate-programming-init-emacs-block")

(defun org-insert-literate-programming-init-emacs-block (&optional title)
  "Insert 'literate programming' init-emacs block consisting of a heading,
properties, source block, title comment, and `init-message'."
  (interactive "*")
  (let* ((case-fold-search t)
         (point (point))
         (title (or title (read-string "Block title: ")))
         (tag (org-generate-custom-id-from-title title)))
    (org-insert-heading)
    (insert title)
    (org-set-property "CUSTOM_ID" tag)
    (re-search-forward "^[ \t]*:END:$")
    (newline)
    (newline)
    (insert "#+BEGIN_SRC emacs-lisp\n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (insert "  ;;;; " title "\n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (newline)
    (insert "  (init-message 2 \"" title "\")\n")
    (newline)
    (newline)
    (insert "#+END_SRC\n")
    (indent-region point (point))
    (org-previous-visible-heading 1)
    (goto-char (line-end-position))
    (org-fix-literate-programming-heading)
    (forward-line 12)))
;; org-insert-literate-programming-init-emacs-block:1 ends here

;; [[file:init-emacs.org::*org-insert-literate-programming-code-block][org-insert-literate-programming-code-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-insert-literate-programming-code-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-insert-literate-programming-code-block")

(defun org-insert-literate-programming-code-block ()
  "Insert 'literate programming' code block consisting of a heading,
properties, source block, and title comment."
  (interactive "*")
  (let* ((case-fold-search t)
         (point (point))
         (title (read-string "Block title: "))
         (funct (org-generate-custom-id-from-title title))
         (lang (save-mark-and-excursion
                 (if (re-search-backward "^[ \t]*#\\+BEGIN_SRC \\([^ \t\n]*\\)" nil :noerror)
                     (match-string 1)
                   "emacs-lisp"))))
    (org-previous-visible-heading 1)
    (org-insert-heading-respect-content)
    (insert title)
    (org-set-property "CUSTOM_ID" funct)
    (re-search-forward "^[ \t]*:END:$")
    (newline)
    (newline)
    (insert "#+BEGIN_SRC " lang "\n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (insert "  ;;;; " title "\n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (newline)
    (insert "  (define (" funct " num)\n")
    (insert "    )\n")
    (insert "#+END_SRC\n")
    (newline)
    (insert "#+BEGIN_SRC " lang " :tangle no\n")
    (insert "  (define (" funct "-test)\n")
    (insert "    (check-equal? (" funct " ) )\n")
    (insert "    )\n")
    (insert "#+END_SRC\n")
    (indent-region point (point))
    (org-previous-visible-heading 1)
    (goto-char (line-end-position))
    (org-fix-literate-programming-heading)
    (forward-line 10)))
;; org-insert-literate-programming-code-block:1 ends here

;; [[file:init-emacs.org::*org-insert-literate-programming-project-euler-problem-block][org-insert-literate-programming-project-euler-problem-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-insert-literate-programming-project-euler-problem-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-insert-literate-programming-project-euler-problem-block")

(defun org-insert-literate-programming-project-euler-problem-block ()
  "Insert 'literate programming' project euler problem block consisting of a
heading, properties, source block with title comment, and test block."
  (interactive "*")
  (let* ((case-fold-search t)
         (point (point))
         (num (read-string "Problem number: "))
         (title (concat "Problem " num))
         (funct (format "project-euler-%03d" (string-to-number num)))
         (lang (save-mark-and-excursion
                 (if (re-search-backward "^[ \t]*#\\+BEGIN_SRC \\([^ \t\n]*\\)" nil :noerror)
                     (match-string 1)
                   "emacs-lisp"))))
    (org-previous-visible-heading 1)
    (org-insert-heading-respect-content)
    (insert title)
    (org-set-property "CUSTOM_ID" funct)
    (re-search-forward "^[ \t]*:END:$")
    (newline)
    (newline)
    (insert "#+BEGIN_SRC " lang "\n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (insert "  ;;;; " title "\n")
    (insert "  ;;\n")
    (insert "  ;;\n")
    (insert "  ;;\n")
    (insert "  ;; The correct answer is: \n")
    (insert "  ;;------------------------------------------------------------------------------\n")
    (newline)
    (insert "  (define (" funct " [num 1000])\n")
    (insert "    )\n")
    (insert "#+END_SRC\n")
    (newline)
    (insert "#+BEGIN_SRC " lang " :tangle no\n")
    (insert "  (define (" funct "-test)\n")
    (insert "    (check-equal? (" funct " ) )\n")
    (insert "    )\n")
    (insert "#+END_SRC\n")
    (indent-region point (point))
    (org-previous-visible-heading 1)
    (goto-char (line-end-position))
    (org-fix-literate-programming-heading)
    (forward-line 9)))
;; org-insert-literate-programming-project-euler-problem-block:1 ends here

;; [[file:init-emacs.org::*Visibility][Visibility:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Visibility
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Visibility")

(use-package org-visibility
  :straight t
  ;;:load-path (lambda () (file-truename (expand-file-name "~/code/github-nullman/emacs-org-visibility")))
  :after (org)
  ;;:demand t
  :bind (:map org-visibility-mode-map
              ("C-x C-v" . org-visibility-force-save) ; default: `find-alternative-file'
              ("C-x M-v" . org-visibility-remove))    ; default: undefined
  :hook (org-mode . org-visibility-mode)
  :custom
  ;; list of directories and files to automatically persist and restore visibility state of
  (org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
                                  ,(file-truename "~/code/github-nullman")
                                  ,(file-truename "~/dev")
                                  ,(file-truename "~/doc/bbs")
                                  ,(file-truename "~/org")
                                  ,(file-truename "~/web/org")))
  ;; list of directories and files to not persist and restore visibility state of
  (org-visibility-exclude-paths `(,(file-truename "~/org/old")
                                  ,(file-truename "~/org/test"))))
;; Visibility:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-guid][org-bookmarks-guid:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-guid
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-guid")

(defun org-bookmarks-guid ()
  "Return a twelve character GUID."
  (cl-labels
      ((random-char ()
                    (let ((num (random 62)))
                      (cond
                       ((< num 10)
                        (byte-to-string (+ num 48)))
                       ((< num 36)
                        (byte-to-string (+ num 55)))
                       (t
                        (byte-to-string (+ num 61)))))))
    (cl-loop repeat 12
             concat (random-char))))
;; org-bookmarks-guid:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-timestamp][org-bookmarks-timestamp:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-timestamp
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-timestamp")

(defun org-bookmarks-timestamp ()
  "Return time since the epoch in microseconds."
  (floor (* (float-time (current-time)) 1000000)))
;; org-bookmarks-timestamp:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-parse][org-bookmarks-parse:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-parse
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-parse")

(defun org-bookmarks-parse (file)
  "Return a tree structure representing the org folders and
bookmarks found in FILE.

Example input:

  * Org
  * Folder 1
  ** Folder 2
  *** Bookmark 1
        URI1
  *** Bookmark 2 [bm2]
        URI2
  ** Folder 3

Example output:

  ((:type \"folder\" :title \"Folder 1\" :children
    [(:type \"folder\" :title \"Folder 2\" :children
     [(:type \"bookmark\" :title \"Bookmark 1\" :uri \"URI1\")
      (:type \"bookmark\" :title \"Bookmark 2\" :uri \"URI2\" :keyword \"bm2\")])
     (:type \"folder\" :title \"Folder 3\")]))

If the first headline is \"Org\", it is ignored."
  (cl-labels
      ((parse (bm tree)
              (cond
               ;; end of branch
               ((not bm)
                nil)
               ;; bookmark
               ((and (stringp (car bm)) (stringp (cadr bm)))
                (let* ((title (car x))
                       (uri (cadr x))
                       (keyword (if (string-match " \\[\\(.*\\)\\]$" title)
                                    (match-string-no-properties 1 title)
                                  nil))
                       (title (replace-regexp-in-string " \\[.*\\]$" "" title))
                       (entry (list :type "bookmark" :title title :uri uri)))
                  (when keyword
                    (setq entry (append entry (list :keyword keyword))))
                  entry))
               ;; nested folder
               ((stringp (car bm))
                (let* ((title (car bm))
                       (entry (list :type "folder" :title title)))
                  (if (cdr bm)
                      (append entry (list :children (map 'vector (lambda (x)
                                                                   (parse x tree))
                                                         (cdr bm))))
                    entry))))))
    (let ((bm (org-get-file-data file)))
      (map 'vector (lambda (x)
                     (parse x nil))
           (cdr bm)))))
;; org-bookmarks-parse:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-export-to-json][org-bookmarks-export-to-json:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-export-to-json
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-export-to-json")

(defun org-bookmarks-export-to-json (org-file &optional json-file)
  "Export from an Org Bookmarks file, ORG-FILE,
to a Mozilla/Firefox Bookmarks JSON file, JSON-FILE.

If JSON-FILE is non-nil, then output is returned."
  (let* ((type-code-list '(("bookmark" . 1)
                           ("folder" . 2)))
         (type-value-list '(("bookmark" . "text/x-moz-place")
                            ("folder" . "text/x-moz-place-container")))
         (title-guid-list '(("Bookmarks Menu" . "menu________")
                            ("Bookmarks Toolbar" . "toolbar_____")
                            ("Other Bookmarks" . "unfiled_____")
                            ("Mobile Bookmarks" . "mobile______")))
         (title-root-list '(("Bookmarks Menu" . "bookmarksMenuFolder")
                            ("Bookmarks Toolbar" . "toolbarFolder")
                            ("Other Bookmarks" . "unfiledBookmarksFolder")
                            ("Mobile Bookmarks" . "mobileFolder")))
         (global-id 0)
         (timestamp (org-bookmarks-timestamp)))
    (cl-labels
        ((gen-id ()
                 (incf global-id))
         (parse (bm tree index)
                (let* ((type (plist-get bm :type))
                       (type-value (cdr (assoc type type-value-list)))
                       (type-code (cdr (assoc type type-code-list)))
                       (title (plist-get bm :title))
                       (uri (plist-get bm :uri))
                       (keyword (plist-get bm :keyword))
                       (children (plist-get bm :children))
                       (guid (org-bookmarks-guid))
                       (id (gen-id))
                       (root (cdr (assoc title title-root-list)))
                       (custom-guid (cdr (assoc title title-guid-list)))
                       (entry (list
                               :guid (or custom-guid guid)
                               :title title
                               :index index
                               :dateAdded timestamp
                               :lastModified timestamp
                               :id id
                               :typeCode type-code
                               :type type-value)))
                  (when uri
                    (setq entry (append entry (list :uri uri))))
                  (when keyword
                    (setq entry (append entry (list :keyword keyword))))
                  (when root
                    (setq entry (append entry (list :root root))))
                  (when children
                    (let ((idx -1))
                      (setq entry (append entry (list :children (map 'vector (lambda (x) (parse x tree (incf idx))) children))))))
                  (append tree entry))))
      (let ((json-object-type 'plist)
            (json-array-type 'vector)
            (json-key-type 'string)
            (bookmarks (org-bookmarks-parse org-file)))
        (with-temp-buffer
          (insert
           (json-encode
            (list :guid "root________"
                  :title ""
                  :index 0
                  :dateAdded timestamp
                  :lastModified timestamp
                  :id (gen-id)
                  :typeCode 2
                  :type (cdr (assoc "folder" type-value-list))
                  :root "placesRoot"
                  :children (map 'vector (lambda (x) (parse x nil 0)) bookmarks))))
          (newline)
          (if json-file
              (write-region (point-min) (point-max) json-file)
            (buffer-substring-no-properties (point-min) (point-max))))))))
;; org-bookmarks-export-to-json:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-export-to-html][org-bookmarks-export-to-html:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-export-to-html
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-export-to-html")

(defun org-bookmarks-export-to-html (org-file &optional html-file)
  "Export from an Org Bookmarks file to a Mozilla/Firefox Bookmarks HTML file."
  "Export from an Org Bookmarks file, ORG-FILE,
to a Mozilla/Firefox Bookmarks HTML file, HTML-FILE.

If HTML-FILE is non-nil, then output is returned."
  (let* ((type-code-list '(("bookmark" . 1)
                           ("folder" . 2)))
         (title-code-list '(("Bookmarks Toolbar" . "PERSONAL_TOOLBAR_FOLDER")
                            ("Other Bookmarks" . "UNFILED_BOOKMARKS_FOLDER")))
         (timestamp (number-to-string (truncate (org-bookmarks-timestamp) 1000000))))
    (cl-labels
        ((indent (idt str)
                 (concat (spaces-string idt) str))
         (parse (bm str idt)
                (let* ((type (plist-get bm :type))
                       (type-code (cdr (assoc type type-code-list)))
                       (title (plist-get bm :title))
                       (title-code (cdr (assoc title title-code-list)))
                       (uri (url-encode-url (plist-get bm :uri)))
                       (keyword (plist-get bm :keyword))
                       (children (plist-get bm :children))
                       (entry (cl-case type-code
                                (2 (concat
                                    (indent idt "<DT><H3>")
                                    " ADD_DATE=\"" timestamp "\""
                                    " LAST_MODIFIED=\"" timestamp "\""
                                    (if title-code (concat " " title-code "=\"true\"") "")
                                    ">" title "</H3>"))
                                (1 (concat
                                    (indent idt "<DT>")
                                    "<A HREF=\"" uri "\""
                                    " ADD_DATE=\"" timestamp "\""
                                    " LAST_MODIFIED=\"" timestamp "\""
                                    (if keyword (concat " SHORTCUTURL=\"" keyword "\"") "")
                                    ">" title "</A>")))))
                  (when children
                    (setq entry (concat entry
                                        (indent idt "<DL><p>\n")
                                        (mapconcat (lambda (x) (parse x str (+ idt 4))) children "\n")
                                        (indent idt "</DL>\n"))))
                  entry)))
      (let ((bookmarks (org-bookmarks-parse org-file)))
        (with-temp-buffer
          (insert
           (concat
            "<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">
<TITLE>Bookmarks</TITLE>
<H1>Bookmarks Menu</H1>

<DL><p>\n"
            (mapconcat (lambda (x) (parse x "" 4)) bookmarks "\n"))
           "</DL>\n")
          (if html-file
              (write-region (point-min) (point-max) html-file)
            (buffer-substring-no-properties (point-min) (point-max))))))))
;; org-bookmarks-export-to-html:1 ends here

;; [[file:init-emacs.org::*org-bookmarks-export-to-nyxt][org-bookmarks-export-to-nyxt:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-export-to-nyxt
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-export-to-nyxt")

(defun org-bookmarks-export-to-nyxt (org-file &optional nyxt-file)
  "Export from an Org Bookmarks file, ORG-FILE,
to an NYXT Bookmarks Lisp file, NYXT-FILE.

If NYXT-FILE is non-nil, then output is returned."
  (let ((timestamp (format-time-string "%FT%T.%6NZ"))
        (title-root-list '(("Bookmarks Menu" . "bookmarksMenuFolder")
                           ("Bookmarks Toolbar" . "toolbarFolder")
                           ("Other Bookmarks" . "unfiledBookmarksFolder")
                           ("Mobile Bookmarks" . "mobileFolder"))))
    (cl-labels
        ((folder? (type)
                  (string= type "folder"))
         (tags (name)
               (split-string name " " :omit-nulls))
         (parse (bm tags)
                (let* ((type (plist-get bm :type))
                       (folder? (folder? type))
                       (title (plist-get bm :title))
                       (uri (plist-get bm :uri))
                       (keyword (plist-get bm :keyword))
                       (children (plist-get bm :children))
                       (root? (cdr (assoc title title-root-list)))
                       (tags tags))
                  (when (and folder? (not root?))
                    (setq tags (append tags (tags title))))
                  (when keyword
                    (setq tags (append tags (list keyword))))
                  (when (not folder?)
                    (insert
                     (format "%S\n" (list :url uri :title title :date timestamp :tags tags))))
                  (when children
                    (map 'vector (lambda (x) (parse x tags)) children)))))
      (let ((bookmarks (org-bookmarks-parse org-file)))
        (with-temp-buffer
          (insert "(\n")
          (map 'vector (lambda (x) (parse x '())) bookmarks)
          (insert ")\n")
          (if nyxt-file
              (write-region (point-min) (point-max) nyxt-file)
            (buffer-substring-no-properties (point-min) (point-max))))))))
;; org-bookmarks-export-to-nyxt:1 ends here

;; [[file:init-emacs.org::*Finances][Finances:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Finances
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Finances")
;; Finances:1 ends here

;; [[file:init-emacs.org::*export-taxes][export-taxes:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Finances: export-taxes
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Finances: export-taxes")

(defun export-taxes ()
  "Export tax information found at current org subtree."
  (interactive)
  (let* ((title (concat
                 "Sherman Taxes "
                 (save-mark-and-excursion
                   (save-match-data
                     (goto-char (line-beginning-position))
                     (re-search-forward "^\*+ " (line-end-position))
                     (buffer-substring-no-properties (point) (line-end-position))))))
         (target-buffer (generate-new-buffer-name (concat "*" title "*"))))
    (org-cycle 8)
    (org-copy-subtree)
    (set-buffer (get-buffer-create target-buffer))
    (org-mode)
    (buffer-disable-undo)
    (org-paste-subtree)
    (goto-char (point-min))
    (kill-region (line-beginning-position) (line-end-position))
    (insert title)
    (newline)
    (insert "------------------")
    (while (re-search-forward "^\*\\{3\\} " nil :noerror)
      (kill-region (line-beginning-position) (point))
      (insert "\n\n")
      (capitalize-region (line-beginning-position) (line-end-position))
      (goto-char (line-end-position)))
    (goto-char (point-min))
    (while (re-search-forward "^\*\\{5\\} " nil :noerror)
      (kill-region (line-beginning-position) (point))
      (insert "\n- ")
      (capitalize-region (line-beginning-position) (line-end-position)))
    (goto-char (point-min))
    (while (re-search-forward "^\*\\{7\\} " nil :noerror)
      (kill-region (line-beginning-position) (point))
      (insert "  - ")
      (capitalize-region (line-beginning-position) (line-end-position)))
    (goto-char (point-min))
    (while (re-search-forward "^\|-" nil :noerror)
      (goto-char (line-beginning-position))
      (when (save-mark-and-excursion
              (forward-line -1)
              (not (looking-at "^$")))
        (newline))
      (goto-char (org-table-end)))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+TBLFM:.*$" nil :noerror)
      (kill-region (line-beginning-position) (1+ (point))))
    (goto-char (point-min))
    (switch-to-buffer target-buffer)))
;; export-taxes:1 ends here

;; [[file:init-emacs.org::*duluth-hotel-invoiced-expense][duluth-hotel-invoiced-expense:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Finances: duluth-hotel-invoiced-expense
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Finances: duluth-hotel-invoiced-expense")

(defun duluth-hotel-invoiced-expense (balance)
  "Insert Duluth Hotel Invoiced Expense."
  (interactive "*sTotal Balance: ")
  (org-table-goto-column 5)
  (org-table-blank-field)
  (org-table-recalculate)
  (org-table-goto-column 7)
  (let* ((invoice (/ (round (* (- (string-to-number (org-table-get-field))
                                  (string-to-number balance))
                               100)) 100.0)))
    (org-table-goto-column 5)
    (insert (format "%.2f" invoice))
    (org-table-recalculate)))
;; duluth-hotel-invoiced-expense:1 ends here

;; [[file:init-emacs.org::*nwm-add-monthly-account-data][nwm-add-monthly-account-data:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Finances: nwm-add-monthly-account-data
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Finances: nwm-add-monthly-account-data")

(defun nwm-add-monthly-account-data (data)
  "Add Northwestern Mutual monthly entries for all accounts found in DATA."
  (interactive "*sNorthwestern Mutual Account Summary: ")
  (cl-labels
      ((date-to-year-month (date)
                           (concat
                            (substring date 6 10)
                            (substring date 0 2))))
    (let ((buffer (get-buffer "personal-encrypted.org.cpt"))
          (tables '("NWM_A40_344433"
                    "NWM_A40_344458"
                    "NWM_A40_345190"
                    "NWM_B40_300756"
                    "NWM_B40_300798"))
          date
          balances)
      (with-temp-buffer
        (insert data)
        (goto-char (point-min))
        (re-search-forward "^Period Ending: ")
        (setq date (date-to-year-month
                    (buffer-substring-no-properties (point) (line-end-position))))
        (mapcar (lambda (x)
                  (re-search-forward "%\\([0-9,]*\.[0-9][0-9]\\)[0-9.]*%")
                  (push (replace-regexp-in-string "," "" (match-string 1)) balances))
                tables))
      (with-current-buffer buffer
        (do ((tables tables (cdr tables))
             (balances (nreverse balances) (cdr balances)))
            ((null tables))
          (goto-char (point-min))
          (re-search-forward (concat "^[ \t]*#\\+NAME: " (car tables) "$"))
          (goto-char (org-table-end))
          (forward-line -4)
          (org-table-insert-row)
          (org-table-goto-column 1)
          (insert "#")
          (org-table-goto-column 2)
          (insert date)
          (org-table-goto-column 4)
          (insert (car balances))
          (org-table-recalculate)
          (re-search-forward (concat "^[ \t]*#\\+NAME: " (car tables) "_STATS$"))
          (forward-line 1)
          (org-table-recalculate)
          (org-table-recalculate))))))
;; nwm-add-monthly-account-data:1 ends here

;; [[file:init-emacs.org::*Magic the Gathering][Magic the Gathering:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Magic the Gathering
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Magic the Gathering")
;; Magic the Gathering:1 ends here

;; [[file:init-emacs.org::*mtg-cards-owned-file-name][mtg-cards-owned-file-name:1]]
(defconst mtg-cards-owned-file-name (file-truename (expand-file-name "~/org/magic-the-gathering-cards-owned.org")))
;; mtg-cards-owned-file-name:1 ends here

;; [[file:init-emacs.org::*mtg-card-list][mtg-card-list:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Magic the Gathering: mtg-card-list
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Magic the Gathering: mtg-card-list")

(defun mtg-card-list ()
  "Create a list of Magic The Gathering cards in inventory."
  (interactive)
  (unless (string= (buffer-name) (file-name-nondirectory mtg-cards-owned-file-name))
    (find-file mtg-cards-owned-file-name))
  ;; create a copy of the org buffer to work with
  (let ((source-buffer (current-buffer))
        (target-buffer (generate-new-buffer-name "*mtg*")))
    (set-buffer (get-buffer-create target-buffer))
    (buffer-disable-undo)
    (insert-buffer-substring source-buffer)
    (goto-char (point-min))
    (search-forward "* mtg cards owned")
    (goto-char (line-beginning-position))
    (kill-region (point-min) (point))
    (unless (search-forward "* mtg card sets" nil :noerror)
      (goto-char (point-max)))
    (goto-char (line-beginning-position))
    (kill-region (point) (point-max))
    (org-mode)
    (goto-char (point-min))
    (outline-show-subtree)
    (while (re-search-forward "^| +|.*\n" nil :noerror)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "| +Artist" nil :noerror)
      (org-table-delete-column))
    (goto-char (point-min))
    (switch-to-buffer target-buffer)))
;; mtg-card-list:1 ends here

;; [[file:init-emacs.org::*mtg-deck-search][mtg-deck-search:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Magic the Gathering: mtg-deck-search
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Magic the Gathering: mtg-deck-search")

(defun mtg-deck-search ()
  "Show cards owned from a deck list.

LIST must have one card per line with the number of cards
followed by the card name."
  (interactive)
  (let ((buffer (current-buffer))
        cards
        (ignored '("Plains" "Island" "Swamp" "Mountain" "Forest")))
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "^\s*\\([0-9]+\\)\s*\\(.*\\)\s*$" nil :noerror)
          (let ((name (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                (count (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
            (setq name (replace-regexp-in-string "\s*\\[.*\\]\s*" "" name))
            (unless (member name ignored)
              (push (list name count) cards))))))
    (setq cards (sort cards (lambda (a b) (string< (car a) (car b)))))
    ;;(message "%S" cards)
    (find-file mtg-cards-owned-file-name)
    (occur (concat "\\(" (substring (apply #'concat (mapcar (lambda (x) (concat "\\|" (car x))) cards)) 2) "\\)"))
    (switch-to-buffer buffer)))
;; mtg-deck-search:1 ends here

;; [[file:init-emacs.org::*mtg-set-to-table][mtg-set-to-table:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Magic the Gathering: mtg-set-to-table
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Magic the Gathering: mtg-set-to-table")

(defun mtg-set-to-table (&optional owned)
  "Convert `http://gatherer.wizards.com/' set checklist list to table.

If OWNED is non-nil, add an Owned column to the table."
  (interactive "P")
  (goto-char (point-min))
  (let ((source-buffer (current-buffer))
        (target-buffer (generate-new-buffer-name "*mtg-set-table*")))
    (set-buffer (get-buffer-create target-buffer))
    (buffer-disable-undo)
    (insert-buffer-substring source-buffer)
    (goto-char (point-min))
    (delete-non-matching-lines "<tr class=\"cardItem\">" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "</tr><tr" nil :noerror)
      (replace-match "</tr>\n<tr"))
    (goto-char (point-min))
    (while (re-search-forward "<td[^>]*>" nil :noerror)
      (replace-match "| "))
    (goto-char (point-min))
    (while (re-search-forward "</td>" nil :noerror)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "^\s*<tr[^>]*>" nil :noerror)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "</tr>" nil :noerror)
      (replace-match "|"))
    (goto-char (point-min))
    (while (re-search-forward "<a[^>]*>" nil :noerror)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "</a>" nil :noerror)
      (replace-match ""))
    (goto-char (point-min))
    (goto-char (point-min))
    (insert (concat "|-+-+-+-+-+-|"))
    (newline)
    (insert (concat "| # | Name | Artist | Color | Rarity | Set |"))
    (newline)
    (insert (concat "|-+-+-+-+-+-|"))
    (newline)
    (goto-char (point-max))
    (insert (concat "|-+-+-+-+-+-|"))
    (goto-char (point-min))
    (org-mode)
    (org-table-align)
    (when owned
      (org-table-insert-column)
      (insert "Owned")
      (org-table-align))
    (switch-to-buffer target-buffer)))
;; mtg-set-to-table:1 ends here

;; [[file:init-emacs.org::*MechWarrior Online][MechWarrior Online:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: MechWarrior Online
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: MechWarrior Online")
;; MechWarrior Online:1 ends here

;; [[file:init-emacs.org::*mwo-export-mech][mwo-export-mech:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: MechWarrior Online: mwo-export-mech
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: MechWarrior Online: mwo-export-mech")

(defun mwo-export-mech ()
  "Export mech found at current org subtree."
  (interactive)
  (let ((target-buffer (generate-new-buffer-name "*mwo-mech*")))
    (org-cycle 8)
    (org-copy-subtree)
    (set-buffer (get-buffer-create target-buffer))
    (buffer-disable-undo)
    (org-paste-subtree)
    (goto-char (point-min))
    (while (re-search-forward "^\*+ " (line-end-position) :noerror)
      (replace-match ""))
    (while (re-search-forward "^\*+ " nil :noerror)
      (kill-region (line-beginning-position) (point))
      (newline)
      (capitalize-region (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))
      (newline))
    (switch-to-buffer target-buffer)
    (goto-char (point-min))))
;; mwo-export-mech:1 ends here

;; [[file:init-emacs.org::*Dungeons and Dragons Online][Dungeons and Dragons Online:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Dungeons and Dragons Online
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Dungeons and Dragons Online")
;; Dungeons and Dragons Online:1 ends here

;; [[file:init-emacs.org::*ddo-get-item-info][ddo-get-item-info:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Dungeons and Dragons Online: ddo-get-item-info
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Dungeons and Dragons Online: ddo-get-item-info")

(defun ddo-get-item-info (&optional item)
  "Look up ITEM in the DDO Wiki and return information about it."
  (interactive "sDDO Item Name: ")
  (save-current-buffer
    (let* ((url (url-encode-url (concat "http://ddowiki.com/index.php?search=" item)))
           (buffer (url-retrieve-synchronously url))
           (enchantments ""))
      (when buffer
        (set-buffer buffer)
        (goto-char (point-min))
        (when (re-search-forward "Enchantments" nil :noerror)
          (let ((start (point)))
            (when (re-search-forward "</td>" nil :noerror)
              (let ((end (point)))
                (goto-char start)
                (while (re-search-forward "<a .*>\\([^<]*\\)</a>" end :noerror)
                  (setq enchantments
                        (concat
                         enchantments
                         (if (zerop (length enchantments)) "" ", ")
                         (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
                  (when (re-search-forward "</span>\\([^<]*\\)</li>" end :noerror)
                    (setq enchantments
                          (concat
                           enchantments
                           (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))))))
        (concat item " (" enchantments  ")\n" url)))))
;; ddo-get-item-info:1 ends here

;; [[file:init-emacs.org::*ddo-fix-wiki-description][ddo-fix-wiki-description:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Dungeons and Dragons Online: ddo-fix-wiki-description
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Dungeons and Dragons Online: ddo-fix-wiki-description")

(defun ddo-fix-wiki-description ()
  "Fix wiki description around point."
  (interactive)
  (goto-char (line-beginning-position))
  (while (not (looking-at "\\*"))
    (insert ", ")
    (goto-char (line-beginning-position))
    (delete-char -1)
    (goto-char (line-beginning-position)))
  (while (re-search-forward "Icon tooltip\.png" (line-end-position) :noerror)
    (replace-match ""))
  (goto-char (line-beginning-position))
  (while (re-search-forward " +," (line-end-position) :noerror)
    (replace-match ","))
  (goto-char (line-beginning-position))
  (while (re-search-forward ",," (line-end-position) :noerror)
    (replace-match ","))
  (goto-char (line-beginning-position))
  (while (re-search-forward "  +" (line-end-position) :noerror)
    (replace-match " "))
  (goto-char (line-beginning-position))
  (while (re-search-forward " +$" (line-end-position) :noerror)
    (replace-match ""))
  (goto-char (line-end-position))
  (insert ")"))
;; ddo-fix-wiki-description:1 ends here

;; [[file:init-emacs.org::*Ironsworn][Ironsworn:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Ironsworn
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Ironsworn")
;; Ironsworn:1 ends here

;; [[file:init-emacs.org::*Org Website][Org Website:1]]
;;==============================================================================
;;; Org Website
;;==============================================================================

(init-message 1 "Org Website")
;; Org Website:1 ends here

;; [[file:init-emacs.org::*Configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Configuration")
;; Configuration:1 ends here

;; [[file:init-emacs.org::*Publish Configuration][Publish Configuration:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Configuration: Publish Configuration
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Configuration: Publish Configuration")

(use-package org
  :straight (:type built-in)
  :commands (org-export-as
             org-export-to-buffer
             org-export-to-file)
  :functions (org-export-collect-footnote-definitions
              org-export-data
              org-export-define-derived-backend
              org-export-footnote-first-reference-p
              org-export-get-footnote-number
              org-export-get-previous-element
              org-export-get-relative-level
              org-website-convert-url-to-gopher-selector-hostname-port)
  :config
  (when (require 'ox nil :no-error)
    ;; add gopher link support (i.e. [[gopher:LINK]])
    (defun org-link-gopher-export-link (link desc format)
      "Create export version of LINK and DESC to FORMAT."
      (let ((link (concat "gopher:" link)))
        (cond
         ((eq format 'html)
          (format "<a href=\"%s\">%s</a>" link desc))
         ((eq format 'latex)
          (format "\\href{%s}{%s}" link desc))
         ((eq format 'gopher)
          (format "1\t%s\t%s" desc (org-website-convert-url-to-gopher-selector-hostname-port link)))
         (t
          (format "[%s](%s)" desc link)))))
    (org-link-set-parameters "gopher" :export #'org-link-gopher-export-link))

  (require 'ox-publish nil :no-error)
  (require 'ox-ascii nil :no-error)

  (when (require 'ox-html nil :no-error)
    (let* ((site-extension "org")
           (shared-extension "css")
           (statics-extension (concat "html\\|css\\|js\\|sh\\|"
                                      "txt\\|rss\\|"
                                      "ico\\|gif\\|png\\|jpg\\|"
                                      "mp4\\|webm\\|"
                                      "eot\\|ttf\\|woff\\|"
                                      "pdf\\|odt\\|doc\\|docx\\|"
                                      "tgz\\|zip\\|"
                                      "xml\\|xsd\\|xsl"))
           (assets-extension (concat "org\\|" statics-extension)))

      ;; website publishing instructions
      (setq org-publish-project-alist
            `(;; nullman site
              ("nullman" :components ("nullman-shared"
                                      "nullman-assets"
                                      "nullman-site"
                                      "nullman-site-statics"
                                      "nullman-site-gopher"
                                      "rsync"))
              ("nullman-shared"
               :base-directory "~/web/sites/shared"
               :base-extension ,shared-extension
               :publishing-directory "~/web/sites/nullman/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nullman-assets"
               :base-directory "~/web/sites/nullman/assets"
               :base-extension ,assets-extension
               :publishing-directory "~/web/sites/nullman/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nullman-site"
               :base-directory "~/web/sites/nullman/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_html/sites/nullman"
               :publishing-function org-website-html-publish-to-html
               :recursive t)
              ("nullman-site-statics"
               :base-directory "~/web/sites/nullman/site"
               :base-extension ,statics-extension
               :publishing-directory "~/public_html/sites/nullman"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nullman-site-gopher"
               :base-directory "~/web/sites/nullman/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_gopher/sites/nullman"
               :publishing-function org-website-gopher-publish-to-gopher
               :recursive t)
              ;; nulldot site
              ("nulldot" :components ("nulldot-shared"
                                      "nulldot-assets"
                                      "nulldot-site"
                                      "nulldot-site-statics"
                                      ;;"nulldot-site-rss"
                                      "rsync"))
              ("nulldot-shared"
               :base-directory "~/web/sites/shared"
               :base-extension ,shared-extension
               :publishing-directory "~/web/sites/nulldot/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nulldot-assets"
               :base-directory "~/web/sites/nulldot/assets"
               :base-extension ,assets-extension
               :publishing-directory "~/web/sites/nulldot/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nulldot-site"
               :base-directory "~/web/sites/nulldot/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_html/sites/nulldot"
               :publishing-function org-website-html-publish-to-html
               :recursive t)
              ("nulldot-site-statics"
               :base-directory "~/web/sites/nulldot/site"
               :base-extension ,statics-extension
               :publishing-directory "~/public_html/sites/nulldot"
               :publishing-function org-publish-attachment
               :recursive t)
              ;; nulldot-site-rss is no longer used as the rss feed is generated from a tangle block in nulldot.org
              ;; ("nulldot-site-rss"
              ;;  :base-directory "~/web/sites/nulldot/site"
              ;;  :base-extension ,site-extension
              ;;  :rss-extension "rss"
              ;;  :rss-feed-url "http://nulldot.net/"
              ;;  :rss-image-url "http://nulldot.net/img/image.png"
              ;;  :publishing-directory "~/public_html/sites/nulldot"
              ;;  :publishing-function org-website-rss-publish-to-rss
              ;;  :exclude "."
              ;;  :include ("index.org"))
              ;; nullware site
              ("nullware" :components ("nullware-shared"
                                       "nullware-assets"
                                       "nullware-site"
                                       "nullware-site-statics"
                                       "rsync"))
              ("nullware-shared"
               :base-directory "~/web/sites/shared"
               :base-extension ,shared-extension
               :publishing-directory "~/web/sites/nullware/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nullware-assets"
               :base-directory "~/web/sites/nullware/assets"
               :base-extension ,assets-extension
               :publishing-directory "~/web/sites/nullware/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("nullware-site"
               :base-directory "~/web/sites/nullware/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_html/sites/nullware"
               :publishing-function org-website-html-publish-to-html
               :recursive t)
              ("nullware-site-statics"
               :base-directory "~/web/sites/nullware/site"
               :base-extension ,statics-extension
               :publishing-directory "~/public_html/sites/nullware"
               :publishing-function org-publish-attachment
               :recursive t)
              ;; kylesherman site
              ("kylesherman" :components ("kylesherman-shared"
                                          "kylesherman-assets"
                                          "kylesherman-site"
                                          "kylesherman-site-statics"
                                          "rsync"))
              ("kylesherman-shared"
               :base-directory "~/web/sites/shared"
               :base-extension ,shared-extension
               :publishing-directory "~/web/sites/kylesherman/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("kylesherman-assets"
               :base-directory "~/web/sites/kylesherman/assets"
               :base-extension ,assets-extension
               :publishing-directory "~/web/sites/kylesherman/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("kylesherman-site"
               :base-directory "~/web/sites/kylesherman/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_html/sites/kylesherman"
               :publishing-function org-website-html-publish-to-html
               :recursive t)
              ("kylesherman-site-statics"
               :base-directory "~/web/sites/kylesherman/site"
               :base-extension ,statics-extension
               :publishing-directory "~/public_html/sites/kylesherman"
               :publishing-function org-publish-attachment
               :recursive t)
              ;; shermanwest site
              ("shermanwest" :components ("shermanwest-shared"
                                          "shermanwest-assets"
                                          "shermanwest-site"
                                          "shermanwest-site-statics"
                                          "rsync"))
              ("shermanwest-shared"
               :base-directory "~/web/sites/shared"
               :base-extension ,shared-extension
               :publishing-directory "~/web/sites/shermanwest/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("shermanwest-assets"
               :base-directory "~/web/sites/shermanwest/assets"
               :base-extension ,assets-extension
               :publishing-directory "~/web/sites/shermanwest/site"
               :publishing-function org-publish-attachment
               :recursive t)
              ("shermanwest-site"
               :base-directory "~/web/sites/shermanwest/site"
               :base-extension ,site-extension
               :publishing-directory "~/public_html/sites/shermanwest"
               :publishing-function org-website-html-publish-to-html
               :recursive t)
              ("shermanwest-site-statics"
               :base-directory "~/web/sites/shermanwest/site"
               :base-extension ,statics-extension
               :publishing-directory "~/public_html/sites/shermanwest"
               :publishing-function org-publish-attachment
               :recursive t)
              ;; rsync
              ("rsync"
               :base-directory "~/web"
               :base-extension "none"
               :publishing-directory "~/public_html")
              ;;:completion-function org-website-rsync-to-localhost-morpheus)
              ))

      ;; links
      (setq org-link-abbrev-alist
            (append
             ;; inernal links
             '(("about" . "http://nullman.net/about/")
               ("blog" . "%(org-website-blog-url)")
               ("emacs" . "http://nullman.net/emacs/")
               ("nulldot" . "http://nulldot.net/")
               ("nullman" . "http://nullman.net/")
               ("pet-peeves" . "http://nullman.net/rants/pet-peeves.html")
               ("powerhouse" . "http://powerhouse.nullware.com/")
               ("projects" . "http://nullman.net/projects/")
               ("rants" . "http://nullman.net/rants/")
               ("tutorials" . "http://nullman.net/tutorials/"))
             ;; external links
             '(("cowiki" . "http://www.champions-online-wiki.com/wiki/")
               ("google" . "http://www.google.com/search?q=%h")
               ("urban" . "http://www.urbandictionary.com/define.php?term=")
               ("wiki" . "http://en.wikipedia.org/wiki/Special:Search?search=")
               ("word" . "http://en.wiktionary.org/wiki/")
               ("youtube" . "https://www.youtube.com/watch?v="))))

      ;; use h1 tags for top level headlines (instead of h2)
      (setq org-html-toplevel-hlevel 1)

      ;; remove table attributes
      (setq org-html-table-default-attributes nil)
      ;;(setq org-html-table-default-attributes '(:class "org-table"))
      )))
;; Publish Configuration:1 ends here

;; [[file:init-emacs.org::*Menu Lists][Menu Lists:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Configuration: Menu Lists
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Configuration: Menu Lists")

;; standard menu list
(defconst org-website-menu-list
  '((:home . (:name "Home" :title "Home Page" :url "http://nullman.net/"))
    (:blog . (:name "Blog" :title "Personal Blog" :url "http://nulldot.net/"))
    (:applications . (:name "Applications" :title "Nullware Applications" :url "http://nullware.com/"))
    (:emacs . (:name "Emacs" :title "Emacs Customizations" :url "http://nullman.net/emacs/"))
    (:projects . (:name "Projects" :title "Computer Programming Projects" :url "http://nullman.net/projects/"))
    (:tutorials . (:name "Tutorials" :title "Computer Tutorials" :url "http://nullman.net/tutorials/"))
    (:presentations . (:name "Presentations" :title "Presentations I've Given" :url "http://nullman.net/presentations/"))
    (:interesting . (:name "Interesting" :title "Things of Interest" :url "http://nullman.net/interesting/"))
    (:games . (:name "Games" :title "Computer Games" :url "http://nullman.net/games/"))
    (:quotes . (:name "Quotes" :title "Collected Quotes" :url "http://nullman.net/quotes/"))
    (:rants . (:name "Rants" :title "Random Ramblings and Rants" :url "http://nullman.net/rants/"))
    (:social . (:name "Social" :title "Social Links" :url "http://nullman.net/social/"))
    ;;(:twitter . (:name "Twitter" :title "Nullman Twitter Feed" :url "http://nullman.net/social/twitter.html"))
    (:photos . (:name "Photos" :title "Personal Photographs" :url "http://nullman.net/photos/"))
    (:bookmarks . (:name "Bookmarks" :title "Personal Bookmarks" :url "http://nullman.net/bookmarks/"))
    (:about . (:name "About" :title "About This Site and Its Author" :url "http://nullman.net/about/")))
  "Website standard menu list.

Format: ((TAG . (:name NAME :title TITLE :url URL)) ... )")

;; shermanwest menu list
(defconst org-website-shermanwest-menu-list
  '((:home . (:name "Home" :title "Home Page" :url "http://shermanwest.com/"))
    (:pictures . (:name "Pictures" :title "Condo Pictures" :url "http://www.flickr.com/photos/nullman/sets/72157603365914146/show/"))
    (:directions . (:name "Directions" :title "Sherman West Directions" :url "http://shermanwest.com/directions.html"))
    (:restaurants . (:name "Restaurants" :title "San Diego Restaurants" :url "http://shermanwest.com/restaurants.html"))
    (:bars . (:name "Bars" :title "San Diego Bars" :url "http://shermanwest.com/bars.html"))
    (:attractions . (:name "Attractions" :title "San Diego Attractions" :url "http://shermanwest.com/attractions.html"))
    (:shopping . (:name "Shopping" :title "San Diego Shopping" :url "http://shermanwest.com/shopping.html"))
    (:rules . (:name "Rules" :title "House Rules" :url "http://shermanwest.com/rules.html"))
    (:guestbook . (:name "Guestbook" :title "Guestbook" :url "http://shermanwest.com/guestbook.html")))
  "Website Sherman West menu list.

Format: ((TAG . (:name NAME :title TITLE :url URL)) ... )")

;; gopher menu list
(defconst org-website-gopher-menu-list
  '((:home . (:name "Home" :title "Home Page" :selector "/nullman/index.gopher"))
    (:blog . (:name "Blog" :title "Personal Blog" :selector "/nulldot/index.gopher"))
    (:applications . (:name "Applications" :title "Nullware Applications" :selector "/nullware/index.gopher"))
    (:emacs . (:name "Emacs" :title "Emacs Customizations" :selector "/nullman/emacs/index.gopher"))
    (:projects . (:name "Projects" :title "Computer Programming Projects" :selector "/nullman/projects/index.gopher"))
    (:tutorials . (:name "Tutorials" :title "Computer Tutorials" :selector "/nullman/tutorials/index.gopher"))
    (:presentations . (:name "Presentations" :title "Presentations I've Given" :selector "/nullman/presentations/index.gopher"))
    (:interesting . (:name "Interesting" :title "Things of Interest" :selector "/nullman/interesting/index.gopher"))
    (:games . (:name "Games" :title "Computer Games" :selector "/nullman/games/index.gopher"))
    (:quotes . (:name "Quotes" :title "Collected Quotes" :selector "/nullman/quotes/index.gopher"))
    (:rants . (:name "Rants" :title "Random Ramblings and Rants" :selector "/nullman/rants/index.gopher"))
    (:social . (:name "Social" :title "Social Links" :selector "/nullman/social/index.gopher"))
    ;;(:twitter . (:name "Twitter" :title "Nullman Twitter Feed" :selector "/nullman/social/twitter.gopher"))
    (:photos . (:name "Photos" :title "Personal Photographs" :selector "/nullman/photos/index.gopher"))
    (:bookmarks . (:name "Bookmarks" :title "Personal Bookmarks" :selector "/nullman/bookmarks/index.gopher"))
    (:about . (:name "About" :title "About This Site and Its Author" :selector "/nullman/about/index.gopher")))
  "Website Gopher menu list.

Format: ((TAG . (:name NAME :title TITLE :selector SELECTOR)) ... )")
;; Menu Lists:1 ends here

;; [[file:init-emacs.org::*Gopher Configuration][Gopher Configuration:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Configuration: Gopher Configuration
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Configuration: Gopher Configuration")

;; gopher server port
(defconst gopher-port 70
  "Port used by Gopher server.")

;; gopher max text width before word wrapping
(defconst gopher-text-width org-ascii-text-width
  "Maximum width of Gopher text before word wrapping.")

;; gopher item types
;; INFO https://en.wikipedia.org/wiki/Gopher_(protocol)
(defconst gopher-type-text-file "0")
(defconst gopher-type-submenu "1")
(defconst gopher-type-binary-file "9")
(defconst gopher-type-gif-file "g")
(defconst gopher-type-image-file "i")
(defconst gopher-type-html-file "h")
(defconst gopher-type-sound-file "s")
;; Gopher Configuration:1 ends here

;; [[file:init-emacs.org::*Functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::*Get Property List][Get Property List:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Property List
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Property List")

(defun org-website-get-property-list (info)
  "Return an association list of org properties in INFO."
  (org-element-map (plist-get info :parse-tree) 'keyword
    (lambda (x)
      (let ((key (intern (concat ":" (replace-regexp-in-string "_" "-" (downcase (org-element-property :key x)))))))
        (cons key (org-element-property :value x))))))
;; Get Property List:1 ends here

;; [[file:init-emacs.org::*Get Property Element][Get Property Element:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Property Element
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Property Element")

(defun org-website-get-property-element (property-list element)
  "Return ELEMENT from PROPERTY-LIST returned from `org-website-get-property-list', or empty string if element was not found."
  (or (cdr (assoc element property-list)) ""))
;; Get Property Element:1 ends here

;; [[file:init-emacs.org::*Get URL][Get URL:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get URL
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get URL")

(defun org-website-get-url (property-list)
  "Return target URL for current buffer being exported."
  (concat
   (org-website-get-property-element property-list :link-home)
   (replace-regexp-in-string
    "^.*/site/" ""
    (replace-regexp-in-string "\.[^\.]*$" "" buffer-file-name))
   ".html"))
;; Get URL:1 ends here

;; [[file:init-emacs.org::*Blog URL][Blog URL:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Blog URL
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Blog URL")

(defun org-website-blog-url (name)
  "Return URL of given blog NAME."
  (let ((parts (split-string name ".")))
    (concat
     "http://nulldot.net/"
     (nth 0 parts) "/"
     (nth 1 parts) "/"
     name ".html")))
;; Blog URL:1 ends here

;; [[file:init-emacs.org::*Is Blog Post][Is Blog Post:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Is Blog Post
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Is Blog Post")

(defun org-website-is-blog-post (property-list)
  "Return non-nil if current page is a blog entry."
  (zerop (length (replace-regexp-in-string
                  "^.*/[0-9]\\{4\\}.[0-9]\\{2\\}.[0-9]\\{2\\}.[0-9]\\{4\\}-.*$"
                  ""
                  buffer-file-name))))
;; Is Blog Post:1 ends here

;; [[file:init-emacs.org::*Get Level][Get Level:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Level
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Level")

(defun org-website-get-level (property-list)
  "Return level of current project file."
  (- (length (split-string (org-website-get-url property-list) "/")) 3))
;; Get Level:1 ends here

;; [[file:init-emacs.org::*Format Headline][Format Headline:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Format Headline
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Format Headline")

(defun org-website-format-headline (headline &optional char col)
  "Return HEADLINE surrounded by CHAR ending at COL length."
  (let* ((char (or char ?=))
         (col (or col org-ascii-text-width))
         (text-width (- col 12))
         ;; if headline is too long, split it into multiple lines
         (headline (if (> (length headline) text-width)
                       (let ((fill-column text-width))
                         (with-temp-buffer
                           (insert headline)
                           (fill-region (point-min) (point-max))
                           (buffer-string)))
                     headline)))
    (mapconcat
     (lambda (line)
       (let ((str (concat (make-string 5 char) " " (upcase line) " " (make-string 5 char))))
         (while (< (length str) col)
           (setq str (concat str (string char))))
         str))
     (split-string headline "\n") "\n")))
;; Format Headline:1 ends here

;; [[file:init-emacs.org::*Get Gopher Selector Hostname Port][Get Gopher Selector Hostname Port:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Gopher Selector Hostname Port
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Gopher Selector Hostname Port")

(defun org-website-get-gopher-selector-hostname-port (selector)
  "Return Gopher selector, hostname, and port from given SELECTOR."
  (let* ((hostname "nullman.net")
         (port (if (boundp 'gopher-port) gopher-port 70)))
    (concat selector "\t" hostname "\t" (int-to-string port))))
;; Get Gopher Selector Hostname Port:1 ends here

;; [[file:init-emacs.org::*Convert URL to Gopher Selector Hostname Port][Convert URL to Gopher Selector Hostname Port:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Convert URL to Gopher Selector Hostname Port
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Convert URL to Gopher Selector Hostname Port")

(defun org-website-convert-url-to-gopher-selector-hostname-port (url)
  "Return Gopher selector, hostname, and port from given URL."
  (let* ((suffix (replace-regexp-in-string "^.*://" "" url))
         (hostname (replace-regexp-in-string "/.*$" "" suffix))
         (selector (replace-regexp-in-string "^.*?/" "/" suffix))
         (port (if (boundp 'gopher-port) gopher-port 70)))
    (concat selector "\t" hostname "\t" (int-to-string port))))
;; Convert URL to Gopher Selector Hostname Port:1 ends here

;; [[file:init-emacs.org::*Gopher Justify Lines][Gopher Justify Lines:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Gopher Justify Lines
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Gopher Justify Lines")

(defun org-website-gopher-justify-lines (str &optional indent width justify)
  "Return STR after justifying all lines.

INDENT is the amount to indent the final text (defaults to 0).

WIDTH is an integer specifying maximum length of a line (defaults
to `gopher-text-width' or `org-ascii-text-width').

JUSTIFY determines the type of justification: `left', `right',
`full', `center', or `none' (defaults to `none')."
  (with-temp-buffer
    (let ((indent (or indent 0))
          (fill-column (or width
                           (and (boundp 'gopher-text-width)
                                gopher-text-width)
                           org-ascii-text-width)))
      (insert str)
      ;; replace all newlines with doubles to prevent their removal when justifying
      (goto-char (point-min))
      (while (search-forward "\n" nil :noerror)
        (replace-match "\n\n"))
      ;; justify string
      (fill-region (point-min) (point-max) justify :nosqueeze :to-eop)
      ;; replace double newlines with single to return to original format
      (goto-char (point-min))
      (while (search-forward "\n\n" nil :noerror)
        (replace-match "\n"))
      ;; indent
      (when (> indent 0)
        (let ((spc (make-string indent ? )))
          (goto-char (point-min))
          (while (not (eobp))
            (insert spc)
            (goto-char (line-beginning-position))
            (forward-line 1))))
      ;; return justified string
      (buffer-string))))
;; Gopher Justify Lines:1 ends here

;; [[file:init-emacs.org::*Publish HTML][Publish HTML:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish HTML
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish HTML")
;; Publish HTML:1 ends here

;; [[file:init-emacs.org::*Derived Backend][Derived Backend:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Derived Backend
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Derived Backend")

;; create derived backend with customizations to html backend
(org-export-define-derived-backend 'org-website-html 'html
  :translate-alist '((template . org-website-html-template)
                     (inner-template . org-website-html-inner-template)
                     (headline . org-website-html-headline)
                     (section . org-website-html-section)))
;; Derived Backend:1 ends here

;; [[file:init-emacs.org::*Publish to HTML][Publish to HTML:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Publish to HTML
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Publish to HTML")

(defun org-website-html-publish-to-html (plist file-name pub-dir)
  "Publish a Website org file to HTML and return output file name.

FILE-NAME is the file name of the Org file to be published.

PLIST is the property list for the given project.

PUB-DIR is the publishing directory."
  (org-publish-org-to 'org-website-html file-name
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))
;; Publish to HTML:1 ends here

;; [[file:init-emacs.org::*Template][Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Template")

;; custom html template
;; INFO reference: http://orgmode.org/worg/dev/org-export-reference.html
(defun org-website-html-template (contents info)
  "Return complete document string after HTML conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (let* ((property-list (org-website-get-property-list info))
         (site (org-website-get-property-element property-list :site))
         (title (org-website-get-property-element property-list :title)))
    ;;(message "%S" info)
    ;;(message "%S" property-list)
    (concat
     ;; "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
     ;; "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
     "<!DOCTYPE html>\n"
     (format "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\">\n" (org-website-get-property-element property-list :language))
     "\n"
     "<head>\n"
     "\n"
     (format "  <title>%s</title>\n" title)
     "\n"
     (format "  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=%s\" />\n" (or (coding-system-get org-html-coding-system 'mime-charset) "utf-8"))
     "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n"
     (format "  <meta name=\"author\" content=\"%s\" />\n" (org-website-get-property-element property-list :author))
     (format "  <meta name=\"keywords\" content=\"%s\" />\n" (org-website-get-property-element property-list :keywords))
     (format "  <meta name=\"description\" content=\"%s\" />\n" (org-website-get-property-element property-list :description))
     "  <meta name=\"generator\" content=\"org-mode\" />\n"
     "  <meta name=\"robots\" content=\"all\" />\n"
     "  <meta name=\"google-site-verification\" content=\"" (org-website-get-property-element property-list :google-site-verification) "\" />\n"
     "\n"
     "  <link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS Feed\" href=\"http://nulldot.net/index.rss\" />\n"
     "  <link rel=\"author\" href=\"http://nullman.net/about.html\" />\n"
     "  <link rel=\"home\" href=\"/\" />\n"
     (cond
      ((and (string= site "kylesherman") (string= title "Resume"))
       "  <link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"/styles/resume.css\" />\n")
      (t
       "  <link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"/styles/default.css\" />\n"))
     "  <link rel=\"stylesheet\" type=\"text/css\" media=\"print\" href=\"/styles/print.css\" />\n"
     "\n"
     "  <!-- Google Analytics -->\n"
     "  <script><!--\n"
     "    var _gaq = _gaq || [];\n"
     "    _gaq.push(['_setAccount', '" (org-website-get-property-element property-list :google_analytics) "']);\n"
     "    _gaq.push(['_trackPageview']);\n"
     "    (function() {\n"
     "        var ga = document.createElement('script'); ga.async = true;\n"
     "        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n"
     "        var s = document.getElementsByTagName('script')[0];\n"
     "        s.parentNode.insertBefore(ga, s);\n"
     "    })();\n"
     "  // --></script>\n"
     "\n"
     (concat
      (org-element-normalize-string (plist-get info :html-head))
      (org-element-normalize-string (plist-get info :html-head-extra)))
     "\n"
     "</head>\n"
     "\n"
     "<body>\n"
     "\n"
     "  <div id=\"container\">\n"
     "\n"
     ;; header
     (cond
      ((and (string= site "kylesherman") (string= title "Resume"))
       "")
      (t
       (concat
        "    <!-- header start -->\n"
        "\n"
        "    <div id=\"header\">\n"
        "      <h1 class=\"title\">\n"
        (format "        <span>%s</span>\n" title)
        "      </h1>\n"
        "    </div>\n"
        "\n"
        "    <!-- header end -->\n"
        "\n")))
     ;; left menu
     (if (> (length (org-website-get-property-element property-list :menu)) 0)
         (concat
          "    <!-- menu start -->\n"
          "\n"
          "    <div id=\"menu\">\n"
          "\n"
          (mapconcat
           (lambda (x)
             (let* ((plist (cdr x))
                    (name (plist-get plist :name))
                    (title (plist-get plist :title))
                    (url (plist-get plist :url))
                    (current (string= (org-website-get-property-element property-list :menu) name)))
               (concat "      <div class=\"" (if current "menu-item-current" "menu-item") "\">\n"
                       "        <a title=\"" title "\" href=\"" url "\">" name "</a>\n"
                       "      </div>\n")))
           (if (string= site "shermanwest")
               org-website-shermanwest-menu-list
             org-website-menu-list)
           "")
          "\n"
          "      <!-- search start -->\n"
          "\n"
          "      <div class=\"search\">\n"
          "        <form method=\"get\" action=\"http://www.google.com/search\">\n"
          "          <fieldset>\n"
          "            <input type=\"hidden\" name=\"q\" value=\"site:http://nullman.net/ OR site:http://nulldot.net/ OR site:http://nullware.com/\" />\n"
          "            <input type=\"hidden\" name=\"hl\" value=\"en\" />\n"
          "            <input type=\"text\" name=\"q\" maxlength=\"2048\" value=\"\" title=\"Search\" style=\"width: 5.5em\" />\n"
          "            <input type=\"image\" name=\"btnG\" src=\"/img/search.png\" alt=\"Search\" style=\"height: 100%; vertical-align: middle\" />\n"
          "          </fieldset>\n"
          "        </form>\n"
          "      </div>\n"
          "\n"
          "      <!-- search end -->\n"
          "\n"
          "    </div>\n"
          "\n"
          "    <!-- menu end -->\n"
          "\n")
       "")
     ;; main content
     (cond
      ((string= site "nulldot")
       (org-website-html-blog-contents-template contents info property-list))
      ((string= site "kylesherman")
       (concat
        "    <!-- content start -->\n"
        "\n"
        (if (string= title "Resume")
            contents
          (concat
           "    <div id=\"content-generic\">\n"
           "\n"
           contents
           "\n"
           "    </div>\n"))
        "\n"
        "    <!-- content end -->\n"
        "\n"))
      (t
       (concat
        "    <!-- content start -->\n"
        "\n"
        "    <div id=\"content\">\n"
        "\n"
        contents
        "\n"
        "    </div>\n"
        "\n"
        "    <!-- content end -->\n"
        "\n")))
     ;; footer
     (cond
      ((string= site "kylesherman")
       "")
      (t
       (concat
        "    <!-- footer start -->\n"
        "\n"
        "    <div id=\"footer\">\n"
        "\n"
        "      <div class=\"validator\">\n"
        "        <a title=\"Check the validity of this page using W3C's unified validator\"\n"
        (format "           href=\"http://validator.w3.org/unicorn/check?ucn_task=conformance&amp;ucn_uri=%s\">unicorn</a>\n" (org-website-get-property-element property-list :link-home))
        "      </div>\n"
        "\n"
        "      <div class=\"validator\">\n"
        "        <a title=\"Check the validity of this page's XHTML\"\n"
        "           href=\"http://validator.w3.org/check?uri=referer\">xhtml</a>\n"
        "      </div>\n"
        "\n"
        "      <div class=\"validator\">\n"
        "        <a title=\"Check the validity of this page's CSS\"\n"
        "           href=\"http://jigsaw.w3.org/css-validator/check/referer\">css</a>\n"
        "      </div>\n"
        "\n"
        "      <div class=\"validator\">\n"
        "        <a title=\"Check the performance this page\"\n"
        (format "           href=\"https://developers.google.com/speed/pagespeed/insights/?url=%s\">pagespeed</a>\n" (org-website-get-url property-list))
        "      </div>\n"
        "\n"
        "      <!--\n"
        "      <div class=\"validator\">\n"
        "        <a title=\"Check the accessibility of this page according to U.S. Section 508\"\n"
        (format "           href=\"http://www.contentquality.com/mynewtester/cynthia.exe?Url1=%s\">508</a>\n" (org-website-get-property-element property-list :link-home))
        "      </div>\n"
        "      -->\n"
        "\n"
        "      <div class=\"license\">\n"
        "        <span>Last Modified: " (org-website-get-property-element property-list :last-modified) "<br />\n"
        "        " (org-website-get-property-element property-list :copyright) "<br />\n"
        "        <span class=\"license\">Creative Commons\n"
        "        <a title=\"View details of the license of this site\"\n"
        "           href=\"http://creativecommons.org/licenses/by-nc-sa/3.0/\">\n"
        "          Attribution-NonCommercial-ShareAlike\n"
        "        </a> license</span></span>\n"
        "      </div>\n"
        "\n"
        "    </div>\n"
        "\n"
        "    <!-- footer end -->\n"
        "\n")))
     "  </div>\n"
     "\n"
     "</body>\n"
     "\n"
     "</html>\n")))
;; Template:1 ends here

;; [[file:init-emacs.org::*Blog Contents Template][Blog Contents Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Blog Contents Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Blog Contents Template")

;; custom blog contents html template
;; INFO reference: http://orgmode.org/worg/dev/org-export-reference.html
(defun org-website-html-blog-contents-template (contents info property-list)
  "Return blog contents string after HTML conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options.

PROPERTY-LIST is the list of org properties found in INFO."
  ;; if the POSTED property exists and level is 2, then assume current file is a blog post
  (let ((post (org-website-is-blog-post property-list))
        (site-url (org-website-get-property-element property-list :link-home)))
    (concat
     ;; menu-index (right blog menu)
     (with-temp-buffer
       (insert-file-contents "~/web/sites/nulldot/site/menu-index.html-nopub")
       (goto-char (point-min))
       (while (re-search-forward "^" nil :noerror)
         (replace-match "    "))
       (buffer-string))
     "\n"
     "    <!-- content-index start -->\n"
     "\n"
     "    <div id=\"content-index\">\n"
     "\n"
     (if post
         (concat
          ;; posted date
          "      <div class=\"timestamp\">"
          (substring (org-website-get-property-element property-list :posted) 0 10)
          "</div>\n"
          ;; description and link headline
          (concat
           "      <h2><a href=\"" (org-website-get-url property-list) "\">"
           (org-website-get-property-element property-list :description)
           "</a></h2>\n"))
       "")
     contents
     (if post
         (concat
          "\n"
          ;; tags
          "      <div class=\"tags\">Tags: " (org-website-get-property-element property-list :tags) "</div>\n"
          "\n"
          ;; ;; disqus comments
          ;; "      <!-- comments start -->\n"
          ;; "\n"
          ;; "      <div id=\"comments\">\n"
          ;; "\n"
          ;; "        <div id=\"disqus_thread\"></div>\n"
          ;; "\n"
          ;; "        <script><!--\n"
          ;; "          var disqus_config = function () {\n"
          ;; "            this.page.url = \"" (org-website-get-url property-list) "\";\n"
          ;; "            this.page.identifier = \"" (org-website-get-property-element property-list :uuid) "\";\n"
          ;; "          };\n"
          ;; "          (function() {\n"
          ;; "            var d = document, s = d.createElement('script');\n"
          ;; "            s.src = '//nulldot.disqus.com/embed.js';\n"
          ;; "            s.setAttribute('data-timestamp', +new Date());\n"
          ;; "            (d.head || d.body).appendChild(s);\n"
          ;; "          })();\n"
          ;; "        // --></script>\n"
          ;; "\n"
          ;; "        <noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>\n"
          ;; "\n"
          ;; "      </div>\n"
          ;; "\n"
          ;; "      <!-- comments end -->\n"
          )
       "")
     "\n"
     "    </div>\n"
     "\n"
     "    <!-- content-index end -->\n"
     "\n")))
;; Blog Contents Template:1 ends here

;; [[file:init-emacs.org::*Inner Template][Inner Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Inner Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Inner Template")

;; custom html inner template
(defun org-website-html-inner-template (contents info)
  "Return body of document string after HTML conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (require 'ox-html)
  (concat
   ;; table of contents
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (org-html-toc depth info)))

   ;; document contents
   contents

   ;; footnotes section
   (org-website-html-footnote-section info)))
;; Inner Template:1 ends here

;; [[file:init-emacs.org::*Headline][Headline:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Headline
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Headline")

;; custom html headline
(defun org-website-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.

CONTENTS holds the contents of the headline.

INFO is a plist holding contextual information."
  (let ((level (+ (org-export-get-relative-level headline info)
                  (1- (or (plist-get info :html-toplevel-hlevel) 1))))
        (id (org-export-data (org-element-property :CUSTOM_ID headline) info))
        (title (org-export-data (org-element-property :title headline) info))
        (contents (or contents "")))
    (concat
     (if id
         (format "\n<h%d id=\"%s\">%s</h%d>\n" level id title level)
       (format "\n<h%d>%s</h%d>\n" level title level))
     contents)))
;; Headline:1 ends here

;; [[file:init-emacs.org::*Section][Section:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Section
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Section")

;; custom html section
(defun org-website-html-section (section contents info)
  "Transcode a SECTION element from Org to HTML.

CONTENTS holds the contents of the section.

INFO is a plist holding contextual information."
  (or contents ""))
;; Section:1 ends here

;; [[file:init-emacs.org::*Footnote Reference][Footnote Reference:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Footnote Reference
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Footnote Reference")

(defun org-website-html-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.

CONTENTS is nil.

INFO is a plist holding contextual information."
  (require 'ox-html)
  (concat
   ;; insert separator between two footnotes in a row
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (id (format "fnr.%d%s"
                      n
                      (if (org-export-footnote-first-reference-p
                           footnote-reference info)
                          ""
                        ".100"))))
     (format
      (plist-get info :html-footnote-format)
      (org-html--anchor
       id n (format " class=\"footnote-reference\" href=\"#fn.%d\"" n) info)))))
;; Footnote Reference:1 ends here

;; [[file:init-emacs.org::*Footnote Section][Footnote Section:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish HTML: Footnote Section
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish HTML: Footnote Section")

;; custom html footnote section
(defun org-website-html-footnote-section (info)
  "Format the footnote section.

INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (cl-loop for (n type raw) in fn-alist
                   collect (cons n (if (eq (org-element-type raw) 'org-data)
                                       (org-trim (org-export-data raw info))
                                     (format "<p>%s</p>" (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (format
       "<div class=\"footnotes\">\n%s</div>"
       (mapconcat
        (lambda (fn)
          (let ((n (car fn))
                (def (cdr fn)))
            (format
             "<div class=\"footnote\">\n%s\n%s\n</div>\n"
             (format
              org-html-footnote-format
              (org-html--anchor
               (format "fn.%s" n)
               n
               (format " class=\"footnote-number\" href=\"#fnr.%s\"" n)
               info))
             def)))
        fn-alist "\n")))))
;; Footnote Section:1 ends here

;; [[file:init-emacs.org::*Publish RSS][Publish RSS:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish RSS
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish RSS")
;; Publish RSS:1 ends here

;; [[file:init-emacs.org::*Derived Backend][Derived Backend:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish RSS: Derived Backend
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish RSS: Derived Backend")

;; create derived backend with customizations to html backend
(org-export-define-derived-backend 'org-website-rss 'html
  :translate-alist '((template . org-website-rss-template)
                     (inner-template . org-website-rss-inner-template)))
;; Derived Backend:1 ends here

;; [[file:init-emacs.org::*Publish to RSS][Publish to RSS:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish RSS: Publish to RSS
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish RSS: Publish to RSS")

(defun org-website-rss-publish-to-rss (plist file-name pub-dir)
  "Publish a Website org file to RSS and return output file name.

FILE-NAME is the file name of the Org file to be published.

PLIST is the property list for the given project.

PUB-DIR is the publishing directory."
  (org-publish-org-to 'org-website-rss file-name ".rss" plist pub-dir))
;; Publish to RSS:1 ends here

;; [[file:init-emacs.org::*Template][Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish RSS: Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish RSS: Template")

;; custom rss template
;; INFO reference: http://orgmode.org/worg/dev/org-export-reference.html
(defun org-website-rss-template (contents info)
  "Return complete document string after RSS conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (let* ((property-list (org-website-get-property-list info))
         (link-home (org-website-get-property-element property-list :link-home))
         (title (org-website-get-property-element property-list :title)))
    ;;(message "%S" info)
    ;;(message "%S" property-list)
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n"
     "         xmlns=\"http://purl.org/rss/1.0/\"\n"
     "         xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\"\n"
     "         xmlns:taxo=\"http://purl.org/rss/1.0/modules/taxonomy/\"\n"
     "         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n"
     "         xmlns:syn=\"http://purl.org/rss/1.0/modules/syndication/\"\n"
     "         xmlns:admin=\"http://webns.net/mvcb/\"\n"
     "         xmlns:feedburner=\"http://rssnamespace.org/feedburner/ext/1.0\">\n\n"
     "  <channel rdf:about=\"" link-home "\">\n"
     "    <title>" title "</title>\n"
     "    <link>" link-home "</link>\n"
     "    <description>" (org-website-get-property-element property-list :description) "</description>\n"
     "    <dc:language>en-us</dc:language>\n"
     "    <dc:rights>" (org-website-get-property-element property-list :copyright) "</dc:rights>\n"
     "    <dc:date>" (format-time-string "%FT%TZ" nil t) "</dc:date>\n"
     "    <dc:publisher>" (org-website-get-property-element property-list :author) "</dc:publisher>\n"
     "    <dc:creator>" (org-website-get-property-element property-list :author) "</dc:creator>\n"
     "    <dc:subject>Software Engineering</dc:subject>\n"
     "    <syn:updatePeriod>daily</syn:updatePeriod>\n"
     "    <syn:updateFrequency>1</syn:updateFrequency>\n"
     "    <syn:updateBase>2007-06-04T00:00:00Z</syn:updateBase>\n"
     "    <items>\n"
     "      <rdf:Seq>\n"
     ;; (mapconcat
     ;;  (lambda (x)
     ;;    (let ((file (substring (car x) 1))
     ;;          (date (cadr x))
     ;;          (desc (caddr x)))
     ;;      (concat "        <rdf:li rdf:resource=\"" link-home file ".html\" />")))
     ;;  indexes "\n")
     "      </rdf:Seq>\n"
     "    </items>\n"
     "    <image rdf:resource=\"" link-home "img/image.jpg\" />\n"
     "  </channel>\n\n"
     "  <image rdf:about=\"" link-home "img/image.jpg\">\n"
     "    <title>" title "</title>\n"
     "    <url>" link-home "img/image.jpg</url>\n"
     "    <link>" link-home "</link>\n"
     "  </image>\n"
     contents
     "\n"
     "  <textinput rdf:about=\"" link-home "search.html\">\n"
     "    <title>Search " title "</title>\n"
     "    <description>Search " title " entries</description>\n"
     "    <name>query</name>\n"
     "    <link>" link-home "search.html</link>\n"
     "  </textinput>\n\n"
     "</rdf:RDF>\n")))
;; Template:1 ends here

;; [[file:init-emacs.org::*Inner Template][Inner Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish RSS: Inner Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish RSS: Inner Template")

;; custom rss inner template
(defun org-website-rss-inner-template (contents info)
  "Return body of document string after RSS conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (let ((property-list (org-website-get-property-list info)))
    (concat
     ;; table of contents
     ;; (let ((depth (plist-get info :with-toc)))
     ;;   (when depth (org-html-toc depth info)))

     ;; document contents
     "\n"
     "  <item rdf:about=\"" (org-website-get-url property-list) "\">\n"
     "    <title>" (org-website-get-property-element property-list :description) "</title>\n"
     "    <link>" (org-website-get-url property-list) "</link>\n"
     "    <description>\n"
     "      <![CDATA[\n"
     contents
     "      ]]>\n"
     "    </description>\n"
     "  </item>\n"

     ;; footnotes section
     (org-website-html-footnote-section info))))
;; Inner Template:1 ends here

;; [[file:init-emacs.org::*Publish Gopher][Publish Gopher:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish Gopher
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish Gopher")
;; Publish Gopher:1 ends here

;; [[file:init-emacs.org::*Derived Backend][Derived Backend:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Derived Backend
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Derived Backend")

;; create derived backend with customizations to ascii backend
(org-export-define-derived-backend 'gopher 'ascii
  ;;:options-alist '((:ascii-text-width nil nil gopher-text-width))
  :translate-alist '((template . org-website-gopher-template)
                     (inner-template . org-website-gopher-inner-template)
                     ;;(inner-template . org-ascii-inner-template)
                     (headline . org-website-gopher-headline)
                     (section . org-website-gopher-section)))
;;(link . org-website-gopher-link)))
;; Derived Backend:1 ends here

;; [[file:init-emacs.org::*Publish to Gopher][Publish to Gopher:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Publish to Gopher
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Publish to Gopher")

(defun org-website-gopher-publish-to-gopher (plist file-name pub-dir)
  "Publish a Website org file to Gopher and return output file name.

FILE-NAME is the file name of the Org file to be published.

PLIST is the property list for the given project.

PUB-DIR is the publishing directory."
  (org-publish-org-to 'gopher file-name ".gopher" plist pub-dir))
;; Publish to Gopher:1 ends here

;; [[file:init-emacs.org::*Template][Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Template")

;; custom gopher template
;; INFO reference: http://orgmode.org/worg/dev/org-export-reference.html
(defun org-website-gopher-template (contents info)
  "Return complete document string after Gopher conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (let* ((property-list (org-website-get-property-list info))
         (site (org-website-get-property-element property-list :site))
         (title (org-website-get-property-element property-list :title)))
    ;;(message "%S" info)
    ;;(message "%S" property-list)
    (concat
     (format "%s\n\n" (org-website-format-headline title))
     ;; menu (only display on home page)
     (if (string= (downcase title) "nullman")
         (concat
          (format "%s\n\n" (org-website-format-headline "menu"))
          (mapconcat
           (lambda (x)
             (let* ((plist (cdr x))
                    (name (plist-get plist :name))
                    (title (plist-get plist :title))
                    (selector (plist-get plist :selector)))
               (format "%s\t%s\t%s\n" gopher-type-submenu title (org-website-get-gopher-selector-hostname-port selector))))
           (if (string= site "shermanwest")
               org-website-shermanwest-menu-list
             org-website-menu-list)
           "")
          "\n")
       "")
     ;; main content
     (cond
      ((string= site "nulldot")
       (org-website-gopher-blog-contents-template contents info property-list))
      (t
       (concat contents "\n")))
     ;; footer
     (cond
      ((string= site "kylesherman")
       "")
      (t
       (concat
        (format "%s\n\n" (make-string gopher-text-width ?=))
        (format "Last Modified: %s\n" (org-website-get-property-element property-list :last-modified))
        (format "%s\n" (org-website-get-property-element property-list :copyright))
        (format "%s\t%s\t%s\t%s\n"
                gopher-type-html-file
                "Creative Commons Attribution-NonCommercial-ShareAlike license"
                "http://creativecommons.org/licenses/by-nc-sa/3.0/"
                gopher-port))))
     )))
;; Template:1 ends here

;; [[file:init-emacs.org::*Inner Template][Inner Template:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Inner Template
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Inner Template")

;; custom gopher inner template
(defun org-website-gopher-inner-template (contents info)
  "Return body of document string after Gopher conversion.

CONTENTS is the transcoded contents string.

INFO is a plist holding export options."
  (require 'ox-ascii)
  (concat
   ;; ;; table of contents
   ;; (let ((depth (plist-get info :with-toc)))
   ;;   (when depth
   ;;     (org-ascii--build-toc info depth)))

   ;; document contents
   (replace-regexp-in-string "^  " "" contents) ; remove indentation

   ;; footnotes section
   (org-website-gopher-footnote-section info)))
;; Inner Template:1 ends here

;; [[file:init-emacs.org::*Headline][Headline:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Headline
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Headline")

;; custom gopher headline
(defun org-website-gopher-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Gopher.

CONTENTS holds the contents of the headline.

INFO is a plist holding contextual information."
  (let ((level (+ (org-export-get-relative-level headline info)
                  (1- (or (plist-get info :gopher-toplevel-hlevel) 1))))
        (text (org-export-data (org-element-property :title headline) info))
        (contents (or contents "")))
    (concat
     (format "\n%s\n" (if (< level 3)
                          (org-website-format-headline text ?= gopher-text-width)
                        (org-website-format-headline text ?- gopher-text-width)))
     contents)))
;; Headline:1 ends here

;; [[file:init-emacs.org::*Section][Section:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Section
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Section")

;; custom gopher section
(defun org-website-gopher-section (section contents info)
  "Transcode a SECTION element from Org to Gopher.

CONTENTS holds the contents of the section.

INFO is a plist holding contextual information."
  (or contents ""))
;; Section:1 ends here

;; [[file:init-emacs.org::*Footnote Reference][Footnote Reference:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Footnote Reference
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Footnote Reference")

(defun org-website-gopher-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Gopher.

CONTENTS is nil.

INFO is a plist holding contextual information."
  (require 'ox-ascii)
  (concat
   ;; insert separator between two footnotes in a row
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :ascii-footnote-separator)))
   (let ((n (org-export-get-footnote-number footnote-reference info)))
     (format
      (plist-get info :ascii-footnote-format)
      (format "%d" n)))))
;; Footnote Reference:1 ends here

;; [[file:init-emacs.org::*Footnote Section][Footnote Section:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish Gopher: Footnote Section
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish Gopher: Footnote Section")

;; custom gopher footnote section
(defun org-website-gopher-footnote-section (info)
  "Format the footnote section.

INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (cl-loop for (n type raw) in fn-alist
                   collect (cons n (if (eq (org-element-type raw) 'org-data)
                                       (org-trim (org-export-data raw info))
                                     (format "%s" (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (format
       "%s"
       (mapconcat
        (lambda (fn)
          (let ((n (car fn))
                (def (cdr fn)))
            (format
             "%s\n%s\n"
             (format
              org-ascii-footnote-format
              (format "%s" n))
             def)))
        fn-alist "\n")))))
;; Footnote Section:1 ends here

;; [[file:init-emacs.org::*Helper Functions][Helper Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Helper Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Helper Functions")
;; Helper Functions:1 ends here

;; [[file:init-emacs.org::*Publish][Publish:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Helper Functions: Publish
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Helper Functions: Publish")

(defun org-website-publish (&optional project force)
  "Publish org-website projects.

If PROJECT is non-nil, only publish that project.

If FORCE is non-nil, force publish all files in project."
  (interactive)
  (let ((files (directory-files "~/web/org/" nil "\.org\\'")))
    (when (member "styles.org" files)
      (setq files (append "styles.org" (remove "styles.org" files))))
    (dolist (file files)
      (let ((site (file-name-sans-extension file)))
        (when (or (not project)
                  (string= project site))
          ;; publish site
          (org-publish site force))))))

(defun org-website-publish-async (&optional project force)
  "Asynchronous version of `org-website-publish'."
  (interactive)
  (eval
   `(async-spinner
     (lambda ()
       (load "~/web/bin/init-emacs-website.el")
       (org-website-publish ,project ,force))
     (lambda (result)
       (message "Website publish finished")))))
;; Publish:1 ends here

;; [[file:init-emacs.org::*Tangle Publish][Tangle Publish:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Helper Functions: Tangle Publish
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Helper Functions: Tangle Publish")

(defun org-website-tangle-publish (&optional project force)
  "Tangle and publish org-website projects.

If PROJECT is non-nil, only tangle/publish that project.

If FORCE is non-nil, force publish all files in project."
  (interactive)
  (let ((files (directory-files "~/web/org/" nil "\.org\\'"))
        (org-html-htmlize-output-type 'css))
    (when (member "styles.org" files)
      (setq files (append "styles.org" (remove "styles.org" files))))
    (dolist (file files)
      (let ((site (file-name-sans-extension file)))
        (when (or (not project)
                  (string= project site))
          ;; tangle site file
          (org-babel-tangle-file (concat "~/web/org/" file))
          ;; publish site
          (org-publish site force))))))

(defun org-website-tangle-publish-async (&optional project force)
  "Asynchronous version of `org-website-tangle-publish'."
  (interactive)
  (eval
   `(async-spinner
     (lambda ()
       (load "~/web/bin/init-emacs-website.el")
       (org-website-tangle-publish ,project ,force))
     (lambda (result)
       (message "Website tangle/publish finished")))))
;; Tangle Publish:1 ends here

;; [[file:init-emacs.org::*+Tangle Publish Asynchronously+][+Tangle Publish Asynchronously+:1]]
;; ;;------------------------------------------------------------------------------
;; ;;;; Org Website: Helper Functions: Tangle Publish Asynchronously
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "Org Website: Helper Functions: Tangle Publish Asynchronously")

;; (defun org-website-tangle-publish-async (&optional project force)
;;   "Tangle and publish org-website projects asynchronously.
;; \nIf PROJECT is non-nil, only tangle/publish that project.
;; If FORCE is non-nil, force publish all files in project."
;;   (interactive)
;;   (message (concat "~/web/bin/website-tangle-publish"
;;                    (if project (concat " --site " project) "")
;;                    (if force " --force" "")))
;;   ;; (let ((args (cl-remove-if #'null
;;   ;;                           (append
;;   ;;                            (when project (append '("--site") (list project)))
;;   ;;                            (when force '("--force"))))))
;;   (eval
;;    `(async-spinner
;;      (lambda ()
;;        (shell-command
;;         (concat "~/web/bin/website-tangle-publish"
;;                 (if ,project (concat " --site " ,project) "")
;;                 (if ,force " --force" ""))))
;;      (lambda (result)
;;        (message "Website tangle/publish finished")))))

;; ;; (eval `(start-process
;; ;;         "org-website-tangle-publish-async-process-name"
;; ;;         "*org-website-tangle-publish-async*"
;; ;;         "~/web/bin/website-tangle-publish"
;; ;;         ,@args)))))
;; +Tangle Publish Asynchronously+:1 ends here

;; [[file:init-emacs.org::*Blog Post Create][Blog Post Create:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Helper Functions: Blog Post Create
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Helper Functions: Blog Post Create")

(defun org-website-blog-post-create (&optional title)
  "Create empty blog post entry with TITLE.

If TITLE is nil, caller is prompted for one."
  (interactive "sTitle: ")
  (save-match-data
    (setq title (titleize title))
    (find-file "~/web/org/nulldot.org")
    (goto-char (point-min))
    (re-search-forward "^\*\*\* Blog Posts$")
    (org-forward-heading-same-level 1 t)
    (forward-line -1)
    (newline)
    (let* ((ts (current-time))
           (time (format-time-string "%Y.%m.%d.%H%M" ts))
           (name (concat time "-" (org-generate-custom-id-from-title title)))
           (path (format-time-string "%Y/%m" ts))
           (file (concat "~/web/sites/nulldot/site/" path "/" name ".org"))
           (posted (format-time-string "%Y-%m-%d %H:%M" ts)))
      (insert
       (concat
        "******* " time " " title "\n"
        "        :PROPERTIES:\n"
        "        :CUSTOM_ID: site-blog-posts-" time "\n"
        "        :END:\n"
        "\n"
        "#+NAME: blog-" time "\n"
        "#+BEGIN_SRC org :tangle " file "\n"
        "  <<level-2>>\n"
        "  ,#+TITLE: " title "\n"
        "  ,#+DESCRIPTION: \n"
        "  ,#+POSTED: " posted "\n"
        "  ,#+LAST_MODIFIED: " posted "\n"
        "  ,#+UUID: " (uuid) "\n"
        "  ,#+TAGS: \n"
        "\n"
        "  ,#+ATTR_HTML: :class blog-img :title " title "\n"
        "  [[][<<site-url>>/" path "/img/]]\n"
        "\n"
        "\n"
        "\n"
        "  ,#+BEGIN_QUOTE\n"
        "  ,#+END_QUOTE\n"
        "#+END_SRC\n"))
      (forward-line -14)
      (goto-char (line-end-position)))))
;; Blog Post Create:1 ends here

;; [[file:init-emacs.org::*Blog Post Update Posted][Blog Post Update Posted:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Helper Functions: Blog Post Update Posted
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Helper Functions: Blog Post Update Posted")

(defun org-website-blog-post-update-posted (&optional date)
  "Update posted time of current blog post entry.

Set blog timestamp to `current-time' or DATE, if non-nil."
  (interactive)
  (when (string= (expand-file-name "~/web/org/nulldot.org") buffer-file-name)
    (save-mark-and-excursion
      (save-match-data
        (let* ((case-fold-search t)
               (start (progn (org-previous-visible-heading 1) (point)))
               (end (progn (org-next-visible-heading 1) (point)))
               (title (progn (goto-char start) (re-search-forward "#\\+TITLE: \\(.*\\)" end) (match-string 1)))
               (ts (if date (date-to-time date) (current-time)))
               (time (format-time-string "%Y.%m.%d.%H%M" ts))
               (name (concat time "-" (org-generate-custom-id-from-title title)))
               (path (format-time-string "%Y/%m" ts))
               (file (concat "~/web/sites/nulldot/site/" path "/" name ".org"))
               (posted (format-time-string "%Y-%m-%d %H:%M" ts)))
          (goto-char start)
          ;; heading
          (re-search-forward "^\*+ \\(.*\\)" (line-end-position))
          (replace-match (concat time " " title) nil nil nil 1)
          ;; custom_id
          (re-search-forward "^[ \t]*:CUSTOM_ID: site-blog-posts-\\(.*\\)" end)
          (replace-match time nil nil nil 1)
          ;; name
          (re-search-forward "^[ \t]*#\\+NAME: blog-\\(.*\\)" end)
          (replace-match time nil nil nil 1)
          ;; file
          (re-search-forward "^[ \t]*#\\+BEGIN_SRC org :tangle \\(.*\\)" end)
          (let ((old-file (match-string 1)))
            (when (file-exists-p old-file)
              (delete-file old-file)))
          (replace-match file nil nil nil 1)
          ;; posted
          (re-search-forward "^[ \t]*#\\+POSTED: \\(.*\\)" end)
          (replace-match posted nil nil nil 1)
          ;; last_modified
          (re-search-forward "^[ \t]*#\\+LAST_MODIFIED: \\(.*\\)" end)
          (replace-match posted nil nil nil 1))))))
;; Blog Post Update Posted:1 ends here

;; [[file:init-emacs.org::*Unflatten][Unflatten:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Helper Functions: Unflatten
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Helper Functions: Unflatten")

(defun unflatten (xs &optional fn-value fn-level)
  "Unflatten a list XS into a tree, e.g. (1 2 3 1) => (1 (2 (3)) 1).
FN-VALUE specifies how to extract the values from each element,
which are included in the output tree, FN-LEVEL tells how to
extract the level of each element. By default these are the
`identity' function so it will work on a list of numbers."
  (let* ((level 1)
         (tree (cons nil nil))
         (start tree)
         (stack nil)
         (fn-value (or fn-value #'identity))
         (fn-level (or fn-level #'identity)))
    (dolist (x xs)
      (let ((x-value (funcall fn-value x))
            (x-level (funcall fn-level x)))
        (cond
         ((> x-level level)
          (setcdr tree (cons (cons x-value nil) nil))
          (setq tree (cdr tree))
          (push tree stack)
          (setq tree (car tree))
          (setq level x-level))
         ((= x-level level)
          (setcdr tree (cons x-value nil))
          (setq tree (cdr tree)))
         ((< x-level level)
          (while (< x-level level)
            (setq tree (pop stack))
            (setq level (- level 1)))
          (setcdr tree (cons x-value nil))
          (setq tree (cdr tree))
          (setq level x-level)))))
    (cdr start)))
;; Unflatten:1 ends here

;; [[file:init-emacs.org::*Generate Website Emacs Initialization File][Generate Website Emacs Initialization File:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Generate Website Emacs Initialization File
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Generate Website Emacs Initialization File")

(defun org-website-generate-website-emacs-initialization-file ()
  "Generate minimal version of init-emacs.el called
init-emacs-website.el to be used with batch commands."
  (save-mark-and-excursion
    (save-match-data
      (let ((source-file (expand-file-name "init-emacs.el" emacs-home-dir))
            (target-file (expand-file-name "init-emacs-website.el" "~/web/bin"))
            (sections (append '(;;";;; Package Manager: Quelpa"
                                ";;; Package Manager: Straight"
                                ";; set emacs home directory"
                                ";; do not make backup files"
                                ";;;; Org Mode: Functions: org-get-file-data"
                                ";;;; Org Mode: Babel: Setup"
                                ";;;; Org Mode: Babel: Tangle Update Timestamps"
                                ";;;; Org Mode: Babel: Tangle Case-Sensitive"
                                ";;;; Functions: Emacs Functions: delete-line"
                                ";;; Modules: htmlize"
                                ";;; Modules: w3m")
                              (let ((start (progn
                                             (goto-char (point-min))
                                             (re-search-forward "^[ \t]*:CUSTOM_ID: org-website$")))
                                    (end (re-search-forward "^[ \t]*:CUSTOM_ID: org-website-generate-website-emacs-initialization-file$"))
                                    list)
                                (goto-char start)
                                (while (re-search-forward "^[ \t]*\\(;;;; .*\\)$" end :noerror)
                                  (push (match-string-no-properties 1) list))
                                (nreverse list))))
            (prefix (concat "(require 'cl-macs)\n"
                            "(require 'subr-x)\n"
                            "(require 'org)\n"
                            "(require 'ox)\n")))
        (org-copy-tangled-sections source-file target-file sections prefix)))))

(defun after-save-hook--generate-init-emacs-website-elisp-file ()
  "Hook to generate init-emacs-website.el file on save."
  (when (and buffer-file-name
             (string= (file-truename buffer-file-name) init-emacs-true-file-name))
    (org-website-generate-website-emacs-initialization-file)))
(add-hook 'after-save-hook #'after-save-hook--generate-init-emacs-website-elisp-file :append)
;; Generate Website Emacs Initialization File:1 ends here

;; [[file:init-emacs.org::*Remote Synchronization][Remote Synchronization:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Remote Synchronization
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Remote Synchronization")
;; Remote Synchronization:1 ends here

;; [[file:init-emacs.org::*Rsync to Morpheus][Rsync to Morpheus:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Remote Synchronization: Rsync to Morpheus
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Remote Synchronization: Rsync to Morpheus")

(defun org-website-rsync-to-morpheus (&optional property-list)
  "Synchronize published site with morpheus server."
  (interactive)
  (shell-command (concat "rsync -rlptx --delete --force"
                         " --exclude=\".git*\""
                         " \"${HOME}/public_html/\""
                         " \"morpheus:${HOME}/public_html/\""))
  (shell-command (concat "rsync -rlptx --delete --force"
                         " --exclude=\".git*\""
                         " \"${HOME}/public_gopher/\""
                         " \"morpheus:${HOME}/public_gopher/\""))
  (shell-command (concat "rsync -rlptx --delete --force"
                         " --exclude=\".git*\""
                         " \"${HOME}/public_gemini/\""
                         " \"morpheus:${HOME}/public_gemini/\"")))

(defun org-website-rsync-to-morpheus-async (&optional property-list)
  "Asynchronous version of `org-website-rsync-to-morpheus'."
  (interactive)
  (eval
   `(async-spinner
     (lambda ()
       (fset 'org-website-rsync-to-morpheus ,(symbol-function 'org-website-rsync-to-morpheus))
       (org-website-rsync-to-morpheus ,property-list))
     (lambda (result)
       (message "Website rsync to morpheus finished")))))
;; Rsync to Morpheus:1 ends here

;; [[file:init-emacs.org::*Rsync to DigitalOcean][Rsync to DigitalOcean:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Remote Synchronization: Rsync to DigitalOcean
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Remote Synchronization: Rsync to DigitalOcean")

(defun org-website-rsync-to-digitalocean (&optional application force)
  "Synchronize published site with digitalocean server.

Normally, applications are not synced. If APPLICATION is non-nil,
sync it instead. Supported applications:

  \"powerhouse\"
  \"bloodmoon\"

If FORCE is non-nil, do not prompt before synchronizing
applicaitons."
  (interactive)
  (let ((digitalocean "159.203.165.79"))
    (if application
        (when (or force
                  (yes-or-no-p (format "Website rsync %s to DigitalOcean" application)))
          (shell-command
           (concat "rsync -rlptx --delete --force"
                   " --exclude=\".git*\""
                   " --rsh=\"ssh -l kyle\""
                   " \"${HOME}/public_html/sites/nullware/" application "/\""
                   " \"" digitalocean ":/home/kyle/public_html/sites/nullware/" application "/\"")))
      (progn
        (shell-command
         (concat "rsync -rlptx --delete --force"
                 " --exclude=\".git*\""
                 " --exclude=\"nullware/powerhouse/*\""
                 " --exclude=\"nullware/bloodmoon/*\""
                 " --rsh=\"ssh -l kyle\""
                 " \"${HOME}/public_html/sites/\""
                 " \"" digitalocean ":/home/kyle/public_html/sites/\""))
        (shell-command
         (concat "rsync -rlptx --delete --force"
                 " --exclude=\".git*\""
                 " --rsh=\"ssh -l kyle\""
                 " \"${HOME}/public_gopher/\""
                 " \"" digitalocean ":/home/kyle/public_gopher/\""))
        (shell-command
         (concat "rsync -rlptx --delete --force"
                 " --exclude=\".git*\""
                 " --rsh=\"ssh -l kyle\""
                 " \"${HOME}/public_gemini/\""
                 " \"" digitalocean ":/home/kyle/public_gemini/\""))))))

(defun org-website-rsync-to-digitalocean-async (&optional application force)
  "Asynchronous version of `org-website-rsync-to-digitalocean'."
  (interactive)
  (when (or (not application)
            force
            (yes-or-no-p (format "Website rsync %s to DigitalOcean" application)))
    (eval
     `(async-spinner
       (lambda ()
         (fset 'org-website-rsync-to-digitalocean ,(symbol-function 'org-website-rsync-to-digitalocean))
         (org-website-rsync-to-digitalocean ,application ,force))
       (lambda (result)
         (message "Website rsync to DigitalOcean finished"))))))
;; Rsync to DigitalOcean:1 ends here

;; [[file:init-emacs.org::*Deployment][Deployment:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Deployment
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Deployment")
;; Deployment:1 ends here

;; [[file:init-emacs.org::*Functions][Functions:1]]
;;==============================================================================
;;; Functions
;;==============================================================================

(init-message 1 "Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::*Initialization Functions][Initialization Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Initialization Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Initialization Functions")
;; Initialization Functions:1 ends here

;; [[file:init-emacs.org::*require-if-available][require-if-available:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: require-if-available
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: require-if-available")

;; prevent load errors if a required package does not exist
(defun require-if-available (&rest args)
  "Require symbols and load library strings.

Fails quietly if some are not available."
  (let (lib)
    (condition-case nil
        (mapc (lambda (e)
                (setq lib e)
                (cond
                 ((stringp e) (load-library e))
                 ((symbolp e) (require e))))
              args)
      ('file-error
       (progn (message "Could not load extension: %s" lib) nil)))))
;; require-if-available:1 ends here

;; [[file:init-emacs.org::*load-file-if-available][load-file-if-available:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: load-file-if-available
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: load-file-if-available")

(defun load-file-if-available (file)
  "Load emacs lisp file, if it exists.

Fails quietly if file does not exist."
  (when (file-exists-p file)
    (load-file file)))
;; load-file-if-available:1 ends here

;; [[file:init-emacs.org::*compile-file-if-needed][compile-file-if-needed:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: compile-file-if-needed
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: compile-file-if-needed")

(defun compile-file-if-needed-process-file (file)
  "Byte-compile expanded FILE name."
  (let* ((file (expand-file-name file))
         (file-comp (concat file "c")))
    (when (or
           (not (file-exists-p file-comp))
           (file-newer-than-file-p file file-comp))
      (byte-compile-file file))))

(defun compile-file-if-needed (file)
  "Byte-compile emacs lisp FILE if needed."
  ;; add .el if needed
  (unless (and (> (length file) 3)
               (string= (substring file -3) ".el"))
    (setq file (concat file ".el")))
  ;; find file
  (if (file-exists-p file)
      (compile-file-if-needed-process-file file)
    (dolist (path load-path)
      (let ((file (concat path "/" file)))
        (when (file-exists-p file)
          (compile-file-if-needed-process-file file)))))
  (when (get-buffer "*Compile-Log*")
    ;;(kill-buffer "*Compile-Log*")
    (delete-other-windows)))
;; compile-file-if-needed:1 ends here

;; [[file:init-emacs.org::*with-eval-after-load][with-eval-after-load:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: with-eval-after-load
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: with-eval-after-load")

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1))
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))
;; with-eval-after-load:1 ends here

;; [[file:init-emacs.org::*eval-after-load-with-byte-compile][eval-after-load-with-byte-compile:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: eval-after-load-with-byte-compile
;;
;; Byte compile the body. If the feature is not available, ignore warnings.
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: eval-after-load-with-byte-compile")

;; byte compiled `eval-after-load'
(defmacro eval-after-load-with-byte-compile (file &rest body)
  "After FILE is loaded, evaluate BODY.

BODY is byte compiled.

FILE may be a named feature or a file name, see `eval-after-load'
for details."
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp file)
                 (require file nil :no-error)
               (load file :no-message :no-error)))
         'progn
       (message "eval-after-load-with-byte-compile: cannot find %s" file)
       'with-no-warnings)
    (with-eval-after-load ',file ,@body)))
;; eval-after-load-with-byte-compile:1 ends here

;; [[file:init-emacs.org::*safe-load][safe-load:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: safe-load
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: safe-load")

(defvar safe-load-error-list ""
  "List of files that reported errors when loaded with `safe-load'.")

(defun safe-load (file &optional noerror nomessage nosuffix)
  "Load FILE safely.

If an error occurs when loading, report it and add FILE to
`safe-load-error-list'."
  (interactive "f")
  (condition-case nil
      (load file noerror nomessage nosuffix)
    ('error
     (setq safe-load-error-list (concat safe-load-error-list " " file))
     (message "Error loading %s" file)
     ;;(sleep-for 1)
     nil)))

(defun safe-load-check ()
  "Check for any previous `safe-load' loading errors."
  (interactive)
  (unless (string= safe-load-error-list "")
    (message "Error loading: %s" safe-load-error-list)))

(defun safe-load-compile (file &optional noerror nomessage nosuffix)
  "Calls `compile-file-if-needed' followed by `safe-load'."
  (interactive "f")
  (compile-file-if-needed file)
  (safe-load file noerror nomessage nosuffix))
;; safe-load:1 ends here

;; [[file:init-emacs.org::*save-mark-and-excursion][save-mark-and-excursion:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Initialization Functions: save-mark-and-excursion
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Initialization Functions: save-mark-and-excursion")

(unless (fboundp 'save-mark-and-excursion)

  (defun save-mark-and-excursion--save ()
    (cons
     (let ((mark (mark-marker)))
       (and mark (marker-position mark) (copy-marker mark)))
     mark-active))

  (defun save-mark-and-excursion--restore (saved-mark-info)
    (let ((saved-mark (car saved-mark-info))
          (omark (marker-position (mark-marker)))
          (nmark nil)
          (saved-mark-active (cdr saved-mark-info)))
      ;; mark marker
      (if (null saved-mark)
          (set-marker (mark-marker) nil)
        (setf nmark (marker-position saved-mark))
        (set-marker (mark-marker) nmark)
        (set-marker saved-mark nil))
      ;; mark active
      (let ((cur-mark-active mark-active))
        (setf mark-active saved-mark-active)
        ;; if mark is active now, and either was not active or was at a
        ;; different place, run the activate hook
        (if saved-mark-active
            (unless (eq omark nmark)
              (run-hooks 'activate-mark-hook))
          ;; if mark has ceased to be active, run deactivate hook
          (when cur-mark-active
            (run-hooks 'deactivate-mark-hook))))))

  (defmacro save-mark-and-excursion (&rest body)
    "Like `save-excursion', but also save and restore the mark state.

This macro does what `save-excursion' did before Emacs 25.1."
    (declare (indent 0))
    (let ((saved-marker-sym (make-symbol "saved-marker")))
      `(let ((,saved-marker-sym (save-mark-and-excursion--save)))
         (unwind-protect
             (save-mark-and-excursion ,@body)
           (save-mark-and-excursion--restore ,saved-marker-sym))))))
;; save-mark-and-excursion:1 ends here

;; [[file:init-emacs.org::*Advice Functions][Advice Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Advice Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Advice Functions")
;; Advice Functions:1 ends here

;; [[file:init-emacs.org::*Compile Goto Error Org][Compile Goto Error Org:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Advice Functions: Compile Goto Error Org
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Advice Functions: Compile Goto Error Org")

(defun compile-goto-error--org (&optional event)
  "Open compilation bugs in org file for errors in tangled elisp code."
  (when (eq major-mode 'emacs-lisp-mode)
    (ignore-errors (org-babel-tangle-jump-to-org))))

(defun compilation-mode-hook--compile-goto-error ()
  "Hook to advise `compile-goto-error'."
  (advice-add 'compile-goto-error :after #'compile-goto-error--org))
(add-hook 'compilation-mode-hook #'compilation-mode-hook--compile-goto-error)
;; Compile Goto Error Org:1 ends here

;; [[file:init-emacs.org::*General Functions][General Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: General Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: General Functions")
;; General Functions:1 ends here

;; [[file:init-emacs.org::*list-to-string][list-to-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: list-to-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: list-to-string")

(defun list-to-string (list &optional delimiter)
  "Return concatenated characters in LIST using optional DELIMITER."
  (let ((delimiter (or delimiter "")))
    (mapconcat 'string list delimiter)))
;; list-to-string:1 ends here

;; [[file:init-emacs.org::*string-to-list][string-to-list:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: string-to-list
;;------------------------------------------------------------------------------
;;
;; There are multiple ways to convert a string to a list.
;;
;; You can use `split-string' to return a list of strings:
;;
;;   (split-string STRING "" t)
;;
;; You can use loop to return a list of characters:
;;
;;   (cl-loop for x across STRING collect x)
;;
;; You can also use append to return a list of characters:
;;
;;   (append STRING nil)

(init-message 3 "Functions: General Functions: string-to-list")

(defun string-to-list (string)
  "Return list of characters in STRING."
  (cl-loop for x across string collect x))
;; string-to-list:1 ends here

;; [[file:init-emacs.org::*join-strings][join-strings:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: join-strings
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: join-strings")

(defun join-strings (list &optional delim)
  "Convert LIST of strings into a single string.

Use optional DELIM as a delimiter."
  (if delim
      (cl-reduce (lambda (x y) (concat x delim y)) list)
    (cl-reduce (lambda (x y) (concat x y)) list)))
;; join-strings:1 ends here

;; [[file:init-emacs.org::*file-to-string][file-to-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: file-to-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: file-to-string")

(defun file-to-string (file)
  "Return the contents of FILE as a string."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    nil))
;; file-to-string:1 ends here

;; [[file:init-emacs.org::*safe-substring][safe-substring:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: safe-substring
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: safe-substring")

(defun safe-substring (string from &optional to)
  "Calls the `substring' function safely.

No errors will be returned for out of range values of FROM and
TO. Instead an empty string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        ""
      (substring string from to))))
;; safe-substring:1 ends here

;; [[file:init-emacs.org::*for-each][for-each:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: for-each
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: for-each")

(defun for-each (fn list)
  "Call FN for each element in list LIST."
  (when list
    (funcall fn (car list))
    (for-each fn (cdr list))))
;; for-each:1 ends here

;; [[file:init-emacs.org::*is-single][is-single:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: is-single
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: is-single")

(defun is-single (list)
  "Return true if LIST is a list of one element."
  (and (consp list) (null (cdr list))))
;; is-single:1 ends here

;; [[file:init-emacs.org::*append-element][append-element:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: append-element
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: append-element")

(defun append-element (list elm)
  "Append ELM to end of list LIST."
  (append list (list elm)))
;; append-element:1 ends here

;; [[file:init-emacs.org::*map-integer][map-integer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: map-integer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: map-integer")

(defun map-integer (fn n)
  "Call function FN once for every number from 0 to N-1."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))
;; map-integer:1 ends here

;; [[file:init-emacs.org::*filter][filter:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: filter
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: filter")

(defun filter (fn list)
  "Call function FN for each element in list LIST and return the non-nil results."
  (let (acc)
    (dolist (x list (nreverse acc))
      (let ((val (funcall fn x)))
        (when val (push val acc))))))
;; filter:1 ends here

;; [[file:init-emacs.org::*most][most:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: most
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: most")

(defun most (fn list)
  "Call function FN for each element in LIST and return the highest score.

The function FN must return a number as a score for a given element.
The element with the highest result is returned with its score."
  (if (null list)
      (list nil nil)
    (let* ((wins (car list))
           (max (funcall fn wins)))
      (dolist (x (cdr list))
        (let ((score (funcall fn x)))
          (when (> score max)
            (setq wins x
                  max score))))
      (list wins max))))
;; most:1 ends here

;; [[file:init-emacs.org::*queue][queue:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: queue
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: queue")

(defun make-queue ()
  "Return FIFO (first-in first-out) queue function.

Then call the queue function with one of the following commands:

  'push X = Add X to the back of the queue
  'pop    = Remove element from the front of the queue and return it
  'peek   = Return the element in the front of the queue
  'show   = Display the queue's contents
  'fb     = Display the front and back queues (for debugging)

Source: http://irreal.org/blog/?p=40"
  (lexical-let (front
                back)
    (lambda (cmd &optional data)
      (cl-labels
          ((exchange ()
                     (setq front (reverse back))
                     (setq back '())))
        (cl-case cmd
          ((push) (push data back))
          ((pop) (or (pop front)
                     (progn
                       (exchange)
                       (pop front))))
          ((peek) (unless front
                    (exchange))
           (car front))
          ((show) (format "%s" (append front (reverse back))))
          ((fb) (format "front: %s, back: %s" front back))
          (t (error "Illegal command given to queue object: %s" cmd)))))))
;; queue:1 ends here

;; [[file:init-emacs.org::*quicksort][quicksort:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: quicksort
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: quicksort")

(defun quicksort (list)
  "Return sorted LIST (using the Quicksort algorithm)."
  (if (null list) nil
    (let* ((elt (car list))
           (rst (cdr list))
           (left-p (lambda (x) (< x elt))))
      (append (quicksort (cl-remove-if-not left-p rst))
              (list elt)
              (quicksort (cl-remove-if left-p rst))))))
;; quicksort:1 ends here

;; [[file:init-emacs.org::*hash-table-dump][hash-table-dump:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: hash-table-dump
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: hash-table-dump")

(defun hash-table-dump (table)
  "Return contents of hash TABLE as an association list."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) table)
    (nreverse result)))
;; hash-table-dump:1 ends here

;; [[file:init-emacs.org::*Emacs Functions][Emacs Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Emacs Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Emacs Functions")
;; Emacs Functions:1 ends here

;; [[file:init-emacs.org::*inside-string][inside-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: inside-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: inside-string")

(defun inside-string ()
  "Return non-nil if point is inside a string."
  (not (not (nth 3 (syntax-ppss)))))
;; inside-string:1 ends here

;; [[file:init-emacs.org::*inside-comment][inside-comment:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: inside-comment
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: inside-comment")

(defun inside-comment ()
  "Return non-nil if point is inside a comment."
  (nth 4 (syntax-ppss)))
;; inside-comment:1 ends here

;; [[file:init-emacs.org::*try-finally][try-finally:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: try-finally
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: try-finally")

;; try/finally code block
(defmacro try-finally (fn &rest finally)
  "Evaluate FN catching and returning any errors after FINALLY is
evaluated."
  `(unwind-protect
       (let (result)
         (condition-case err
             (setq result (progn ,fn))
           ('error
            (message (format "Caught error: %s" err))
            (setq result (cons 'error (list err)))))
         result)
     ,@finally))
;; try-finally:1 ends here

;; [[file:init-emacs.org::*save-buffer-always][save-buffer-always:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: save-buffer-always
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: save-buffer-always")

(defun save-buffer-always ()
  "Save current buffer in visited file even if it has not been modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
;; save-buffer-always:1 ends here

;; [[file:init-emacs.org::*save-buffer-always-maybe][save-buffer-always-maybe:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: save-buffer-always-maybe
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: save-buffer-always-maybe")

;; allow prefix to determine if regular save or forced save is used
(defun save-buffer-always-maybe (arg)
  "Call `save-buffer' if no prefix given, otherwise call
`save-buffer-always' to force a save."
  (interactive "P")
  (if arg
      (save-buffer-always)
    (save-buffer)))
;; save-buffer-always-maybe:1 ends here

;; [[file:init-emacs.org::*describe-function-or-variable-at-point][describe-function-or-variable-at-point:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: describe-function-or-variable-at-point
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: describe-function-or-variable-at-point")

(defun describe-function-or-variable-at-point (&optional point)
  "Describe function or variable at POINT (or `point' if not given).

Use `describe-function' or `describe-variable' as appropriate."
  (interactive)
  (let ((pos (or point (point))))
    (save-mark-and-excursion
      (goto-char pos)
      (if (eq (variable-at-point) 0)
          (call-interactively 'describe-function)
        (call-interactively 'describe-variable)))))
;; describe-function-or-variable-at-point:1 ends here

;; [[file:init-emacs.org::*mode-line-add][mode-line-add:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: mode-line-add
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: mode-line-add")

(defun mode-line-add (item)
  "Add ITEM to `global-mode-string' part of the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (add-to-list 'global-mode-string item t))
;; mode-line-add:1 ends here

;; [[file:init-emacs.org::*insert-line-below][insert-line-below:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: insert-line-below
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: insert-line-below")

(defun insert-line-below ()
  "Insert a line below current one."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
;; insert-line-below:1 ends here

;; [[file:init-emacs.org::*insert-line-above][insert-line-above:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: insert-line-above
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: insert-line-above")

(defun insert-line-above ()
  "Insert a line above current one."
  (interactive)
  (forward-line 0)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
;; insert-line-above:1 ends here

;; [[file:init-emacs.org::*move-line-down][move-line-down:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: move-line-down
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: move-line-down")

(defun move-line-down ()
  (interactive)
  (let ((col (current-column))
        (eol (line-end-position)))
    (forward-line 1)
    (unless (> (point) eol)
      (goto-char eol)
      (newline))
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))
;; move-line-down:1 ends here

;; [[file:init-emacs.org::*move-line-up][move-line-up:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: move-line-up
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: move-line-up")

(defun move-line-up ()
  (interactive)
  (let ((col (current-column))
        (eol (line-end-position)))
    (forward-line 1)
    (unless (> (point) eol)
      (goto-char eol)
      (newline))
    (ignore-errors (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))
;; move-line-up:1 ends here

;; [[file:init-emacs.org::*kill-word-enhanced][kill-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: kill-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: kill-word-enhanced")

(defun kill-word-enhanced (arg)
  "Kill word at point."
  (interactive "*P")
  (save-mark-and-excursion
    (let ((p (point)))
      (forward-word 1)
      (when (> (point) p)
        (forward-word -1)))
    (kill-word (or arg 1))
    (when (eq (char-after) ? )
      (delete-char 1))))
;; kill-word-enhanced:1 ends here

;; [[file:init-emacs.org::*kill-region-or-word][kill-region-or-word:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: kill-region-or-word
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: kill-region-or-word")

(defun kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word' depending on
whether or not a region is selected."
  (interactive "*")
  (if (use-region-p)
      (kill-region (point) (mark))
    (backward-kill-word 1)))
;; kill-region-or-word:1 ends here

;; [[file:init-emacs.org::*kill-duplicate-lines][kill-duplicate-lines:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: kill-duplicate-lines
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: kill-duplicate-lines")

(defun kill-duplicate-lines (&optional beg end)
  "Kill duplicate lines in the pre-sorted selected region or entire buffer (if none)."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (save-mark-and-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (kill-line 1)
          (yank)
          (let ((next-line (point)))
            (while (re-search-forward (format "^%s" (regexp-quote (car kill-ring))) nil :noerror)
              (replace-match "" nil nil))
            (goto-char next-line)))))))
;; kill-duplicate-lines:1 ends here

;; [[file:init-emacs.org::*indent-or-expand][indent-or-expand:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: indent-or-expand
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: indent-or-expand")

(defun indent-or-expand ()
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*")
  (if (and
       (not (bobp))
       (not (eobp))
       (= ?w (char-syntax (char-before)))
       (not (= ?w (char-syntax (char-after)))))
      (dabbrev-expand nil)
    (indent-according-to-mode)))
;; indent-or-expand:1 ends here

;; [[file:init-emacs.org::*swap-windows][swap-windows:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: swap-windows
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: swap-windows")

(defun swap-windows ()
  "If you have two windows, swap them."
  (interactive)
  (if (not (= (count-windows) 2))
      (message "You need exactly two windows to swap them.")
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1))))
;; swap-windows:1 ends here

;; [[file:init-emacs.org::*toggle-window-split][toggle-window-split:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: toggle-window-split
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: toggle-window-split")

(defun toggle-window-split ()
  "If you have two windows, toggle them between horizontal and vertical layouts."
  (interactive)
  (if (not (= (count-windows) 2))
      (message "You need exactly two windows to toggle them.")
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2))
           (e1 (window-edges w1))
           (e2 (window-edges w2))
           (win-2nd (not (and (<= (car e1) (car e2))
                              (<= (cadr e1) (cadr e2)))))
           (splitter
            (if (= (car e1) (car e2))
                'split-window-horizontally
              'split-window-vertically)))
      (delete-other-windows)
      (funcall splitter)
      (when win-2nd (other-window 1))
      (set-window-buffer (selected-window) b1)
      (set-window-buffer (next-window) b2)
      (select-window w1)
      (when win-2nd (other-window 1)))))
;; toggle-window-split:1 ends here

;; [[file:init-emacs.org::*enlarge-window-5][enlarge-window-5:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: enlarge-window-5
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: enlarge-window-5")

(defun enlarge-window-5 (arg)
  "Make current window 5 lines bigger."
  (interactive "P")
  (if arg
      (enlarge-window (* 5 arg))
    (enlarge-window 5)))
;; enlarge-window-5:1 ends here

;; [[file:init-emacs.org::*shrink-window-5][shrink-window-5:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: shrink-window-5
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: shrink-window-5")

(defun shrink-window-5 (arg)
  "Make current window 5 lines smaller."
  (interactive "P")
  (if arg
      (enlarge-window (* -5 arg))
    (enlarge-window -5)))
;; shrink-window-5:1 ends here

;; [[file:init-emacs.org::*compile-elisp][compile-elisp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: compile-elisp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: compile-elisp")

(defun compile-elisp (&optional dir)
  "Byte compile DIR directory.

DIR defaults to `emacs-home-dir' or `~/.emacs.d'."
  (interactive)
  (byte-recompile-directory (or dir emacs-home-dir "~/.emacs.d") 0))
;; compile-elisp:1 ends here

;; [[file:init-emacs.org::*join-next-line][join-next-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: join-next-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: join-next-line")

(defun join-next-line (arg)
  "Join next line with current one."
  (interactive "*P")
  (dotimes (n (or arg 1))
    (join-line -1)))
;; join-next-line:1 ends here

;; [[file:init-emacs.org::*sort-all-lines][sort-all-lines:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: sort-all-lines
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: sort-all-lines")

(defun sort-all-lines (&optional reverse)
  "Sort all lines in current buffer.

If REVERSE is non-nil, then sort in reverse order."
  (interactive "*")
  (save-mark-and-excursion
    (sort-lines reverse (point-min) (point-max))))
;; sort-all-lines:1 ends here

;; [[file:init-emacs.org::*sort-lines-removing-duplicates][sort-lines-removing-duplicates:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: sort-lines-removing-duplicates
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: sort-lines-removing-duplicates")

(defun sort-lines-removing-duplicates (&optional reverse beg end)
  "Call `sort-lines' to sort lines in region or between BEG and END,
then remove all duplicate lines.

If REVERSE is non-nil, then sort in reverse order."
  (interactive "*P\nr")
  (sort-lines reverse beg end)
  (save-mark-and-excursion
    (goto-char end)
    (let ((prev (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (while (and (not (bobp))
                  (>= (point) beg))
        (forward-line -1)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (string= prev line)
            (delete-region (line-beginning-position) (1+ (line-end-position))))
          (setq prev line))))))
;; sort-lines-removing-duplicates:1 ends here

;; [[file:init-emacs.org::*delete-word][delete-word:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: delete-word
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: delete-word")

(defun delete-word (arg)
  "Delete characters forward until encountering the end of the
type of character on point. Types are: whitespace, alphanumeric,
and symbol/other.
With argument ARG, do this that many times."
  (interactive "p")
  (let ((whitespace-regexp "[ \t\n]")
        (alphanumeric-regexp "[[:alnum:]-_]"))
    (dotimes (x arg)
      (delete-region
       (point)
       (save-mark-and-excursion
         (let ((type (cond ((looking-at whitespace-regexp) :whitespace)
                           ((looking-at alphanumeric-regexp) :alphanumeric)
                           (t :other)))
               (char (char-after)))
           (forward-char 1)
           (while (and (not (eobp))
                       (cl-case type
                         (:whitespace (looking-at whitespace-regexp))
                         (:alphanumeric (looking-at alphanumeric-regexp))
                         (:other (= char (char-after)))))
             (forward-char 1))
           (point)))))))
;; delete-word:1 ends here

;; [[file:init-emacs.org::*backward-delete-word][backward-delete-word:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: backward-delete-word
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: backward-delete-word")

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of the
type of character on point. Types are: whitespace, alphanumeric,
and symbol/other.
With argument ARG, do this that many times."
  (interactive "p")
  (let ((whitespace-regexp "[ \t\n]")
        (alphanumeric-regexp "[[:alnum:]-_]"))
    (dotimes (x arg)
      (delete-region
       (save-mark-and-excursion
         (backward-char 1)
         (let ((type (cond ((bobp) :bobp)
                           ((looking-at whitespace-regexp) :whitespace)
                           ((looking-at alphanumeric-regexp) :alphanumeric)
                           (t :other)))
               (char (char-after)))
           (unless (eq type :bobp)
             (backward-char 1)
             (while (cl-case type
                      (:bobp nil)
                      (:whitespace (looking-at whitespace-regexp))
                      (:alphanumeric (looking-at alphanumeric-regexp))
                      (:other (= char (char-after))))
               (if (bobp)
                   (setq type :bobp)
                 (backward-char 1)))
             (unless (eq type :bobp)
               (forward-char 1)))
           (point)))
       (point)))))
;; backward-delete-word:1 ends here

;; [[file:init-emacs.org::*copy-line][copy-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: copy-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: copy-line")

(defun copy-line (&optional line)
  "Copy the line containing the point or LINE."
  (interactive)
  (save-mark-and-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (goto-char (line-beginning-position))
    (let ((beg (point)))
      (if (eobp)
          (goto-char (line-end-position))
        (forward-line 1))
      (copy-region-as-kill beg (point)))))
;; copy-line:1 ends here

;; [[file:init-emacs.org::*cut-line][cut-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: cut-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: cut-line")

(defun cut-line (&optional line)
  "Cut the line containing the point or LINE."
  (interactive "*")
  (save-mark-and-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (goto-char (line-beginning-position))
    (let ((beg (point)))
      (if (eobp)
          (goto-char (line-end-position))
        (forward-line 1))
      (kill-region beg (point)))))
;; cut-line:1 ends here

;; [[file:init-emacs.org::*delete-line][delete-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: delete-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: delete-line")

(defun delete-line (&optional line)
  "Delete the line containing the point or LINE."
  (interactive "*")
  (let ((col (- (point) (line-beginning-position))))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (goto-char (line-beginning-position))
    (delete-region (point)
                   (progn
                     (forward-line 1)
                     (point)))
    (if (<= (+ (point) col) (line-end-position))
        (forward-char col)
      (goto-char (line-end-position)))))
;; delete-line:1 ends here

;; [[file:init-emacs.org::*delete-to-end-of-line][delete-to-end-of-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: delete-to-end-of-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: delete-to-end-of-line")

(defun delete-to-end-of-line (&optional arg)
  "Delete from the point to the end of the line containing the point."
  (interactive "*P")
  (delete-region (point)
                 (progn
                   (if arg
                       (forward-line (prefix-numeric-value arg))
                     (if (not (eolp))
                         (goto-char (line-end-position))
                       (forward-char 1)))
                   (point))))
;; delete-to-end-of-line:1 ends here

;; [[file:init-emacs.org::*duplicate-line][duplicate-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: duplicate-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: duplicate-line")

(defun duplicate-line (&optional comment line)
  "Duplicate the line containing the point.

If COMMENT is non-nil, also comment out the original line.
If LINE is non-nil, duplicate that line instead.

Handles duplicating `org-table' lines correctly. Also fixes
closing s-expressions properly.

Cursor is left at current column in newly created line."
  (interactive "*P")
  (let ((col (current-column)))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (when comment
        (comment-region (line-beginning-position) (line-end-position)))
      (when (and (fboundp 'org-at-table-p) (org-at-table-p))
        (org-table-insert-row)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (goto-char (line-end-position))
      (newline)
      (insert line)
      (move-to-column col)
      (when (or (eq major-mode 'emacs-lisp-mode)
                (eq major-mode 'lisp-mode))
        (save-mark-and-excursion
          (ignore-errors
            (forward-line -1)
            (goto-char (line-beginning-position))
            (forward-sexp)
            (when (looking-at "\\()+\\)[ \t]*;")
              (replace-match (make-string (length (match-string 1)) ?\s) nil nil nil 1))
            (when (looking-at ")+")
              (replace-match ""))
            (forward-line 1)))))))
;; duplicate-line:1 ends here

;; [[file:init-emacs.org::*duplicate-line-inc][duplicate-line-inc:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: duplicate-line-inc
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: duplicate-line-inc")

(defun duplicate-line-inc (&optional line)
  "Duplicate the line containing the point or LINE and increment any numbers by 1."
  (interactive "*")
  (let ((col (current-column)))
    (save-mark-and-excursion
      (when line
        (goto-char (point-min))
        (forward-line (1- line)))
      (copy-region-as-kill (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))
      (if (eobp)
          (newline)
        (forward-line 1))
      (open-line 1)
      (yank))
    (forward-line 1)
    (while (re-search-forward "[0-9]+" (line-end-position) :noerror)
      (let ((num (string-to-number
                  (buffer-substring (match-beginning 0) (match-end 0)))))
        (replace-match (int-to-string (1+ num)))))
    (move-to-column col)))
;; duplicate-line-inc:1 ends here

;; [[file:init-emacs.org::*display-line-numbers-type-toggle][display-line-numbers-type-toggle:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: display-line-numbers-type-toggle
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: display-line-numbers-type-toggle")

(defun display-line-numbers-type-toggle ()
  "Toggle `display-line-numbers' between `t' and `relative' values."
  (interactive)
  (setq display-line-numbers
        (if (eq display-line-numbers 'relative)
            t
          'relative)))
;; display-line-numbers-type-toggle:1 ends here

;; [[file:init-emacs.org::*goto-line-enhanced][goto-line-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: goto-line-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: goto-line-enhanced")

(defun goto-line-enhanced ()
  "Show line numbers while prompting for the line number to go to."
  (interactive)
  (if (or
       display-line-numbers
       current-prefix-arg)
      (call-interactively 'goto-line)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (call-interactively 'goto-line))
      (display-line-numbers-mode -1))))
(bind-keys* ([remap goto-line] . goto-line-enhanced))
;; goto-line-enhanced:1 ends here

;; [[file:init-emacs.org::*forward-sexp-enhanced][forward-sexp-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: forward-sexp-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: forward-sexp-enhanced")

(defun forward-sexp-enhanced (arg)
  "Move point forward one balanced expression (sexp) or ARG sexp's (backward if ARG is negative)."
  (interactive "P")
  (let ((arg (or arg 1))
        (p (point)))
    (ignore-errors
      (forward-sexp arg))
    (when (= p (point))
      (cond
       ((> arg 0)
        (forward-char 1)
        (when (> arg 1)
          (forward-sexp-enhanced (1- arg))))
       ((< arg 0)
        (forward-char -1)
        (when (< arg -1)
          (forward-sexp-enhanced (1+ arg))))
       (t)))))
(bind-keys* ([remap forward-sexp] . forward-sexp-enhanced))
;; forward-sexp-enhanced:1 ends here

;; [[file:init-emacs.org::*backward-sexp-enhanced][backward-sexp-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: backward-sexp-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: backward-sexp-enhanced")

(defun backward-sexp-enhanced (arg)
  "Move point backward one balanced expression (sexp) or ARG sexp's (forward if ARG is negative)."
  (interactive "P")
  (let ((arg (or arg 1)))
    (forward-sexp-enhanced (- 0 arg))))
(bind-keys* ([remap backward-sexp] . backward-sexp-enhanced))
;; backward-sexp-enhanced:1 ends here

;; [[file:init-emacs.org::*scroll-up-enhanced][scroll-up-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: scroll-up-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: scroll-up-enhanced")

(defun scroll-up-enhanced (arg)
  "Scroll up one page or ARG amount.

If less than a page away, jump to the end of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-up arg)
          (scroll-up))
      ('error
       (goto-char (point-max))))
    (move-to-column col)))
(bind-keys* ([remap scroll-up] . scroll-up-enhanced))
;; scroll-up-enhanced:1 ends here

;; [[file:init-emacs.org::*scroll-down-enhanced][scroll-down-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: scroll-down-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: scroll-down-enhanced")

(defun scroll-down-enhanced (arg)
  "Scroll down one page or ARG amount.

If less than a page away, jump to the beginning of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-down arg)
          (scroll-down))
      ('error
       (goto-char (point-min))))
    (move-to-column col)))
(bind-keys* ([remap scroll-down] . scroll-down-enhanced))
;; scroll-down-enhanced:1 ends here

;; [[file:init-emacs.org::*scroll-up-command-enhanced][scroll-up-command-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: scroll-up-command-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: scroll-up-command-enhanced")

(defun scroll-up-command-enhanced (arg)
  "Scroll up one page or ARG amount.

If less than a page away, jump to the end of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-up-command arg)
          (scroll-up-command))
      ('error
       (goto-char (point-max))))
    (move-to-column col)))
(bind-keys* ([remap scroll-up-command] . scroll-up-command-enhanced))
;; scroll-up-command-enhanced:1 ends here

;; [[file:init-emacs.org::*scroll-down-command-enhanced][scroll-down-command-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: scroll-down-command-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: scroll-down-command-enhanced")

(defun scroll-down-command-enhanced (arg)
  "Scroll down one page or ARG amount.

If less than a page away, jump to the beginning of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-down-command arg)
          (scroll-down-command))
      ('error
       (goto-char (point-min))))
    (move-to-column col)))
(bind-keys* ([remap scroll-down-command] . scroll-down-command-enhanced))
;; scroll-down-command-enhanced:1 ends here

;; [[file:init-emacs.org::*downcase-region-enhanced][downcase-region-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: downcase-region-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: downcase-region-enhanced")

(defun downcase-region-enhanced (&optional beg end)
  "Convert region, or current line, to lower case."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (line-beginning-position))))
        (end (or end (if (use-region-p) (region-end) (line-end-position)))))
    (deactivate-mark)
    (downcase-region beg end)))
(bind-keys* ("C-x C-l" . downcase-region-enhanced))
;; downcase-region-enhanced:1 ends here

;; [[file:init-emacs.org::*upcase-region-enhanced][upcase-region-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: upcase-region-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: upcase-region-enhanced")

(defun upcase-region-enhanced (&optional beg end)
  "Convert region, or current line, to upper case."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (line-beginning-position))))
        (end (or end (if (use-region-p) (region-end) (line-end-position)))))
    (deactivate-mark)
    (upcase-region beg end)))
(bind-keys* ("C-x C-u" . upcase-region-enhanced))
;; upcase-region-enhanced:1 ends here

;; [[file:init-emacs.org::*downcase-word-enhanced][downcase-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: downcase-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: downcase-word-enhanced")

(defun downcase-word-enhanced (arg)
  "Convert word at point to lower case."
  (interactive "*P")
  (let ((p (point)))
    (forward-word 1)
    (when (> (point) p)
      (forward-word -1)))
  (downcase-word (or arg 1)))
(bind-keys* ([remap downcase-word] . downcase-word-enhanced))
;; downcase-word-enhanced:1 ends here

;; [[file:init-emacs.org::*upcase-word-enhanced][upcase-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: upcase-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: upcase-word-enhanced")

(defun upcase-word-enhanced (arg)
  "Convert word at point to upper case."
  (interactive "*P")
  (let ((p (point)))
    (forward-word 1)
    (when (> (point) p)
      (forward-word -1)))
  (upcase-word (or arg 1)))
(bind-keys* ([remap upcase-word] . upcase-word-enhanced))
;; upcase-word-enhanced:1 ends here

;; [[file:init-emacs.org::*capitalize-word-enhanced][capitalize-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: capitalize-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: capitalize-word-enhanced")

(defun capitalize-word-enhanced (arg)
  "Capitalize word at point."
  (interactive "*P")
  (let ((syntax-table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?- "." syntax-table)
    (with-syntax-table syntax-table
      (let ((p (point)))
        (forward-word 1)
        (when (> (point) p)
          (forward-word -1)))
      (capitalize-word (or arg 1)))))
(bind-keys* ([remap capitalize-word] . capitalize-word-enhanced))
;; capitalize-word-enhanced:1 ends here

;; [[file:init-emacs.org::*toggle-word-case][toggle-word-case:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: toggle-word-case
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: toggle-word-case")

(defun toggle-word-case (arg)
  "Toggle between the following word states:
  - lowercase
  - capitalized
  - uppercase"
  (interactive "*P")
  (save-mark-and-excursion
    (let ((syntax-table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?- "." syntax-table)
      (with-syntax-table syntax-table
        (let ((p (point))
              (arg (or arg 1)))
          (forward-word 1)
          (let ((w (point)))
            ;; count hyphens as part of the word
            (while (and (char-after)
                        (= (char-after) ?-))
              (forward-word 1))
            (let ((eow (point)))
              (goto-char w)
              (when (> (point) p)
                (forward-word -1))
              (let ((bow (point))
                    (word (buffer-substring-no-properties (point) eow)))
                (goto-char bow)
                (while (and (< (point) eow)
                            (or (= (point) bow)
                                (= (char-after) ?-)))
                  (cond
                   ;; lowercase -> capitalized
                   ((s-lowercase-p word)
                    (capitalize-word arg))
                   ;; uppercase -> lowercase
                   ((s-uppercase-p word)
                    (downcase-word arg))
                   ;; other -> uppercase
                   (t
                    (upcase-word arg))))))))))))
(bind-keys* ("M-c" . toggle-word-case))
;; toggle-word-case:1 ends here

;; [[file:init-emacs.org::*eval-current-sexp][eval-current-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: eval-current-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: eval-current-sexp")

(defun eval-current-sexp (eval-last-sexp-arg-internal)
  "Evaluate current sexp; print value in minibuffer.
Interactively, with prefix argument, print output into current buffer.
Calls `eval-last-sexp' to handle eval."
  (interactive "P")
  (save-mark-and-excursion
    (end-of-defun)
    (eval-last-sexp eval-last-sexp-arg-internal)))
;; eval-current-sexp:1 ends here

;; [[file:init-emacs.org::*eval-sexp-buffer][eval-sexp-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: eval-sexp-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: eval-sexp-buffer")

(defun eval-sexp-buffer (&optional buffer)
  "Evaluate all sexp's in BUFFER.

BUFFER defaults to the current buffer."
  (interactive)
  (save-mark-and-excursion
    (when buffer
      (set-buffer buffer))
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-sexp)
        (eval-last-sexp nil)
        (cl-incf count))
      (message (format "Evaluated %d expressions." count)))))
;; eval-sexp-buffer:1 ends here

;; [[file:init-emacs.org::*eval-and-replace-last-sexp][eval-and-replace-last-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: eval-and-replace-last-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: eval-and-replace-last-sexp")

(defun eval-and-replace-last-sexp ()
  "Replace sexp before point with its evaluation."
  (interactive "*")
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    ('error
     (message "Invalid expression")
     (insert (current-kill 0)))))
;; eval-and-replace-last-sexp:1 ends here

;; [[file:init-emacs.org::*eval-and-replace-current-sexp][eval-and-replace-current-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: eval-and-replace-current-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: eval-and-replace-current-sexp")

(defun eval-and-replace-current-sexp ()
  "Replace current sexp with its evaluation."
  (interactive "*")
  (save-mark-and-excursion
    (end-of-defun)
    (eval-and-replace-last-sexp)))
;; eval-and-replace-current-sexp:1 ends here

;; [[file:init-emacs.org::*macroexpand-and-replace][macroexpand-and-replace:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: macroexpand-and-replace
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: macroexpand-and-replace")

(defun macroexpand-and-replace ()
  "Replace sexp before point with its macroexpand."
  (interactive "*")
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (macroexpand (read (current-kill 0)))
             (current-buffer))
    ('error
     (message "Invalid expression")
     (insert (current-kill 0)))))
;; macroexpand-and-replace:1 ends here

;; [[file:init-emacs.org::*calc-eval-and-replace-region][calc-eval-and-replace-region:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: calc-eval-and-replace-region
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: calc-eval-and-replace-region")

(defun calc-eval-and-replace-region (beg end)
  "Evaluate region using `calc-eval' and replace it with the result."
  (interactive "*r")
  (let ((result (calc-eval (buffer-substring-no-properties beg end))))
    (kill-region beg end)
    (insert result)))
;; calc-eval-and-replace-region:1 ends here

;; [[file:init-emacs.org::*calc-eval-and-replace-line][calc-eval-and-replace-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: calc-eval-and-replace-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: calc-eval-and-replace-line")

(defun calc-eval-and-replace-line ()
  "Evaluate line using `calc-eval' and replace it with the result."
  (interactive "*")
  (calc-eval-and-replace-region (line-beginning-position) (line-end-position)))
;; calc-eval-and-replace-line:1 ends here

;; [[file:init-emacs.org::*indent-current-sexp][indent-current-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: indent-current-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: indent-current-sexp")

(defun indent-current-sexp ()
  "Indent current sexp."
  (interactive "*")
  (save-mark-and-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      ;; indent sexp
      (indent-sexp nil)
      ;; loop through every line checking for eol comments
      (while (< (point) end)
        (goto-char (line-end-position))
        ;; if comment exists, indent it
        (when (eq (get-text-property (point) 'face) 'font-lock-comment-face)
          (comment-indent))
        (forward-line 1)))))
;; indent-current-sexp:1 ends here

;; [[file:init-emacs.org::*indent-sexp-buffer][indent-sexp-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: indent-sexp-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: indent-sexp-buffer")

(defun indent-sexp-buffer (&optional buffer)
  "Indent all sexp's in BUFFER.

BUFFER defaults to the current buffer."
  (interactive "*")
  (save-mark-and-excursion
    (when buffer
      (set-buffer buffer))
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-sexp 1)
        (indent-current-sexp)
        (cl-incf count))
      (message (format "Indented %d expressions." count)))))
;; indent-sexp-buffer:1 ends here

;; [[file:init-emacs.org::*comment-or-uncomment-sexp][comment-or-uncomment-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: comment-or-uncomment-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: comment-or-uncomment-sexp")

(defun uncomment-sexp (&optional n)
  "Uncomment an sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-mark-and-excursion
                (save-match-data
                  (when (elt (syntax-ppss) 4)
                    (search-backward-regexp comment-start-skip
                                            (line-beginning-position)
                                            :noerror))
                  (setq p (point-marker))
                  (comment-forward (point-max))
                  (point-marker))))
         (beg (save-mark-and-excursion
                (save-match-data
                  (forward-line 0)
                  (while (= end (save-mark-and-excursion
                                  (comment-forward (point-max))
                                  (point)))
                    (forward-line -1))
                  (goto-char (line-end-position))
                  (search-backward-regexp comment-start-skip
                                          (line-beginning-position)
                                          :noerror)
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1))
                  (point-marker)))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; identify the top-level sexp inside the comment
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; re-comment everything before it
      (ignore-errors
        (comment-region beg p))
      ;; and everything after it
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; if this is a closing delimiter, pull it up
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; without a prefix, it's more useful to leave point where it was
    (unless n
      (goto-char initial-point))))

;; (defun comment-sexp--raw ()
;;   "Comment the sexp at point or ahead of point."
;;   (pcase (or (bounds-of-thing-at-point 'sexp)
;;              (save-mark-and-excursion
;;                (skip-chars-forward "\r\n[:blank:]")
;;                (bounds-of-thing-at-point 'sexp)))
;;     (`(,l . ,r)
;;      (goto-char r)
;;      (skip-chars-forward "\r\n[:blank:]")
;;      (comment-region l r)
;;      (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-mark-and-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-mark-and-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (pcase (or (bounds-of-thing-at-point 'sexp)
                 (save-mark-and-excursion
                   (skip-chars-forward "\r\n[:blank:]")
                   (bounds-of-thing-at-point 'sexp)))
        (`(,l . ,r)
         (goto-char r)
         (skip-chars-forward "\r\n[:blank:]")
         (comment-region l r)
         (skip-chars-forward "\r\n[:blank:]"))))))
;; comment-or-uncomment-sexp:1 ends here

;; [[file:init-emacs.org::*rename-buffer-and-file][rename-buffer-and-file:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: rename-buffer-and-file
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: rename-buffer-and-file")

(defun rename-buffer-and-file (file-name &optional confirm)
  "Rename current buffer and file to FILE-NAME."
  (interactive
   (list (if buffer-file-name
             (read-file-name "Rename buffer to: " default-directory)
           (error "Current buffer is not visiting a file"))
         (not current-prefix-arg)))
  (or (not file-name) (string-equal file-name "")
      (let ((source-file-name buffer-file-name))
        (unless source-file-name
          (error "Buffer '%s' is not visiting a file" (buffer-name)))
        (write-file file-name confirm)
        (when (file-exists-p file-name)
          (delete-file source-file-name)))))
;; rename-buffer-and-file:1 ends here

;; [[file:init-emacs.org::*move-buffer-and-file][move-buffer-and-file:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: move-buffer-and-file
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: move-buffer-and-file")

(defun move-buffer-and-file (dir &optional confirm)
  "Move current buffer and file to DIR."
  (interactive
   (list (if buffer-file-name
             (read-directory-name "Move to directory: " default-directory
                                  (file-name-directory buffer-file-name))
           (error "Current buffer is not visiting a file"))
         (not current-prefix-arg)))
  (let* ((source-file-name buffer-file-name)
         (dir (if (or (string= dir "")
                      (not (string= (substring dir -1) "/")))
                  (concat dir "/")
                dir))
         (file-name (concat dir (file-name-nondirectory source-file-name))))
    (if (not source-file-name)
        (message "Buffer '%s' is not visiting a file" (buffer-name))
      (progn
        (unless (and confirm
                     (file-exists-p file-name)
                     (not (yes-or-no-p (format "File `%s' exists; overwrite? " file-name)))
                     (message "Canceled"))
          (copy-file source-file-name file-name t)
          (delete-file source-file-name)
          (set-visited-file-name file-name)
          (set-buffer-modified-p nil)
          (vc-refresh-state))))))
;; move-buffer-and-file:1 ends here

;; [[file:init-emacs.org::*delete-buffer-and-file][delete-buffer-and-file:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: delete-buffer-and-file
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: delete-buffer-and-file")

(defun delete-buffer-and-file (&optional buffer)
  "Delete BUFFER and file associated with it.

BUFFER defaults to the current buffer."
  (interactive)
  (let* ((buffer-name (or buffer (current-buffer)))
         (file-name (buffer-file-name buffer-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (if (fboundp 'ido-kill-buffer)
            (ido-kill-buffer)
          (kill-buffer))
      (unless (and
               (not (yes-or-no-p (format "Are you sure you want to delete '%s'? " file-name)))
               (message "Canceled"))
        (delete-file file-name)
        (kill-buffer buffer-name)
        (message "File '%s' successfully deleted" file-name)))))
;; delete-buffer-and-file:1 ends here

;; [[file:init-emacs.org::*expand-relative-file-name][expand-relative-file-name:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: expand-relative-file-name
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: expand-relative-file-name")

(defun expand-relative-file-name (file-name)
  "Expand FILE-NAME found in current directory."
  (file-truename (expand-file-name file-name (file-name-directory (or load-file-name buffer-file-name)))))
;; expand-relative-file-name:1 ends here

;; [[file:init-emacs.org::*remove-trailing-blanks][remove-trailing-blanks:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: remove-trailing-blanks
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: remove-trailing-blanks")

;; mode hooks are added in their sections
(defun remove-trailing-blanks (&optional ask)
  "Remove trailing spaces and tabs from every line in the current buffer.

Also remove trailing newlines from the end of the buffer, apart
from one.

If ASK is non-nil, ask for confirmation."
  (when (and (not (zerop (buffer-size)))
             (char-equal (char-after (buffer-size)) ?\n)
             (save-mark-and-excursion
               (save-restriction
                 (save-match-data
                   (widen)
                   (goto-char (point-min))
                   (or (search-forward " \n" nil :noerror)
                       (search-forward "\t\n" nil :noerror)
                       (re-search-forward "\n\n$" nil :noerror)))))
             (if ask
                 (yes-or-no-p "Remove trailing spaces and newlines before saving? ")
               (message "Removing trailing spaces and newlines...")
               t))
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil 'move)
            (replace-match ""))
          (when (bolp)
            (skip-chars-backward "\n")
            (delete-region (1+ (point)) (point-max)))))))
  nil)

(defun remove-trailing-blanks-ask ()
  (remove-trailing-blanks t))

(defun install-remove-trailing-blanks ()
  (add-hook 'write-contents-functions #'remove-trailing-blanks))

(defun install-remove-trailing-blanks-ask ()
  (add-hook 'write-contents-functions #'remove-trailing-blanks-ask))

;; remove trailing blanks
;;(add-hook 'fundamental-mode-hook #'install-remove-trailing-blanks)

;; remove trailing blanks from all save files
;;(add-hook 'write-contents-functions #'remove-trailing-blanks)
;; remove-trailing-blanks:1 ends here

;; [[file:init-emacs.org::*remove-tabs][remove-tabs:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: remove-tabs
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: remove-tabs")

;; mode hooks are added in their sections
(defun remove-tabs (&optional ask)
  "Remove tabs from every line in the current buffer.

If ASK is non-nil, ask for confirmation.

If buffer is read-only quietly do nothing."
  (when (not buffer-read-only)
    (when (and (not (zerop (buffer-size)))
               (char-equal (char-after (buffer-size)) ?\n)
               (save-mark-and-excursion
                 (save-restriction
                   (save-match-data
                     (widen)
                     (goto-char (point-min))
                     (search-forward "\t" nil :noerror))))
               (if ask
                   (yes-or-no-p "Remove tabs before saving? ")
                 (message "Removing tabs...")
                 t))
      (save-mark-and-excursion
        (save-restriction
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward "[ \t]+$" nil :noerror)
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-min))
            (when (search-forward "\t" nil :noerror)
              (untabify (1- (point)) (point-max))))))))
  nil)

(defcustom remove-tabs-exceptions '((:file . "\.tsv\\'")
                                    (:file . "\.gopher\\'")
                                    (:file . "\\'.gopherus.bookmarks\\'")
                                    (:file . "\\'Makefile\\'")
                                    (:mode . "Makefile"))
  "List of mode name and file name regexp patterns to exclude
from tab removal on file save."
  :type 'list
  :group 'files)

(defun remove-tabs-with-exceptions (&optional ask)
  (let ((file-name (file-name-nondirectory buffer-file-name)))
    (unless
        (cl-remove-if (lambda (x)
                        (let ((type (car x))
                              (name (cdr x)))
                          (and
                           (not (and (eq type :mode)
                                     (stringp mode-name)
                                     (string= mode-name name)))
                           (not (and (eq type :file)
                                     (string-match name file-name))))))
                      remove-tabs-exceptions)
      (remove-tabs ask))))

(defun remove-tabs-ask ()
  (remove-tabs t))

(defun remove-tabs-with-exceptions-ask ()
  (remove-tabs-with-exceptions t))

(defun install-remove-tabs ()
  (add-hook 'write-contents-functions #'remove-tabs-with-exceptions))

(defun install-remove-tabs-ask ()
  (add-hook 'write-contents-functions #'remove-tabs-with-exceptions-ask))

;; remove tabs
;;(add-hook 'fundamental-mode-hook #'install-remove-tabs)

;; remove tabs from all saved files (with exceptions)
(add-hook 'write-contents-functions #'remove-tabs-with-exceptions)
;; remove-tabs:1 ends here

;; [[file:init-emacs.org::*indent-down][indent-down:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: indent-down
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: indent-down")

(defun indent-down ()
  "Indent current line via `lisp-indent-line' then go down one line via `next-line'."
  (interactive "*")
  (lisp-indent-line)
  (forward-line 1))
;; indent-down:1 ends here

;; [[file:init-emacs.org::*server-start-maybe][server-start-maybe:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: server-start-maybe
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: server-start-maybe")

(defun server-start-maybe ()
  "Safe way to start or restart an emacs server."
  (unless (or window-system-windows
              (and (fboundp 'server-runningp)
                   (server-running-p)))
    (server-start :leave-dead)
    (server-start)))
;; server-start-maybe:1 ends here

;; [[file:init-emacs.org::*load-bookmarks][load-bookmarks:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: load-bookmarks
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: load-bookmarks")

(defun load-bookmarks (&optional file)
  "Load bookmarks html FILE.

FILE defaults to `~/lynx_bookmarks.html'."
  (interactive)
  (let ((file (or file "~/lynx_bookmarks.html")))
    ;;(w3m-browse-url (file-truename (expand-file-name file)))))
    (eww-open-in-new-buffer (file-truename (expand-file-name file)))))
;; load-bookmarks:1 ends here

;; [[file:init-emacs.org::*find-file-updir][find-file-updir:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: find-file-updir
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: find-file-updir")

(defun find-file-updir (name &optional directory)
  "Return the absolute file name of NAME if it is found in the
current buffer's default directory or in any parent directory.

If DIRECTORY is non-nil, then it is used instead of the current
buffer's default directory."
  (let ((name (file-truename (expand-file-name name directory))))
    (while (and
            (not (file-exists-p name))
            (not (string= name (concat "/" (file-name-nondirectory name)))))
      (setq name (file-truename (expand-file-name (concat
                                                   (file-name-directory name)
                                                   "../"
                                                   (file-name-nondirectory name))))))
    (when (file-exists-p name) name)))
;; find-file-updir:1 ends here

;; [[file:init-emacs.org::*find-file-eof][find-file-eof:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: find-file-eof
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: find-file-eof")

(defun find-file-eof (file)
  "Run `find-file' with FILE, then move the point to the end of buffer."
  (find-file file)
  (goto-char (point-max)))
;; find-file-eof:1 ends here

;; [[file:init-emacs.org::*mark-full-word][mark-full-word:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: mark-full-word
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: mark-full-word")

(defun mark-full-word (&optional arg allow-extend)
  "Set mark ARG words away from start of word at point.

Point is moved to the beginning of the word at point, then
`mark-word' is called with the given arguments."
  (interactive "P\np")
  (beginning-of-thing 'word)
  (mark-word arg allow-extend))
;; mark-full-word:1 ends here

;; [[file:init-emacs.org::*term-buffer][term-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: term-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: term-buffer")

(defun term-buffer ()
  "Create or visit a persistent terminal buffer."
  (interactive)
  (let ((name "*ansi-term*"))
    (if (not (get-buffer name))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (ansi-term (getenv "SHELL")))
      (switch-to-buffer-other-window name))))
;; term-buffer:1 ends here

;; [[file:init-emacs.org::*term-ansi][term-ansi:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: term-ansi
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: term-ansi")

(when (fboundp 'term-ansi-make-term)
  (defun term-ansi (name cmd &rest switches)
    "Run an application in an ansi-term window."
    (let ((buffer (apply #'term-ansi-make-term
                         (generate-new-buffer-name (concat "*" name "*"))
                         cmd nil switches)))
      (set-buffer buffer)
      (when (fboundp 'term-mode) (term-mode))
      (when (fboundp 'term-char-mode) (term-char-mode))
      (when (fboundp 'term-set-escape-char) (term-set-escape-char ?\C-x))
      (switch-to-buffer buffer))))
;; term-ansi:1 ends here

;; [[file:init-emacs.org::*pop-up-shell][pop-up-shell:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: pop-up-shell
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: pop-up-shell")

(defun pop-up-shell (arg)
  "Pop-up a shell in a side window passing ARG."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'shell))
      (current-buffer))
    '((side . bottom)))))
;; pop-up-shell:1 ends here

;; [[file:init-emacs.org::*pop-up-shell-toggle][pop-up-shell-toggle:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: pop-up-shell-toggle
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: pop-up-shell-toggle")

(defun pop-up-shell-toggle (arg)
  "Toggle visibility of `pop-up-shell'.
ARG is passed along if shell is being toggled on."
  (interactive "P")
  (let ((buffer-window (get-buffer-window "*shell*")))
    (if buffer-window
        (delete-window buffer-window)
      (pop-up-shell arg))))
;; pop-up-shell-toggle:1 ends here

;; [[file:init-emacs.org::*switch-to-scratch][switch-to-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-scratch")

(defun switch-to-scratch ()
  "Switch to `*scratch*' buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))
;; switch-to-scratch:1 ends here

;; [[file:init-emacs.org::*switch-to-scratch-for-current-mode][switch-to-scratch-for-current-mode:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-scratch-for-current-mode
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-scratch-for-current-mode")

(defun switch-to-scratch-for-current-mode ()
  "Switch to `*scratch-MODE*' buffer, creating it if needed."
  (interactive)
  (let* ((mode major-mode)
         (buffer-name (concat "*scratch-" (symbol-name mode) "*")))
    (switch-to-buffer buffer-name)
    (funcall mode)))
;; switch-to-scratch-for-current-mode:1 ends here

;; [[file:init-emacs.org::*new-scratch][new-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: new-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: new-scratch")

(defun new-scratch ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*")))
;; new-scratch:1 ends here

;; [[file:init-emacs.org::*new-emacs-lisp-scratch][new-emacs-lisp-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: new-emacs-lisp-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: new-emacs-lisp-scratch")

(defun new-emacs-lisp-scratch ()
  "Create a new scratch buffer with `emacs-lisp-mode'."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch-emacs-lisp-mode*"))
  (emacs-lisp-mode))
;; new-emacs-lisp-scratch:1 ends here

;; [[file:init-emacs.org::*recreate-scratch-when-killed][recreate-scratch-when-killed:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: recreate-scratch-when-killed
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: recreate-scratch-when-killed")

(defun recreate-scratch-when-killed ()
  "Recreate scratch buffer, when it is killed.

Add the following to your init.el file for this to work:

  \(add-hook 'kill-buffer-query-functions #'recreate-scratch-when-killed)"
  (interactive)
  (let ((buffer-name "*scratch*"))
    (if (string= (buffer-name (current-buffer)) buffer-name)
        (let ((kill-buffer-query-functions kill-buffer-query-functions))
          (remove-hook 'kill-buffer-query-functions 'recreate-scratch-when-killed)
          (kill-buffer (current-buffer))
          (set-buffer (get-buffer-create buffer-name))
          nil)
      t)))

(add-hook 'kill-buffer-query-functions #'recreate-scratch-when-killed)
;; recreate-scratch-when-killed:1 ends here

;; [[file:init-emacs.org::*switch-to-messages][switch-to-messages:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-messages
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-messages")

(defun switch-to-messages ()
  "Switch to `*Messages*' buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*Messages*"))
;; switch-to-messages:1 ends here

;; [[file:init-emacs.org::*diff-current-buffer][diff-current-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: diff-current-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: diff-current-buffer")

(defun diff-current-buffer ()
  "Show a diff of the current buffer with its file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
;; diff-current-buffer:1 ends here

;; [[file:init-emacs.org::*get-char-property-here][get-char-property-here:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: get-char-property-here
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: get-char-property-here")

(defun get-char-property-here ()
  "Get character property at current point."
  (interactive)
  (let (face)
    (setq face (get-char-property (point) 'face))
    (when (called-interactively-p 'any)
      (message "%s" face))
    face))
;; get-char-property-here:1 ends here

;; [[file:init-emacs.org::*comments-in-buffer][comments-in-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: comments-in-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: comments-in-buffer")

(defun comments-in-buffer (&optional beg end)
  "Return a list of all the comments in the current buffer.

Optional START and END parameters will limit the search to a region."
  (interactive)
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        comments)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (comment-search-forward (point-max) t)
            (push (buffer-substring-no-properties (point) (line-end-position)) comments))))
      (nreverse comments))))
;; comments-in-buffer:1 ends here

;; [[file:init-emacs.org::*count-words][count-words:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: count-words
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: count-words")

(defun count-words (&optional beg end)
  "Count the number of words in the selected region or entire buffer (if none)."
  (interactive)
  (let* ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
         (end (or end (if (use-region-p) (region-end) (point-max))))
         (count (how-many "\\w+" beg end)))
    (when (called-interactively-p 'any)
      (message "%s" count))
    count))
;; count-words:1 ends here

;; [[file:init-emacs.org::*count-words-paragraph][count-words-paragraph:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: count-words-paragraph
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: count-words-paragraph")

(defun count-words-paragraph ()
  "Count the number of words in the current paragraph."
  (interactive)
  (save-mark-and-excursion
    (let (end
          (count 0))
      (forward-paragraph 1)
      (setq end (point))
      (backward-paragraph 1)
      (setq count (how-many "\\w+" (point) end))
      (when (called-interactively-p 'any)
        (message "%s" count))
      count)))
;; count-words-paragraph:1 ends here

;; [[file:init-emacs.org::*date-offset][date-offset:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: date-offset
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: date-offset")

(defun date-offset (&optional offset timezone format)
  "Return current date/time plus OFFSET seconds.

OFFSET is the number of seconds to add to the current
time (defaults to 0).

TIMEZONE changes the timezone (defaults to local system setting).

FORMAT is a 'date' format string (defaults to
'+%Y-%m-%dT%H:%M:%SZ')."
  (interactive)
  (let* ((offset (or offset 0))
         (format (or format (setq format "+%Y-%m-%dT%H:%M:%SZ")))
         (date (replace-regexp-in-string
                "^ +\\|[ \n]+$" ""
                (shell-command-to-string
                 (concat
                  (if timezone
                      (concat "TZ=" (shell-quote-argument timezone) " ")
                    "")
                  "date -d \"" (shell-quote-argument (number-to-string offset))
                  " sec\" " (shell-quote-argument format))))))
    (when (called-interactively-p 'any)
      (message "%s" date))
    date))
;; date-offset:1 ends here

;; [[file:init-emacs.org::*ascii-table][ascii-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: ascii-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: ascii-table")

(defun ascii-table ()
  "Print a table of the ASCII characters from 0 to 254 in a buffer."
  (interactive)
  ;; thanks to David Jolley for the special-chars list
  (let ((special-chars ["NUL " "SOH " "STX " "ETX " "EOT "
                        "ENQ " "ACK " "BEL " "BS  " "HT  "
                        "LF  " "VT  " "FF  " "CR  " "SO  "
                        "SI  " "DLE " "DC1 " "DC2 " "DC3 "
                        "DC4 " "NAK " "SYN " "ETB " "CAN "
                        "EM  " "SUB " "ESC " "FS  " "GS  "
                        "RS  " "US  "]))
    (switch-to-buffer "*ASCII Table*")
    (buffer-disable-undo (current-buffer))
    (let (buffer-read-only)
      (erase-buffer)
      (dotimes (y 32)
        (dotimes (x 8)
          (when (and (> y 0) (zerop (mod x 8)))
            (newline))
          (let ((c (+ y (* x 32))))
            (insert (format "%4d " c)
                    (cond
                     ((< c 32)
                      (aref special-chars c))
                     ((= c 127)
                      "DEL ")
                     ((or (< c 127) (> c 159))
                      (format "%-4c" c))
                     (t "    ")))))))
    (setq buffer-read-only t)
    (goto-char (point-min))))

;; simple version
;; (defun ascii-table ()
;;   "Print the ASCII characters from 0 to 254 in a buffer."
;;   (interactive)
;;   (switch-to-buffer "*ASCII Table*")
;;   (buffer-disable-undo (current-buffer))
;;   (erase-buffer)
;;   (dotimes (x 255)
;;     (insert (format "%4d %c\n" x x)))
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))
;; ascii-table:1 ends here

;; [[file:init-emacs.org::*http-status-code-table][http-status-code-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: http-status-code-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: http-status-code-table")

(defun http-status-code-table ()
  "Print a table of the HTTP status codes in a buffer."
  (interactive)
  (let ((status-codes
         '((100 . "Continue")
           (101 . "Switching Protocols")
           (102 . "Processing")
           (103 . "Early Hints")
           (104 . "Unassigned")
           (200 . "OK")
           (201 . "Created")
           (202 . "Accepted")
           (203 . "Non-Authoritative Information")
           (204 . "No Content")
           (205 . "Reset Content")
           (206 . "Partial Content")
           (207 . "Multi-Status")
           (208 . "Already Reported")
           (209 . "Unassigned")
           (226 . "IM Used")
           (227 . "Unassigned")
           (300 . "Multiple Choices")
           (301 . "Moved Permanently")
           (302 . "Found")
           (303 . "See Other")
           (304 . "Not Modified")
           (305 . "Use Proxy")
           (306 . "(Unused)")
           (307 . "Temporary Redirect")
           (308 . "Permanent Redirect")
           (400 . "Bad Request")
           (401 . "Unauthorized")
           (402 . "Payment Required")
           (403 . "Forbidden")
           (404 . "Not Found")
           (405 . "Method Not Allowed")
           (406 . "Not Acceptable")
           (407 . "Proxy Authentication Required")
           (408 . "Request Timeout")
           (409 . "Conflict")
           (410 . "Gone")
           (411 . "Length Required")
           (412 . "Precondition Failed")
           (413 . "Payload Too Large")
           (414 . "URI Too Long")
           (415 . "Unsupported Media Type")
           (416 . "Range Not Satisfiable")
           (417 . "Expectation Failed")
           (421 . "Misdirected Request")
           (422 . "Unprocessable Entity")
           (423 . "Locked")
           (424 . "Failed Dependency")
           (425 . "Too Early")
           (426 . "Upgrade Required")
           (427 . "Unassigned")
           (428 . "Precondition Required")
           (429 . "Too Many Requests")
           (430 . "Unassigned")
           (431 . "Request Header Fields Too Large")
           (451 . "Unavailable For Legal Reasons")
           (500 . "Internal Server Error")
           (501 . "Not Implemented")
           (502 . "Bad Gateway")
           (503 . "Service Unavailable")
           (504 . "Gateway Timeout")
           (505 . "HTTP Version Not Supported")
           (506 . "Variant Also Negotiates")
           (507 . "Insufficient Storage")
           (508 . "Loop Detected")
           (509 . "Unassigned")
           (510 . "Not Extended")
           (511 . "Network Authentication Required"))))
    (switch-to-buffer "*HTTP Status Code Table*")
    (buffer-disable-undo (current-buffer))
    (let (buffer-read-only)
      (erase-buffer)
      (mapc (lambda (c) (insert (format "%5d %s\n" (car c) (cdr c))))
            status-codes))
    (setq buffer-read-only t)
    (goto-char (point-min))))
;; http-status-code-table:1 ends here

;; [[file:init-emacs.org::*powers-of-two-table][powers-of-two-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: powers-of-two-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: powers-of-two-table")

(defun powers-of-two-table ()
  "Print a table of the first 64 Powers of two in a buffer."
  (interactive)
  (switch-to-buffer "*Powers of Two Table*")
  (buffer-disable-undo (current-buffer))
  (let (buffer-read-only)
    (erase-buffer)
    (insert "POWER   DECIMAL                HEX\n")
    (insert "-----   --------------------   -----------------\n")
    (dotimes (x 65)
      (if (<= x  60)
          (let ((pow (expt 2 x)))
            (insert (format "%5d   %20d   %17X\n" x pow pow)))
        (let ((dec (progn (calc-radix 10)
                          (calc-eval (format "2^%d" x))))
              (hex (progn (calc-radix 16)
                          (substring (calc-eval (format "2^%d" x)) 3))))
          (insert (format "%5d   %20s   %17s\n" x dec hex))))))
  (setq buffer-read-only t)
  (goto-char (point-min)))
;; powers-of-two-table:1 ends here

;; [[file:init-emacs.org::*memory-use-counts-pretty][memory-use-counts-pretty:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: memory-use-counts-pretty
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: memory-use-counts-pretty")

(defun memory-use-counts-pretty ()
  "Pretty print version of `memory-use-counts'."
  (interactive)
  (let ((tags '(Conses Floats Vector-Cells Symbols String-Chars Miscs Intervals Strings))
        (muc (memory-use-counts))
        (str ""))
    (dotimes (n (length tags))
      (setq str (concat str (if (zerop (length str)) "" ", ")
                        (symbol-name (nth n tags)) ": " (number-to-string (nth n muc)))))
    str))
;; memory-use-counts-pretty:1 ends here

;; [[file:init-emacs.org::*git-paste-cleanup][git-paste-cleanup:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: git-paste-cleanup
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: git-paste-cleanup")

(defun git-paste-cleanup (beg end)
  "Remove `+' characters from the start of lines in region."
  (interactive "*r")
  (save-mark-and-excursion
    (goto-char beg)
    (when (> (point) (line-beginning-position))
      (forward-line 1)
      (goto-char (line-beginning-position)))
    (while (< (point) end)
      (when (looking-at "^+")
        (delete-char 1))
      (forward-line 1))))
;; git-paste-cleanup:1 ends here

;; [[file:init-emacs.org::*execute-buffer][execute-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: execute-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: execute-buffer")

;; TODO: need to test
(defun execute-buffer ()
  "Execute or compile current file."
  (interactive)
  (let* ((file-name (shell-quote-argument buffer-file-name))
         (file-type (substring (shell-command-to-string (concat "file " file-name)) 0 -1))
         (type-map '(("Lisp" . "clisp")
                     ("bash" . "bash")
                     ("perl" . "perl")
                     ("python" . "python")
                     ("java" . "javac")
                     ("kotlin" . "kotlinc")
                     ("php" . "php")
                     (" c " . "gcc")))
         cmd)
    (delete-other-windows)
    (cl-do ((type type-map (cdr type)))
        ((or (not type) cmd))
      (when (cl-search (car type) file-type)
        (setq cmd (cdr type))))
    (shell-command (concat cmd " " file-name))))
;; execute-buffer:1 ends here

;; [[file:init-emacs.org::*file-in-exec-path][file-in-exec-path:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: file-in-exec-path
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: file-in-exec-path")

(defun file-in-exec-path (name)
  "Return non-nil if NAME is a file found in `exec-path'."
  (catch 'done
    (dolist (dir exec-path)
      (when (file-exists-p (concat (file-name-as-directory dir) name))
        (throw 'done t)))
    nil))
;; file-in-exec-path:1 ends here

;; [[file:init-emacs.org::*unicode-shell][unicode-shell:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: unicode-shell
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: unicode-shell")

(defun unicode-shell ()
  "Execute the shell buffer in UTF-8 encoding.

Note that you need to set the environment variable LANG and
others appropriately."
  (interactive)
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (coding-system-require-warning t))
    (call-interactively 'shell)))
;; unicode-shell:1 ends here

;; [[file:init-emacs.org::*async-spinner][async-spinner:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: async-spinner
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: async-spinner")

(defmacro async-spinner (start-func &optional finish-func)
  "Return `async-start' command for given params with a spinner."
  (require 'async)
  (require 'spinner)
  (let ((spinner (spinner-start 'horizontal-moving))
        (result (gensym)))
    `(async-start
      (lambda ()
        (funcall ,start-func))
      (lambda (,result)
        (funcall ,spinner)
        (funcall ,finish-func ,result)))))
;; async-spinner:1 ends here

;; [[file:init-emacs.org::*with-time][with-time:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: with-time
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: with-time")

(defmacro with-time (&rest body)
  "Return the time it takes (in seconds) to evaluate BODY."
  (declare (indent 0))
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))
;; with-time:1 ends here

;; [[file:init-emacs.org::*package-desc-summary-to-kill-ring][package-desc-summary-to-kill-ring:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: package-desc-summary-to-kill-ring
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: package-desc-summary-to-kill-ring")

(defun package-desc-summary-to-kill-ring ()
  "When `thing-at-point' is a package library, return package
  description summary and put it on the kill ring."
  (interactive)
  (let* ((name (thing-at-point 'symbol t))
         (summary (package-desc-summary
                   (with-temp-buffer
                     (insert-file-contents-literally
                      (find-library-name name))
                     (package-buffer-info)))))
    (kill-new summary)
    (message "%s: %s" name summary)))
;; package-desc-summary-to-kill-ring:1 ends here

;; [[file:init-emacs.org::*toggle-case-fold-search][toggle-case-fold-search:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: toggle-case-fold-search
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: toggle-case-fold-search")

(defun toggle-case-fold-search ()
  "Toggle search case sensitivity."
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "case-fold-search: %s" (if case-fold-search "ON" "OFF")))
;; toggle-case-fold-search:1 ends here

;; [[file:init-emacs.org::*derived-modes][derived-modes:1]]
;;------------------------------------------------------------------------------
;;; Functions: derived-modes
;;------------------------------------------------------------------------------

(init-message 2 "Functions: derived-modes")

(defun derived-modes (&optional mode)
  "Return a list of the ancestor modes that MODE is derived from.

MODE defaults to `major-mode'."
  (let* ((mode (or mode major-mode))
         (mode-list (list mode)))
    (while (setq mode (get mode 'derived-mode-parent))
      (push mode mode-list))
    (nreverse mode-list)))
;; derived-modes:1 ends here

;; [[file:init-emacs.org::*Emacs Grouped Functions][Emacs Grouped Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Emacs Grouped Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Emacs Grouped Functions")
;; Emacs Grouped Functions:1 ends here

;; [[file:init-emacs.org::*Buffer Kill][Buffer Kill:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Buffer Kill
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Buffer Kill")
;; Buffer Kill:1 ends here

;; [[file:init-emacs.org::*kill-buffer-query-functions-maybe-bury][kill-buffer-query-functions-maybe-bury:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Buffer Kill: kill-buffer-query-functions-maybe-bury
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Emacs Grouped Functions: Buffer Kill: kill-buffer-query-functions-maybe-bury")

;;(defconst bury-buffer-names '("*scratch*" "*Messages*"))
(defconst bury-buffer-names '("*Messages*"))
(defun kill-buffer-query-functions-maybe-bury ()
  "Bury certain buffers instead of killing them.

Return nil if buffer should be buried instead of killed.
Used as a `kill-buffer-query-functions' hook."
  (if (member (buffer-name (current-buffer)) bury-buffer-names)
      (progn
        (kill-region (point-min) (point-max))
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions #'kill-buffer-query-functions-maybe-bury)
;; kill-buffer-query-functions-maybe-bury:1 ends here

;; [[file:init-emacs.org::*kill-other-window-buffer][kill-other-window-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Buffer Kill: kill-other-window-buffer
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Emacs Grouped Functions: Buffer Kill: kill-other-window-buffer")

(defun kill-other-window-buffer (&optional delete-window)
  "Kill buffer in other window, prompting user to select a window
if there are more than two.

When DELETE-WINDOW is non-nil, also delete the window."
  (interactive)
  (let ((orig-window (selected-window)))
    (cond
     ((and (fboundp 'switch-window)
           (> (length (window-list)) 1))
      (switch-window)
      (kill-buffer (current-buffer))
      (when delete-window
        (delete-window))
      (select-window orig-window))
     ((= (length (window-list)) 2)
      (other-window 1)
      (kill-buffer (current-buffer))
      (when delete-window
        (delete-window))
      (select-window orig-window))
     (t
      (message "No other window to kill")))))
;; kill-other-window-buffer:1 ends here

;; [[file:init-emacs.org::*kill-other-window-buffer-and-delete-window][kill-other-window-buffer-and-delete-window:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Buffer Kill: kill-other-window-buffer-and-delete-window
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Emacs Grouped Functions: Buffer Kill: kill-other-window-buffer-and-delete-window")

(defun kill-other-window-buffer-and-delete-window ()
  "Kill buffer in other window, prompting user to select a window
if there are more than two. Then delete that window."
  (interactive)
  (kill-other-window-buffer t))
;; kill-other-window-buffer-and-delete-window:1 ends here

;; [[file:init-emacs.org::*Clipboard][Clipboard:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Clipboard
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Clipboard")

;; Modified versions of the similarly named functions `clipboard-kill-region',
;; `clipboard-kill-ring-save', and `clipboard-yank'. These functions use the
;; Linux command line tool `xsel' (which must be installed) to get the same
;; functionality when running Emacs in command line mode.
;; In Ubuntu/Debian run:
;;   sudo apt-get install xsel

;; only load if not in window mode and xsel command is available on system
(when (and (not window-system)
           (executable-find "xsel"))
  (init-message 4 "xclipboard-kill-region")

  (defun xclipboard-kill-region (beg end)
    "Kill the region, and save it in the X clipboard."
    (interactive "*r")
    (async-shell-command
     (concat "echo "
             (shell-quote-argument (buffer-substring-no-properties beg end))
             " | xsel -i"))
    (kill-region beg end))

  (init-message 4 "xclipboard-kill-ring-save")

  (defun xclipboard-kill-ring-save (beg end)
    "Copy region to kill ring, and save in the X clipboard."
    (interactive "r")
    (async-shell-command
     (concat "echo "
             (shell-quote-argument (buffer-substring-no-properties beg end))
             " | xsel -i"))
    (kill-ring-save beg end))

  (init-message 4 "xclipboard-yank")

  (defun xclipboard-yank ()
    "Insert the clipboard contents, or the last stretch of killed text."
    (interactive "*")
    (insert (shell-command-to-string "xsel -o"))))
;; Clipboard:1 ends here

;; [[file:init-emacs.org::*Occur][Occur:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Occur
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Occur")
;; Occur:1 ends here

;; [[file:init-emacs.org::*occur-inverse][occur-inverse:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Occur: occur-inverse
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Emacs Grouped Functions: Occur: occur-inverse")

(defun occur-inverse (regexp)
  "Show all lines in the current buffer not containing a match for REGEXP.
If a non-match spreads across multiple lines, all those lines are shown.
Otherwise, behaves the same as `occur'."
  (interactive "sRegexp: ")
  (let ((filtered
         (mapconcat
          (lambda (line)
            (if (string-match-p regexp line)
                ""
              line))
          (split-string
           (buffer-substring-no-properties (point-min) (point-max)) "\n")
          "\n")))
    (with-temp-buffer
      (insert filtered)
      (occur ".+"))))
;; occur-inverse:1 ends here

;; [[file:init-emacs.org::*occur-remove][occur-remove:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Occur: occur-remove
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Emacs Grouped Functions: Occur: occur-remove")

(defun occur-remove (regexp)
  "Remove all lines in the *Occur* buffer containing a match for REGEXP."
  (interactive "sRegexp: ")
  (unless (string= (buffer-name) "*Occur*")
    (error "This function can only be run within an *Occur* buffer."))
  (let ((inhibit-read-only t))
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (line-beginning-position))
        (if (re-search-forward regexp (line-end-position) :noerror)
            (delete-line)
          (forward-line 1))))))
;; occur-remove:1 ends here

;; [[file:init-emacs.org::*Text Conversion Functions][Text Conversion Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Text Conversion Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Text Conversion Functions")
;; Text Conversion Functions:1 ends here

;; [[file:init-emacs.org::*set-coding-system][set-coding-system:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: set-coding-system
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: set-coding-system")

(defun set-coding-system (coding-system)
  "Change buffer file coding system to CODING-SYSTEM.

CODING-SYSTEM could be:
  'unix
  'dos
  'mac

Or any coding system returned by `list-coding-systems'."
  (interactive)
  (set-buffer-file-coding-system coding-system :force))
;; set-coding-system:1 ends here

;; [[file:init-emacs.org::*escape-xml][escape-xml:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: escape-xml
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: escape-xml")

(defun escape-xml (string)
  "Escape XML in STRING."
  ;; & => &amp;
  (setq string (replace-regexp-in-string "&" "&amp;" string))
  ;; ' => &apos;
  (setq string (replace-regexp-in-string "'" "&apos;" string))
  ;; ! => &bang;
  ;;(setq string (replace-regexp-in-string "!" "&bang;" string))
  ;; = => &eq;
  ;;(setq string (replace-regexp-in-string "=" "&eq;" string))
  ;; > => &gt;
  (setq string (replace-regexp-in-string ">" "&gt;" string))
  ;; < => &lt;
  (setq string (replace-regexp-in-string "<" "&lt;" string))
  ;; ? => &quest;
  ;;(setq string (replace-regexp-in-string "\?" "&quest;" string))
  ;; " => &quot;
  (setq string (replace-regexp-in-string "\"" "&quot;" string))
  ;; / => &slash;
  ;;(setq string (replace-regexp-in-string "/" "&slash;" string))
  ;; return result
  string)
;; escape-xml:1 ends here

;; [[file:init-emacs.org::*unescape-xml][unescape-xml:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: unescape-xml
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: unescape-xml")

(defun unescape-xml (string)
  "Unescape XML in STRING."
  ;; &apos; => '
  (setq string (replace-regexp-in-string "&apos;" "'" string))
  ;; &bang; => !
  ;;(setq string (replace-regexp-in-string "&bang;" "!" string))
  ;; &eq; => =
  ;;(setq string (replace-regexp-in-string "&eq;" "=" string))
  ;; &gt; => >
  (setq string (replace-regexp-in-string "&gt;" ">" string))
  ;; &lt; => <
  (setq string (replace-regexp-in-string "&lt;" "<" string))
  ;; &quest; => ?
  ;;(setq string (replace-regexp-in-string "&quest;" "\?" string))
  ;; &quot; => "
  (setq string (replace-regexp-in-string "&quot;" "\"" string))
  ;; &slash; => /
  ;;(setq string (replace-regexp-in-string "&slash;" "/" string))
  ;; &amp; => &
  (setq string (replace-regexp-in-string "&amp;" "&" string))
  ;; return result
  string)
;; unescape-xml:1 ends here

;; [[file:init-emacs.org::*titleize][titleize:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: titleize
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: titleize")

;; rules found here: http://titlecapitalization.com/
(defun titleize (string &optional do-not-cap-ends)
  "Capitalize STRING according to titling conventions.

If a word should be capitalized, `capitalize-word' is called,
otherwise `downcase-word' is called.

If DO-NOT-CAP-ENDS is non-nil, the first and last words will not
be automatically capitalized."
  ;; TODO: Do not change words that are mixed case or all caps
  (interactive "*")
  (let ((words-single
         '(;; conjunctions
           "and" "as" "be" "into" "is" "it" "nor" "or" "so" "the" "that" "yet"
           ;; prepositions (single words)
           "a" "abaft" "aboard" "about" "above" "absent" "across" "afore"
           "after" "against" "along" "alongside" "amid" "amidst" "among"
           "amongst" "an" "anenst" "apropos" "apud" "around" "as" "aside"
           "astride" "at" "athwart" "atop" "barring" "before" "behind" "below"
           "beneath" "beside" "besides" "between" "beyond" "but" "by" "circa"
           "concerning" "despite" "down" "during" "except" "excluding"
           "failing" "following" "for" "forenenst" "from" "given" "in"
           "including" "inside" "into" "like" "mid" "midst" "minus" "modulo"
           "near" "next" "notwithstanding" "o'" "of" "off" "on" "onto"
           "opposite" "out" "outside" "over" "pace" "past" "per" "plus" "pro"
           "qua" "regarding" "round" "sans" "save" "since" "than" "through"
           "thru" "throughout" "thruout" "till" "times" "to" "toward" "towards"
           "under" "underneath" "unlike" "until" "unto" "up" "upon" "versus"
           "vs" "via" "vice" "vis-à-vis" "vis-a-vis" "whereas" "with" "within"
           "without" "worth"))
        (words-double
         '(;; prepositions (double words)
           "according to" "ahead of" "apart from" "as for" "as of" "as per"
           "as regards" "aside from" "back to" "because of" "close to"
           "due to" "except for" "far from" "in to" "inside of" "instead of"
           "left of" "near to" "next to" "on to" "out from" "out of"
           "outside of" "owing to" "prior to" "pursuant to" "rather than"
           "regardless of" "right of" "subsequent to" "such as" "thanks to"
           "that of" "up to"))
        (words-triple
         '(;; prepositions (triple words)
           "as far as" "as long as" "as opposed to" "as well as" "as soon as"
           "at the behest of" "by means of" "by virtue of" "for the sake of"
           "in accordance with" "in addition to" "in case of" "in front of"
           "in lieu of" "in order to" "in place of" "in point of"
           "in spite of" "on account of" "on behalf of" "on top of"
           "with regard to" "with respect to" "with a view to")))
    (let ((words-single-regexp (regexp-opt (mapcar #'capitalize words-single) 'words))
          (words-double-regexp (regexp-opt (mapcar #'capitalize words-double) 'words))
          (words-triple-regexp (regexp-opt (mapcar #'capitalize words-triple) 'words))
          (abbreviation-word-regexp "\\b[A-Z][.A-Z]+[^ ]*\\b")
          (first-word-regexp "\\(^[ \t]*\\(\\w+\\)\\|[\.!:&\*()/][ \t]*\\(\\w+\\)\\)")
          (last-word-regexp "\\(\\(\\w+\\)[ \t]*$\\|\\(\\w+\\)[ \t]*[\.!:&\*()\[]\\)"))
      (cl-labels
          ((get-abbrevs (string)
                        (let ((case-fold-search nil)
                              (pos 0)
                              abbrevs)
                          (while (string-match abbreviation-word-regexp string pos)
                            (push (list (match-beginning 0)
                                        (match-end 0)
                                        (match-string 0 string))
                                  abbrevs)
                            (setq pos (match-end 0)))
                          (nreverse abbrevs)))
           (set-abbrevs (string abbrevs)
                        (let ((string string))
                          (dolist (a abbrevs)
                            (setq string
                                  (concat (substring string 0 (car a))
                                          (caddr a)
                                          (substring string (cadr a)))))
                          string))
           (cap (string)
                (replace-regexp-in-string
                 words-single-regexp 'downcase
                 (replace-regexp-in-string
                  words-double-regexp 'downcase
                  (replace-regexp-in-string
                   words-triple-regexp 'downcase
                   (capitalize string) t t) t t) t t)))
        (let ((abbrevs (get-abbrevs string)))
          (if do-not-cap-ends
              (set-abbrevs (cap string) abbrevs)
            (set-abbrevs
             (replace-regexp-in-string
              first-word-regexp 'capitalize
              (replace-regexp-in-string
               last-word-regexp 'capitalize
               (cap string) t t) t t) abbrevs)))))))
;; titleize:1 ends here

;; [[file:init-emacs.org::*titleize-word-enhanced][titleize-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: titleize-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: titleize-word-enhanced")

(defun titleize-word-enhanced (arg)
  "Titleize word at point."
  (interactive "*p")
  (let ((syntax-table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?_ "." syntax-table)
    (modify-syntax-entry ?' "w" syntax-table)
    (with-syntax-table syntax-table
      (dotimes (x (or arg 1))
        (let ((p (point)))
          (forward-word 1)
          (forward-word -1)
          (when (< (point) p)
            (forward-word 1)))
        (let ((bounds (bounds-of-thing-at-point 'word)))
          (when bounds
            (goto-char (car bounds)))
          (when (re-search-forward "\\(\\w+\\)" nil :noerror)
            (replace-match (titleize (match-string 0) t) t)))))))
;; titleize-word-enhanced:1 ends here

;; [[file:init-emacs.org::*titleize-line-or-region][titleize-line-or-region:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: titleize-line-or-region
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: titleize-line-or-region")

(defun titleize-line-or-region (&optional beg end)
  "Capitalize the current line or selected region according to
titling conventions.

If a word should be capitalized, `capitalize-word' is called,
otherwise `downcase-word' is called."
  (interactive "*")
  (let* ((pos (point))
         (beg (or beg (cond
                       ((use-region-p)
                        (region-beginning))
                       ((eq major-mode 'wdired-mode)
                        (point))
                       (t
                        (beginning-of-line-text) (point)))))
         (end (or end (cond
                       ((use-region-p)
                        (region-end))
                       ((eq major-mode 'wdired-mode)
                        (if (re-search-forward "\.[^.]+$" (line-end-position) :noerror)
                            (match-beginning 0)
                          (line-end-position)))
                       (t
                        (line-end-position)))))
         (col (- pos beg)))
    (let ((syntax-table (copy-syntax-table (syntax-table)))
          (str (buffer-substring-no-properties beg end)))
      (modify-syntax-entry ?- "." syntax-table)
      (modify-syntax-entry ?_ "." syntax-table)
      (modify-syntax-entry ?' "w" syntax-table)
      (with-syntax-table syntax-table
        (kill-region beg end)
        (goto-char beg)
        (insert (titleize str))
        (goto-char (+ beg col))))))
;; titleize-line-or-region:1 ends here

;; [[file:init-emacs.org::*unfill-paragraph][unfill-paragraph:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: unfill-paragraph
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: unfill-paragraph")

;; reverse of `fill-paragraph'
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; unfill-paragraph:1 ends here

;; [[file:init-emacs.org::*single-space-punctuation][single-space-punctuation:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: single-space-punctuation
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: single-space-punctuation")

(defun single-space-punctuation (&optional beg end)
  "Single-space sentence ending punctuation in the current
paragraph or selected region."
  (interactive "*")
  (save-mark-and-excursion
    (save-match-data
      (let ((beg (or beg (and (use-region-p) (region-beginning))))
            (end (or end (and (use-region-p) (region-end)))))
        (unless (and beg end)
          (mark-paragraph)
          (setq beg (point)
                end (mark-marker)))
        (goto-char beg)
        (while (and (< (point) end)
                    (re-search-forward "\\([^[:blank:]][.?!]['\"”)]?\\)[[:blank:]]\\([^[:blank:]]\\)" end :noerror))
          (replace-match "\\1 \\2"))))))
;; single-space-punctuation:1 ends here

;; [[file:init-emacs.org::*double-space-punctuation][double-space-punctuation:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: double-space-punctuation
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: double-space-punctuation")

(defun double-space-punctuation (&optional beg end)
  "Double-space sentence ending punctuation in the current
paragraph or selected region."
  (interactive "*")
  (save-mark-and-excursion
    (save-match-data
      (let ((beg (or beg (and (use-region-p) (region-beginning))))
            (end (or end (and (use-region-p) (region-end)))))
        (unless (and beg end)
          (mark-paragraph)
          (setq beg (point)
                end (mark-marker)))
        (goto-char beg)
        (while (and (< (point) end)
                    (re-search-forward "\\([^[:blank:]][.?!]['\"”)]?\\)[[:blank:]]\\([^[:blank:]]\\)" end :noerror))
          (replace-match "\\1  \\2"))))))
;; double-space-punctuation:1 ends here

;; [[file:init-emacs.org::*Text Inserting Functions][Text Inserting Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Text Inserting Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Text Inserting Functions")
;; Text Inserting Functions:1 ends here

;; [[file:init-emacs.org::*insert-timestamp][insert-timestamp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-timestamp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-timestamp")

(defun insert-timestamp (&optional pos)
  "Insert a timestamp at point or POS."
  (interactive "*")
  (if pos
      (save-mark-and-excursion
        (goto-char pos)
        (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))))
;; insert-timestamp:1 ends here

;; [[file:init-emacs.org::*insert-path][insert-path:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-path
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-path")

(defun insert-path (path)
  "Insert path."
  (interactive "*FPath: ")
  (insert (expand-file-name path)))
;; insert-path:1 ends here

;; [[file:init-emacs.org::*uuid][uuid:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: uuid
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: uuid")

;; uuid
(defmacro uuid ()
  "Return a UUID.

Example: 5ac55464-24e6-419c-99cf-5e1682bb3819"
  (cond
   ;; use uuid script if found
   ((executable-find "uuid")
    `(replace-regexp-in-string
      "^ +\\|[ \n]+$" ""
      (shell-command-to-string "uuid")))
   ;; otherwise, use uuidgen command if found
   ((executable-find "uuidgen")
    `(replace-regexp-in-string
      "^ +\\|[ \n]+$" ""
      (shell-command-to-string "uuidgen")))
   ;; otherwise, use linux mcookie command if found
   ((executable-find "mcookie")
    `(let ((uuid (replace-regexp-in-string
                  "^ +\\|[ \n]+$" ""
                  (shell-command-to-string "mcookie"))))
       (concat (substring uuid 0 8)
               "-" (substring uuid 8 12)
               "-" (substring uuid 12 16)
               "-" (substring uuid 16 20)
               "-" (substring uuid 20 32))))
   ;; else, error
   (t
    `(error "Could not find a suitable system command to produce a UUID"))))
(defalias 'guid 'uuid)
;; uuid:1 ends here

;; [[file:init-emacs.org::*insert-uuid][insert-uuid:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-uuid
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-uuid")

(defun insert-uuid ()
  "Insert a UUID at point.

Example: 5ac55464-24e6-419c-99cf-5e1682bb3819"
  (interactive "*")
  (insert (uuid)))
(defalias 'insert-guid 'insert-uuid)
;; insert-uuid:1 ends here

;; [[file:init-emacs.org::*uuid-decimal][uuid-decimal:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: uuid-decimal
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: uuid-decimal")

;; uuid-decimal
(defun uuid-decimal ()
  "Return a UUID as a 128-bit decimal number.

Example: 206479166935211742515584900341856848185"
  (string-to-number (replace-regexp-in-string "-" "" (uuid)) 16))

(defalias 'guid-decimal 'uuid-decimal)
;; uuid-decimal:1 ends here

;; [[file:init-emacs.org::*uuid-string][uuid-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: uuid-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: uuid-string")

;; uuid-string
(defun uuid-string ()
  "Return a UUID as a 21 character ASCII string.

Example: 23MNvqBpz7dP53kZVeGmvR"
  (cl-labels
      (;; convert a number from 0 to 63 to an ASCII string
       (n-to-s (num)
               (cond
                ((< num 10)
                 (byte-to-string (+ num 48))) ; 0-9
                ((< num 36)
                 (byte-to-string (+ num 55))) ; A-Z
                ((< num 62)
                 (byte-to-string (+ num 61))) ; a-z
                ((< num 63)
                 (byte-to-string 43))   ; +
                ((< num 64)
                 (byte-to-string 45))))) ; -
    (do* ((n 0 (mod u 64))
          (s "" (concat (n-to-s n) s))
          (u (uuid-decimal) (/ u 64)))
        ((= u 0) s))))

(defalias 'guid-decimal 'uuid-string)
;; uuid-string:1 ends here

;; [[file:init-emacs.org::*uuid-xml][uuid-xml:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: uuid-xml
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: uuid-xml")

;; uuid-xml
(defmacro uuid-xml ()
  "Return a Java UUID serialized for XML.

Example:

  <java.util.UUID>
    <default>
      <leastSigBits>-8689645201391190588</leastSigBits>
      <mostSigBits>-4837091181110474279</mostSigBits>
    </default>
  </java.util.UUID>"
  (let ((cmd "uuid-xml"))
    ;; use uuid script if found
    (if (executable-find cmd)
        `(shell-command-to-string (concat ,cmd " | tail -n +2"))
      ;; else error
      `(error "Could not find %s command" ,cmd))))
;; uuid-xml:1 ends here

;; [[file:init-emacs.org::*insert-uuid-xml][insert-uuid-xml:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-uuid-xml
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-uuid-xml")

(defun insert-uuid-xml ()
  "Insert a Java UUID serialized for XML at point.

Example:

  <java.util.UUID>
    <default>
      <leastSigBits>-8689645201391190588</leastSigBits>
      <mostSigBits>-4837091181110474279</mostSigBits>
    </default>
  </java.util.UUID>"
  (interactive "*")
  (insert (uuid-xml)))
;; insert-uuid-xml:1 ends here

;; [[file:init-emacs.org::*insert-incrementing-vertical-numbers][insert-incrementing-vertical-numbers:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-incrementing-vertical-numbers
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-incrementing-vertical-numbers")

(defun insert-incrementing-vertical-numbers (bound1 &optional bound2 repeat)
  "Insert incrementing numbers vertically in the current column.

If BOUND2 is nil, number from 1 to BOUND1, inclusive.
If BOUND2 is non-nil, number from BOUND1 to BOUND2, inclusive.
If REPEAT is non-nil, repeat each number that many times."
  (interactive "*nMaximum number: ")
  (let ((start (if bound2 bound1 1))
        (end (if bound2 bound2 bound1))
        (repeat (or repeat 1))
        (col (- (point) (line-beginning-position))))
    (cl-do ((x start (1+ x)))
        ((> x end))
      (cl-do ((y 1 (1+ y)))
          ((> y repeat))
        (insert (number-to-string x))
        (when (or (< x end) (< y repeat))
          (or (zerop (forward-line 1))
              (progn
                (goto-char (line-end-position))
                (newline)))
          (let ((pos (+ (line-beginning-position) col)))
            (while (< (point) pos)
              (if (eobp)
                  (insert " ")
                (forward-char 1)))))))))
;; insert-incrementing-vertical-numbers:1 ends here
