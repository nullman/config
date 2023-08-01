;; [[file:init-emacs.org::#constants-colors][Colors:1]]
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

;; [[file:init-emacs.org::#start-header][Header:1]]
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
;; Header:1 ends here

;; [[file:init-emacs.org::#start-status-messages][Status Messages:1]]
;;------------------------------------------------------------------------------
;;; Start: Status Messages
;;------------------------------------------------------------------------------

;; add timestamps to *Messages* buffer
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

;; keep track of load times
(defvar init-message-timestamp nil
  "Timestamp used to track time between `init-message' calls.")
(setq init-message-timestamp (current-time))

;; load time message
(defun init-message (level format-string &rest args)
  "Custom version of `message' to log messages during Emacs initialization.

LEVEL is the indentation level."
  (let ((file (file-name-sans-extension
               (file-name-nondirectory
                (or load-file-name buffer-file-name (buffer-name)))))
        (time (* (float-time (time-subtract (current-time) init-message-timestamp))
                 1000.0)))
    (message (concat (format "[%4d] " time)
                     file " "
                     (make-string (* 2 level) ?-) "> "
                     (format format-string args) " "))
    (setq init-message-timestamp (current-time))))

(init-message 2 "Start: Status Messages")

;; display load time after startup
(defun emacs-startup-hook--message-startup-time ()
  "Message the Emacs startup time and number of garbage collections."
  (message "Emacs startup time: %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time)))
  (message "Emacs startup garbage collections: %d" gcs-done))
(add-hook 'emacs-startup-hook #'emacs-startup-hook--message-startup-time)
;; Status Messages:1 ends here

;; [[file:init-emacs.org::#start-set-emacs-lisp-garbage-collection-threshold][Set Emacs Lisp Garbage Collection Threshold:1]]
;;------------------------------------------------------------------------------
;;; Start: Set Emacs Lisp Garbage Collection Threshold
;;------------------------------------------------------------------------------

;; reduce frequency of garbage collections
(setq gc-cons-threshold (* 8 1024 1024)) ; default: 800000
;; Set Emacs Lisp Garbage Collection Threshold:1 ends here

;; [[file:init-emacs.org::#start-ignore-errors-advice-wrapper][Ignore Errors Advice Wrapper:1]]
;;------------------------------------------------------------------------------
;;; Start: Ignore Errors Advice Wrapper
;;------------------------------------------------------------------------------

;; generic advice wrapper function to ignore all errors
(defun advice--ignore-all-errors (orig-fun &rest args)
  "Ignore errors when calling ORIG-FUN with ARGS."
  (ignore-errors
    (apply orig-fun args)))

;; generic advice wrapper function to ignore interactive errors
(defun advice--ignore-interactive-errors (orig-fun &rest args)
  "Ignore errors when interactively calling ORIG-FUN with ARGS."
  (condition-case err
      (apply orig-fun args)
    ('error
     (if (called-interactively-p 'any)
         (message "%s" err)
       (error err)))))
;; Ignore Errors Advice Wrapper:1 ends here

;; [[file:init-emacs.org::#start-lock-file-macro-wrapper][Lock-File Macro Wrapper:1]]
;;------------------------------------------------------------------------------
;;; Start: Lock-File Macro Wrapper
;;------------------------------------------------------------------------------

;; lock-file wrapper macro to evaluate code blocks only once per emacs session
(defmacro when-lock-file-acquired (lock-file &rest body)
  "Evaluate BODY unless another running Emacs instance has done so.

LOCK-FILE is a file name to be used as a lock for this BODY code.

Skips checks if run on Windows or Mac."
  (declare (indent 1))
  (let ((procdir (gensym "procdir")))
    `(let ((,procdir (format "/proc/%d" (emacs-pid))))
       (unless (or (string= system-type "windows-nt")
                   (string= system-type "darwin")
                   (file-exists-p ,lock-file))
         (make-symbolic-link ,procdir ,lock-file t))
       (when (or (string= system-type "windows-nt")
                 (string= system-type "darwin")
                 (file-equal-p ,lock-file ,procdir))
         ,@body))))
;; Lock-File Macro Wrapper:1 ends here

;; [[file:init-emacs.org::#package-manager][Package Manager:1]]
;;==============================================================================
;;; Package Manager
;;==============================================================================

(init-message 1 "Package Manager")
;; Package Manager:1 ends here

;; [[file:init-emacs.org::#package-manager-straight][Straight:1]]
;;------------------------------------------------------------------------------
;;; Package Manager: Straight
;;------------------------------------------------------------------------------

(init-message 2 "Package Manager: Straight")

;; initialize package system
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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

;; ;; configure `use-package' to use straight by default
;; (use-package straight
;;   :custom (straight-use-package-by-default t))

;; turn off package file modification check at startup
(setq straight-check-for-modifications '(find-when-checking check-on-save))

;; update recipe repositories
;;(straight-pull-recipe-repositories)
;; Straight:1 ends here

;; [[file:init-emacs.org::#environment][Environment:1]]
;;==============================================================================
;;; Environment
;;==============================================================================

(init-message 1 "Environment")
;; Environment:1 ends here

;; [[file:init-emacs.org::#environment-init-packages][Init Packages:1]]
;;------------------------------------------------------------------------------
;;; Environment: Init Packages
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Init Packages")

;; load packages that are used for initialization
(use-package async
  :straight t)
(use-package bind-key
  :straight t
  :custom
  ;; extract docstrings from lambdas, closures and keymaps if possible
  (bind-key-describe-special-forms t))
(use-package cl-generic
  :straight (:type built-in))
(use-package cl-macs
  :straight (:type built-in))
(use-package dash
  :straight t
  :demand t)
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
;; Init Packages:1 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:1]]
;;------------------------------------------------------------------------------
;;; Environment: Environment
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Environment")
;; Environment:1 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:2]]
;; set coding system to UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Environment:2 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:3]]
;; set timezone to CST
;;(setenv "TZ" "CDT+6")
(setenv "TZ" "America/Chicago")
;; Environment:3 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:4]]
;; determine if running on a MS-Windows display
(defconst window-system-windows
  ;;(memq system-type '(emx win32 w32 mswindows ms-dos windows-nt))
  (string= window-system "w32")
  "Non-nil if running on a MS-Windows display.")
;; Environment:4 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:5]]
;; determine if running on a macintosh gnustep or cocoa display
(defconst window-system-mac
  (string= window-system "ns")
  "Non-nil if running on a Macintosh GNUstep or Cocoa display.")
;; Environment:5 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:6]]
;; determine if running on a Linux X display
(defconst window-system-linux
  (string= window-system "x")
  "Non-nil if running on a Linux X display.")
;; Environment:6 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:7]]
;; determine if running on a work system
(defconst work-system
  (file-exists-p "~/.work")
  "Non-nil if running on a work system.")
;; Environment:7 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:8]]
;; cd to home
(cd "~")
;; Environment:8 ends here

;; [[file:init-emacs.org::#environment-environment][Environment:9]]
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

;; [[file:init-emacs.org::#environment-environment][Environment:10]]
;; set object print depth (do not abbreviate printed objects)
(setq print-length nil
      print-level nil
      eval-expression-print-length nil
      eval-expression-print-level nil)

;; ;; turn off print header
;; (setq ps-print-header nil)
;; Environment:10 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:1]]
;;------------------------------------------------------------------------------
;;; Environment: Global Variables
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Global Variables")
;; Global Variables:1 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:2]]
;; set emacs home directory
(defconst emacs-home-dir
  (file-truename (expand-file-name "~/.emacs.d"))
  "Emacs configuration home directory.")
;; Global Variables:2 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:3]]
(defmacro emacs-home-sub-dir (dir)
  "Return expanded directory name of DIR if found as a
sub-directory of `emacs-home-dir', or just `emacs-home-dir'
otherwise."
  `(let ((file (expand-file-name ,dir emacs-home-dir)))
     (if (file-exists-p file)
         (file-truename file)
       emacs-home-dir)))
;; Global Variables:3 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:4]]
;; set emacs modules directory
(defconst emacs-modules-dir
  (emacs-home-sub-dir "modules")
  "Emacs modules directory.")
;; Global Variables:4 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:6]]
;; set local modules directory
(defconst local-modules-dir
  (emacs-home-sub-dir "local-modules")
  "Emacs local modules directory.")
;; Global Variables:6 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:7]]
;; set local work modules directory
(defconst local-work-modules-dir
  (emacs-home-sub-dir "local-work-modules")
  "Emacs local work modules directory.")
;; Global Variables:7 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:8]]
;; set customization file
(defconst customization-file
  (file-truename (expand-file-name "customization.el" emacs-home-dir))
  "Emacs customization file.")
(setq custom-file customization-file)
;; Global Variables:8 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:9]]
;; set init-emacs.org true file name
(defconst init-emacs-true-file-name
  (file-truename (expand-file-name "init-emacs.org" emacs-home-dir))
  "The true file name of this buffer.")
;; Global Variables:9 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:10]]
;; set user name
(defconst user-name "kyle")
(defconst user-full-name "Kyle W T Sherman")
(defconst user-short-name "Kyle Sherman")
(defconst user-first-name "Kyle")
(defconst user-last-name "Sherman")
;; Global Variables:10 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:11]]
;; set email address
(defconst user-mail-address
  (if (getenv "EMAIL")
      (getenv "EMAIL")
    (concat "kyle" "w" "sherman" "@" "gmail" "." "com"))
  "User email address.")
;; Global Variables:11 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:12]]
;; set no-spam email address
(defconst user-mail-address-nospam
  (replace-regexp-in-string "\\." " dot "
                            (replace-regexp-in-string "@" " at " user-mail-address))
  "Slightly obfuscated user email address.")
;; Global Variables:12 ends here

;; [[file:init-emacs.org::#environment-global-variables][Global Variables:13]]
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

;; [[file:init-emacs.org::#environment-load-path][Load Path:1]]
(init-message 2 "Environment: Load Path")

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

;; [[file:init-emacs.org::#environment-settings-gui-header][Header:1]]
(when window-system
;; Header:1 ends here

;; [[file:init-emacs.org::#environment-gui-general][General:1]]
;;------------------------------------------------------------------------------
;;;; Environment: GUI: General
;;------------------------------------------------------------------------------

(init-message 3 "Environment: GUI: General")

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

;; stretch cursor to glyph width
(setq x-stretch-cursor t)

;; ;; scroll bar on right
;; (setq scroll-bar-mode 'right)
;; (scroll-bar-mode -1)
;; (scroll-bar-mode 1)

;; turn off scroll bar
(when (fboundp 'scroll-bar-mode)
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
;; General:1 ends here

;; [[file:init-emacs.org::#environment-gui-font][Font:1]]
;;------------------------------------------------------------------------------
;;;; Environment: GUI: Font
;;------------------------------------------------------------------------------

(init-message 3 "Environment: GUI: Font")

;; set default font
(cl-labels
    ((set-font (font)
               (set-face-attribute 'default nil :font font)
               (set-face-attribute 'fixed-pitch nil :font font)
               (set-face-attribute 'variable-pitch nil :font font)))
  (cl-case window-system
    (x
     (condition-case nil
         (set-font
          (cond
           ;; "8x13"
           ;; "9x15"
           ;; "Ubuntu Mono-13"
           ;; "Inconsolata-15"
           ;; "BitstreamVeraSansMono Nerd Font Mono-14"
           ;; "DroidSansMono Nerd Font Mono-14"
           ;; "Hack Nerd Font Mono-14"
           ((x-list-fonts "Hack Nerd Font")
            "Hack Nerd Font Mono-14")
           ((x-list-fonts "DroidSansMono Nerd Font")
            "DroidSansMono Nerd Font Mono-14")
           ((x-list-fonts "Fira Code")
            "Fira Code Mono-14")
           (t
            "9x15" nil)))
       ('error
        (set-font "9x15"))))
    (w32
     (condition-case nil
         (set-font "Hack Nerd Font Mono-14")
       ('error
        nil)))
    (ns
     (condition-case nil
         ;;(set-font "BitstreamVeraSansMono Nerd Font Mono-14")
         ;;(set-font "DroidSansMono Nerd Font Mono-14")
         (set-font "Hack Nerd Font Mono-14")
       ('error
        (set-font "Menlo"))))))
;; Font:1 ends here

;; [[file:init-emacs.org::#environment-gui-faces][Faces:1]]
;;------------------------------------------------------------------------------
;;;; Environment: GUI: Faces
;;------------------------------------------------------------------------------

(init-message 3 "Environment: GUI: Faces")

;; ;; set faces
;; ;; white foreground on black background with yellow cursor
;; (custom-set-faces
;;  `(default ((t (:foreground ,color-foreground :background ,color-background))))
;;  `(cursor ((t (:foreground ,color-background :background ,color-cursor)))))

;; ;; set faces
;; ;; green foreground on black background with yellow cursor
;; (custom-set-faces
;;  `(default ((t (:foreground ,color-foreground :background ,color-background)))) ; green
;;  `(cursor ((t (:foreground ,color-background :background ,color-cursor))))) ; yellow

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
(set-frame-parameter (selected-frame) 'alpha background-alpha)
(add-to-list 'default-frame-alist (cons 'alpha background-alpha))

;; set mouse color
(set-mouse-color color-mouse)
;; Faces:1 ends here

;; [[file:init-emacs.org::#environment-gui-modus-themes][Modus Themes:1]]
;;------------------------------------------------------------------------------
;;;; Environment: GUI: Modus Themes
;;------------------------------------------------------------------------------

(init-message 3 "Environment: GUI: Modus Themes")

;; modus themes
;; https://github.com/protesilaos/modus-themes/
(use-package modus-themes
  :straight (:type built-in)
  :demand t
  :bind ("<f2>" . modus-themes-toggle)
  :init
  ;; add customizations before loading
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia t
        modus-themes-tabs-accented t
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related
        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers nil

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9))

        ;; Same as above:
        ;; modus-themes-mode-line '(accented borderless 4 0.9)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup '(background italic)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        ;;modus-themes-syntax nil
        modus-themes-syntax '(alt-syntax faint green-strings yellow-comments)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        ;;modus-themes-hl-line '(underline accented)
        modus-themes-hl-line '(intense)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline background)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat', `accented',
        ;; `faint', `variable-pitch', `underline', `all-buttons', the
        ;; symbol of any font weight as listed in `modus-themes-weights',
        ;; and a floating point number (e.g. 0.9) for the height of the
        ;; button's text.
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        ;;modus-themes-region '(bg-only no-extend)
        modus-themes-region nil

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        ;;modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}
        modus-themes-org-blocks nil ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch 1.3))
          (header-date . (grayscale workaholic bold-today 1.1))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . traffic-light))

        modus-themes-headings ; this is an alist: read the manual or its doc string
        ;; '((1 . (overline background variable-pitch 1.3))
        ;;   (2 . (rainbow overline 1.1))
        ;;   (t . (semibold)))
        ;;'((t . (rainbow)))
        '((t . ()))

        modus-themes-vivendi-color-overrides ; override some main colors
        `((bg-main . ,color-background)
          (fg-main . ,color-foreground)))

  ;; load theme files before enabling
  ;;(modus-themes-load-themes)
  ;;(load-theme 'modus-operandi :no-error)
  (load-theme 'modus-vivendi :no-error)
  :config
  ;;(modus-themes-load-operandi)
  (modus-themes-load-vivendi))
;; Modus Themes:1 ends here

;; [[file:init-emacs.org::#environment-settings-gui-footer][Footer:1]]
)
;; Footer:1 ends here

;; [[file:init-emacs.org::#environment-general][General:1]]
;;------------------------------------------------------------------------------
;;; Environment: General
;;------------------------------------------------------------------------------

(init-message 2 "Environment: General")
;; General:1 ends here

;; [[file:init-emacs.org::#environment-general][General:2]]
;; disable splash screen
(setq inhibit-startup-screen t)
;; General:2 ends here

;; [[file:init-emacs.org::#environment-general][General:3]]
;; prefer newer el files over elc
(setq load-prefer-newer t)
;; General:3 ends here

;; [[file:init-emacs.org::#environment-general][General:4]]
;; hide menu-bar (use C-M-z to activate)
(when (and (fboundp 'menu-bar-mode)
           menu-bar-mode)
  (menu-bar-mode -1))
;; General:4 ends here

;; [[file:init-emacs.org::#environment-general][General:6]]
;; set default buffer mode to `org-mode'
(setq initial-major-mode 'org-mode)
;; General:6 ends here

;; [[file:init-emacs.org::#environment-general][General:7]]
;; clear scratch buffer
(setq initial-scratch-message nil)
;; General:7 ends here

;; [[file:init-emacs.org::#environment-general][General:9]]
;; make baskspace key work
(normal-erase-is-backspace-mode 1)
;; General:9 ends here

;; [[file:init-emacs.org::#environment-general][General:10]]
;; add underscore to word boundaries
(modify-syntax-entry ?_ "w")

;; add dash to word boundaries
(modify-syntax-entry ?- "w")
;; General:10 ends here

;; [[file:init-emacs.org::#environment-general][General:11]]
;; add camel-case words to word boundaries
(global-subword-mode 1)
;; General:11 ends here

;; [[file:init-emacs.org::#environment-general][General:12]]
;; beginning of defun is outermost level open-paren
(setq open-paren-in-column-0-is-defun-start nil
      defun-prompt-regexp nil)
;; General:12 ends here

;; [[file:init-emacs.org::#environment-general][General:13]]
;; do not parse comments in sexp's
(setq parse-sexp-ignore-comments t)
(setq-default parse-sexp-ignore-comments parse-sexp-ignore-comments)
;; General:13 ends here

;; [[file:init-emacs.org::#environment-general][General:14]]
;; ;; wrap lines
;; (setq truncate-lines nil)
;; General:14 ends here

;; [[file:init-emacs.org::#environment-general][General:15]]
;; turn off line wrapping
(setq truncate-lines t)
(setq-default truncate-lines truncate-lines)
(toggle-truncate-lines 1)
;; General:15 ends here

;; [[file:init-emacs.org::#environment-general][General:17]]
;; do not automatically break lines by inserting newlines
(turn-off-auto-fill)
;; General:17 ends here

;; [[file:init-emacs.org::#environment-general][General:19]]
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

;; [[file:init-emacs.org::#environment-general][General:20]]
;; when `display-line-numbers-mode' is on use relative numbering
(setq display-line-numbers-type 'relative)
;; General:20 ends here

;; [[file:init-emacs.org::#environment-general][General:21]]
;; stop cursor at the end of the file
(setq next-line-add-newlines nil)
;; General:21 ends here

;; [[file:init-emacs.org::#environment-general][General:22]]
;; keep screen position when using page-up and page-down
(setq scroll-preserve-screen-position 'keep)
;; General:22 ends here

;; [[file:init-emacs.org::#environment-general][General:23]]
;; scroll one line at a time
(setq scroll-step 1)
;; scroll fewer lines
(setq scroll-conservatively 101)
;; scroll before reaching window edge
(setq scroll-margin 2)
;; turn off vertical auto-scroll
(setq auto-window-vscroll nil)
;; General:23 ends here

;; [[file:init-emacs.org::#environment-general][General:24]]
;; make searches case-insensitive
(setq case-fold-search t)
;; General:24 ends here

;; [[file:init-emacs.org::#environment-general][General:25]]
;; highlight search matches
(setq search-highlight t
      ;;isearch-highlight t
      query-replace-highlight t)
;; General:25 ends here

;; [[file:init-emacs.org::#environment-general][General:26]]
;; make current selection visible
(transient-mark-mode 1)
(setq-default transient-mark-mode transient-mark-mode)
;; General:26 ends here

;; [[file:init-emacs.org::#environment-general][General:27]]
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

;; [[file:init-emacs.org::#environment-general][General:28]]
;; custom fill-column value
(defconst custom-fill-column 78
  "Custom `fill-column' value.")

;; set default fill column for `auto-fill-mode' mode and `fill-paragraph'
(setq fill-column custom-fill-column)
(setq-default fill-column fill-column)
;; General:28 ends here

;; [[file:init-emacs.org::#environment-general][General:29]]
;; set default comment column for in-line comments
(setq comment-column 40)
(setq-default comment-column comment-column)
;; set default comment fill column for in-line comments
(setq comment-fill-column nil)
(setq-default comment-fill-column comment-fill-column)
;; General:29 ends here

;; [[file:init-emacs.org::#environment-general][General:30]]
;; turn on goal column support
(put 'set-goal-column 'disabled nil)
;; General:30 ends here

;; [[file:init-emacs.org::#environment-general][General:31]]
;; insert one space after a sentence when filling text
(setq sentence-end-double-space nil)
;; insert one space after a colon when filling text
(setq colon-double-space nil)
;; General:31 ends here

;; [[file:init-emacs.org::#environment-general][General:33]]
;; highlight matching parenthesis
(show-paren-mode 1)
(set-face-foreground 'show-paren-match color-paren)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;; General:33 ends here

;; [[file:init-emacs.org::#environment-general][General:34]]
;; highlight tabs
(setq highlight-tabs t)
(setq-default highlight-tabs highlight-tabs)
;; General:34 ends here

;; [[file:init-emacs.org::#environment-general][General:35]]
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
(global-whitespace-mode 1)              ; enable whitespace mode everywhere
;; General:35 ends here

;; [[file:init-emacs.org::#environment-general][General:36]]
;; highlight current line
(hl-line-mode 1)
(global-hl-line-mode 1)
;; General:36 ends here

;; [[file:init-emacs.org::#environment-general][General:37]]
;; turn on global font lock mode and syntax highlighting
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
;; General:37 ends here

;; [[file:init-emacs.org::#environment-general][General:38]]
;; replace highlighted text with typed text
(delete-selection-mode 1)
;; General:38 ends here

;; [[file:init-emacs.org::#environment-general][General:39]]
;; ;; set comment start (default) and padding
;; (setq comment-start "#"
;;       comment-padding " ")
;; set comment style
(setq comment-style 'indent)
;; General:39 ends here

;; [[file:init-emacs.org::#environment-general][General:40]]
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
;; General:40 ends here

;; [[file:init-emacs.org::#environment-general][General:42]]
;; make apropos command search all symbols
(setq apropos-do-all t)

;; make apropos command list results by relevance
(setq apropos-sort-by-scores t
      apropos-documentation-sort-by-scores t)
;; General:42 ends here

;; [[file:init-emacs.org::#environment-general][General:43]]
;; display customize menu entries and tag names as symbols
(setq custom-unlispify-menu-entries nil
      custom-unlispify-tag-names nil)
(setq-default custom-unlispify-menu-entries custom-unlispify-menu-entries
              custom-unlispify-tag-names custom-unlispify-tag-names)
;; General:43 ends here

;; [[file:init-emacs.org::#environment-general][General:44]]
;; set grep command
(setq grep-command "grep -n -H -i -r -e ")
;; General:44 ends here

;; [[file:init-emacs.org::#environment-general][General:45]]
;; email settings
(setq mail-sources `((pop :server "pop.gmail.com" :port 995
                          :user ,user-mail-address
                          :connection ssl :leave t)))
;; General:45 ends here

;; [[file:init-emacs.org::#environment-general][General:46]]
;; set default browser
;;(setq browse-url-browser-function #'browse-url-default-browser)
;;(setq browse-url-generic-program "x-www-browser")
;;(setq browse-url-generic-program "w3m")
;;(setq browse-url-generic-program "mozilla")
;;(setq browse-url-browser-function #'browse-url-generic)
;;(setq browse-url-browser-function #'eww-browse-url)
;;(setq browse-url-browser-function #'w3m-browse-url)
(setq browse-url-browser-function #'browse-url-firefox
      browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)
;; set secondary browser
(setq browse-url-secondary-browser-function #'browse-url-default-browser)
;; General:46 ends here

;; [[file:init-emacs.org::#environment-general][General:47]]
;; when deleting an active region via single character deletion command,
;; do not save to kill ring
(setq delete-active-region t)
;; General:47 ends here

;; [[file:init-emacs.org::#environment-general][General:50]]
;; always recenter after `occur-mode-goto-occurrence'
(defun occur-mode-goto-occurrence--recenter (&optional arg)
  "Recenter when an `occur' result is selected."
  (recenter))
;; advise `occur-mode-goto-occurrence'
(advice-add 'occur-mode-goto-occurrence :after #'occur-mode-goto-occurrence--recenter)
;; General:50 ends here

;; [[file:init-emacs.org::#environment-general][General:51]]
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
;; General:51 ends here

;; [[file:init-emacs.org::#environment-system][System:1]]
;;------------------------------------------------------------------------------
;;; Environment: System
;;------------------------------------------------------------------------------

(init-message 2 "Environment: System")
;; System:1 ends here

;; [[file:init-emacs.org::#environment-system][System:3]]
;; set max variable bindings
(setq max-specpdl-size 10000)           ; default: 1300
;; System:3 ends here

;; [[file:init-emacs.org::#environment-system][System:4]]
;; set max eval depth
(setq max-lisp-eval-depth 10000)        ; default: 600
;; System:4 ends here

;; [[file:init-emacs.org::#environment-system][System:5]]
;; set max message log size
(setq message-log-max 10000)            ; default: 1000
;; System:5 ends here

;; [[file:init-emacs.org::#environment-system][System:6]]
;; set max history list size
(setq history-length 250)               ; default: 30

;; remove duplicates from history lists
(setq history-delete-duplicates t)      ; default: nil
;; System:6 ends here

;; [[file:init-emacs.org::#environment-system][System:7]]
;; set max kill ring size
(setq kill-ring-max 100)                ; default: 60

;; set max mark ring size
(setq mark-ring-max 32)                 ; default: 16
;; System:7 ends here

;; [[file:init-emacs.org::#environment-system][System:8]]
;; change all calls to `yes-or-no-p' to `y-or-n-p'
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
;; System:8 ends here

;; [[file:init-emacs.org::#environment-system][System:9]]
;; enable upercase region (C-x C-u)
(put 'upcase-region 'disabled nil)

;; enable lowercase region (C-x C-l)
(put 'downcase-region 'disabled nil)

;; enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; turn off the disabling of certain commands
(setq disabled-command-function nil)
;; System:9 ends here

;; [[file:init-emacs.org::#environment-system][System:10]]
;; turn off bidirectional paragraph formatting
(setq bidi-paragraph-direction 'left-to-right)
(setq-default bidi-paragraph-direction bidi-paragraph-direction)

;; turn off bidirectional parentheses matching
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t))

;; turn on `so-long-mode' for files with long lines to help with performance
(when (version<= "27.1" emacs-version)
  (global-so-long-mode 1))
;; System:10 ends here

;; [[file:init-emacs.org::#environment-system][System:11]]
;; silence advice redefinition warnings
(setq ad-redefinition-action 'accept)
(setq-default ad-redefinition-action ad-redefinition-action)
;; System:11 ends here

;; [[file:init-emacs.org::#environment-files][Files:1]]
;;------------------------------------------------------------------------------
;;; Environment: Files
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Files")
;; Files:1 ends here

;; [[file:init-emacs.org::#environment-files-general][General:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: General
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: General")

(use-package files
  :straight (:type built-in)
  :custom
  ;; do not make backup files
  (make-backup-files nil)
  (backup-inhibited t)
  ;; reuse existing buffers, following file links
  (find-file-existing-other-name t)
  ;; end files with a newline
  (require-final-newline t)
  ;; do not make auto-save files
  (auto-save-default nil)
  ;; disable auto-save list
  (auto-save-list-file-prefix nil)
  ;; delete auto-save files
  (delete-auto-save-files t)
  ;; enable file local variables
  (enable-local-variables t)
  ;; enable directory local variables
  (enable-dir-local-variables t)
  ;; ask user before evaluating local variables
  (enable-local-eval 'maybe)
  ;; increase maximum file size (in bytes) to open before confirmation is requested
  (large-file-warning-threshold (* 50 1000 1000))
  ;; all backup files should go into the system temp directory
  (backup-directory-alist `(("." . ,temporary-file-directory)))
  ;; ask before closing emacs
  (kill-emacs-query-functions
   (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
         kill-emacs-query-functions)))
;; General:1 ends here

;; [[file:init-emacs.org::#environment-files-general][General:2]]
;; org-babel noweb start and end patterns are considered safe
(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-start . "{{"))
(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-end . "}}"))
;; General:2 ends here

;; [[file:init-emacs.org::#environment-files-general][General:3]]
(defun create-buffer-file-name-directory-if-needed ()
  "Create `buffer-file-name' directory if it does not already exist."
  (when (and buffer-file-name
             (not (file-exists-p (file-name-directory buffer-file-name))))
    (make-directory (file-name-directory buffer-file-name) t)))

;; create directories if needed on file save
(add-hook 'before-save-hook #'create-buffer-file-name-directory-if-needed)
;; General:3 ends here

;; [[file:init-emacs.org::#environment-files-general][General:4]]
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
;; General:4 ends here

;; [[file:init-emacs.org::#environment-files-general][General:5]]
;; make shell scripts executable when saving (and reset the buffer mode)
(when (fboundp 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))
;; General:5 ends here

;; [[file:init-emacs.org::#environment-files-general][General:6]]
;; set DOS file extensions
(add-to-list 'file-coding-system-alist '("\\.ASM\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.BAT\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.DO\\'" . dos))
(add-to-list 'file-coding-system-alist '("\\.SYS\\'" . dos))
;; General:6 ends here

;; [[file:init-emacs.org::#environment-files-version-control][Version Control:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Version Control
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Version Control")

(use-package vc-hooks
  :straight (:type built-in)
  :custom
  ;; follow symlinks to version control files without asking or warning
  (vc-follow-symlinks t)
  ;; disable vc-git as it throws debugger errors when revert-buffer is called
  (vc-handled-backends nil))
;; Version Control:1 ends here

;; [[file:init-emacs.org::#environment-files-compression][Compression:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Compression
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Compression")

(use-package jka-cmpr-hook
  :straight (:type built-in)
  :custom
  ;; automatically uncompress and compress gzip/zip/jar/tar files
  (auto-compression-mode t))
;; Compression:1 ends here

;; [[file:init-emacs.org::#environment-files-auto-revert][Auto-Revert:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Auto-Revert
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Auto-Revert")

(use-package autorevert
  :straight (:type built-in)
  :custom
  ;; turn on auto buffer revert mode
  (global-auto-revert-mode 1))
;; Auto-Revert:1 ends here

;; [[file:init-emacs.org::#environment-files-bookmark][Bookmark:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Bookmark
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Bookmark")

(use-package bookmark
  :straight (:type built-in)
  :custom
  ;; auto-save bookmarks every time they change
  (bookmark-save-flag 1))
;; Bookmark:1 ends here

;; [[file:init-emacs.org::#environment-files-desktop][Desktop:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Desktop
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Desktop")

(use-package desktop
  :straight (:type built-in)
  :custom
  (desktop-save 'ask-if-new)
  (desktop-load-locked-desktop t)
  (desktop-restore-eager 0) ; do not restore any buffers until all modules and modes have loaded
  (desktop-buffers-not-to-save (concat "\\("
                                       "\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                       "\\.diary\\|\\diary\\|\\.bbdb"
                                       "\\)$"))
  :init
  ;; desktop history
  (when-lock-file-acquired (expand-file-name "emacs-desktop-history-lock-file"
                                             temporary-file-directory)
    (desktop-save-mode 1)
    (add-to-list 'desktop-globals-to-save 'file-name-history t)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode t)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode t)
    ;;(add-to-list 'desktop-modes-not-to-save 'dired-mode t)
    ;;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode t)
    ))
;; Desktop:1 ends here

;; [[file:init-emacs.org::#environment-files-minibuffer-history][Minibuffer History:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Files: Minibuffer History
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Files: Minibuffer History")

(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables '(search-ring regexp-search-ring))
  :init
  ;; save minibuffer history
  (when (fboundp 'savehist-mode)
    (when-lock-file-acquired (expand-file-name "emacs-minibuffer-history-lock-file"
                                               temporary-file-directory)
      (savehist-mode 1))))
;; Minibuffer History:1 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:1]]
;;------------------------------------------------------------------------------
;;; Environment: Buffers and Windows
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Buffers and Windows")
;; Buffers and Windows:1 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:2]]
;; allow undo/redo of window settings
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; Buffers and Windows:2 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:4]]
;; delay buffer fontification to increase scroll speed
(setq jit-lock-defer-time 0.05)
;; Buffers and Windows:4 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:5]]
;; preserve buffer point for each window
(setq switch-to-buffer-preserve-window-point t)
;; Buffers and Windows:5 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:6]]
;; smoother mouse movement
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
;; Buffers and Windows:6 ends here

;; [[file:init-emacs.org::#environment-buffers-and-windows][Buffers and Windows:7]]
;; increase maximum mini-window height
(setq max-mini-window-height 0.50)
;; Buffers and Windows:7 ends here

;; [[file:init-emacs.org::#environment-tabs][Tabs:1]]
;;------------------------------------------------------------------------------
;;; Environment: Tabs
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Tabs")

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
;; Tabs:1 ends here

;; [[file:init-emacs.org::#environment-terminals][Terminals:1]]
;;------------------------------------------------------------------------------
;;; Environment: Terminals
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Terminals")
;; Terminals:1 ends here

;; [[file:init-emacs.org::#environment-terminals-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Terminals: Configuration
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Terminals: Configuration")

(setq custom-terminal-history-size 10000
      custom-terminal-maximum-lines 10000)
;; Configuration:1 ends here

;; [[file:init-emacs.org::#environment-terminals-eshell][eshell:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Terminals: eshell
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Terminals: eshell")

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

  (with-eval-after-load "esh-opt"
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands '("htop" "ssh" "vim" "zsh"))))

(use-package eshell-git-prompt
  :straight t
  :after (eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))
;; eshell:1 ends here

;; [[file:init-emacs.org::#environment-terminals-term-bash][term-bash:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Terminals: term-bash
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Terminals: term-bash")

(defun term-bash ()
  "Start a BASH terminal-emulator in a new buffer."
  (interactive)
  (term "/bin/bash"))
;; term-bash:1 ends here

;; [[file:init-emacs.org::#environment-terminals-term-zsh][term-zsh:1]]
;;------------------------------------------------------------------------------
;;;; Environment: Terminals: term-zsh
;;------------------------------------------------------------------------------

(init-message 3 "Environment: Terminals: term-zsh")

(defun term-zsh ()
  "Start a ZSH terminal-emulator in a new buffer."
  (interactive)
  (term "/bin/zsh"))
;; term-zsh:1 ends here

;; [[file:init-emacs.org::#environment-bookmarks][Bookmarks:1]]
;;------------------------------------------------------------------------------
;;; Environment: Bookmarks
;;------------------------------------------------------------------------------

(init-message 2 "Environment: Bookmarks")
;; Bookmarks:1 ends here

;; [[file:init-emacs.org::#key-bindings][Key Bindings:1]]
;;==============================================================================
;;; Key Bindings
;;==============================================================================

(init-message 1 "Key Bindings")
;; Key Bindings:1 ends here

;; [[file:init-emacs.org::#key-bindings-system-keys][System Keys:1]]
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

;; [[file:init-emacs.org::#key-bindings-function-keys][Function Keys:1]]
;;------------------------------------------------------------------------------
;;; Key Bindings: Function Keys
;;------------------------------------------------------------------------------

(init-message 2 "Key Bindings: Function Keys")

(defun custom-key-bindings-function-keys ()
  "Set custom function key bindings."
  (when (fboundp 'help-for-help)
    (bind-keys ("<f1>" . help-for-help)))
  (when (fboundp 'help-command)
    (bind-keys ("S-<f1>" . help-command)))
  (when (fboundp 'kmacro-start-macro-or-insert-counter)
    (bind-keys ("<f3>" . kmacro-start-macro-or-insert-counter))) ; default: `kmacro-start-macro-or-insert-counter'
  (when (fboundp 'kmacro-end-or-call-macro)
    (bind-keys ("<f4>" . kmacro-end-or-call-macro))) ; default: `kmacro-end-or-call-macro'
  (when (fboundp 'define-word-at-point-after-spell-check)
    (bind-keys ("<f5>" . define-word-at-point-after-spell-check)))
  (when (fboundp 'define-word-after-spell-check)
    (bind-keys ("S-<f5>" . define-word-after-spell-check)))
  ;; (when (fboundp 'ispell-word)
  ;;   (bind-keys ("<f6>" . ispell-word)))
  ;; (when (fboundp 'ispell)
  ;;   (bind-keys ("<S-f6>" . ispell)))
  (when (fboundp 'web-query-symbol-by-mode-at-point)
    (bind-keys ("<f7>" . web-query-symbol-by-mode-at-point)))
  ;; (when (fboundp 'web-query-word-at-point)
  ;;   (bind-keys ("<f7>" . web-query-word-at-point)))
  (when (fboundp 'web-query)
    (bind-keys ("<S-f7>" . web-query)))
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
  ;; normally f11 and f12 are left for screen to use
  (unbind-key "<f11>")                  ; default: `toggle-frame-fullscreen'
  (unbind-key "<f12>"))

(init-message 3 "custom-key-bindings-function-keys")
(custom-key-bindings-function-keys)
;; Function Keys:1 ends here

;; [[file:init-emacs.org::#key-bindings-extended-keys][Extended Keys:1]]
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

;; [[file:init-emacs.org::#key-bindings-movement-keys][Movement Keys:1]]
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
                ;;("<up>" . previous-line) ; default: `previous-line'
                ;;("<down>" . next-line)   ; default: `next-line'
                ;;("<left>" . left-char)   ; default: `left-char'
                ;;("<right>" . right-char) ; default: `right-char'
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

    ;; window resize keys
    (when (fboundp 'window-shrink-vertically)
      (bind-keys :map keymap ("C-M-S-i" . window-shrink-vertically)))
    (when (fboundp 'window-enlarge-vertically)
      (bind-keys :map keymap ("C-M-S-k" . window-enlarge-vertically)))
    (when (fboundp 'window-shrink-horizontally)
      (bind-keys :map keymap ("C-M-S-j" . window-shrink-horizontally)))
    (when (fboundp 'window-enlarge-horizontally)
      (bind-keys :map keymap ("C-M-S-l" . window-enlarge-horizontally)))

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

;; [[file:init-emacs.org::#key-bindings-standard-keys][Standard Keys:1]]
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
    (bind-keys ("C-x r C-y" . yank-as-rectangle)))
    ;; (bind-keys ("C-x r C-y" . yank-as-rectangle)
    ;;            ("C-M-y" . yank-as-rectangle)))

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

  ;; shortdoc
  (when (fboundp 'shortdoc-display-group)
    (bind-keys ("C-h D" . shortdoc-display-group)
               ("C-x C-h D" . shortdoc-display-group)))

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

  ;; cycle to previous buffer
  (bind-keys ("C-`" . mode-line-other-buffer))

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
  ;;   (bind-keys ("C-=" . er/expand-region)   ; default: `count-lines-region'
  ;;              ("C-+" . er/contract-region) ; default: `count-lines-region'
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

  ;; describe text properties
  (bind-keys ("C-x M-p" . describe-text-properties))

  ;; undo
  (bind-keys ("C-_" . undo))

  ;; undo-redo
  (bind-keys ("M-_" . undo-redo))

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

  ;; ;; complete tag
  ;; (when (fboundp 'complete-tag)
  ;;   (bind-keys ("C-M-/" . complete-tag)))

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
    (bind-keys ("C-x C-]" . pop-up-shell-toggle)))

  ;; describe character under cursor
  (bind-keys ("C-x _" . describe-char)))

(init-message 3 "custom-key-bindings-standard-keys")
(custom-key-bindings-standard-keys)
;; Standard Keys:1 ends here

;; [[file:init-emacs.org::#key-bindings-modes-and-module-keys][Modes and Module Keys:1]]
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

;; [[file:init-emacs.org::#key-bindings-grouped-prefix-keys][Grouped Prefix Keys:1]]
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
  (bind-keys :prefix "C-h e"
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
  (bind-keys :prefix "M-SPC"
             :prefix-map space-map
             :menu-name "Space Prefix Launching Point")
  (bind-keys ("C-." . space-map))      ; in case the OS consumes M-SPC

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
             ("b" . emacs-lisp-byte-compile)
             ("c" . customize-group)
             ("g" . magit-status)
             ("m" . macrostep-mode)
             ("s" . server-start-maybe)
             ("w" . webjump)
             ("x" . regexp-builder))
  ;; miscellaneous display commands
  (bind-keys :map space-miscellaneous-map
             :prefix "d"
             :prefix-map space-miscellaneous-display-map
             :menu-name "Display Commands"
             ("c" . list-colors-display)
             ("f" . list-faces-display)
             ("s" . list-character-sets)
             ("w" . display-time-world))
  (when (fboundp 'term-bash)
    (bind-keys :map space-miscellaneous-display-map ("u" . list-charset-unicode)))
  ;; miscellaneous eval commands
  (bind-keys :map space-miscellaneous-map
             :prefix "e"
             :prefix-map space-miscellaneous-eval-map
             :menu-name "Eval Commands"
             ("b" . eval-buffer)
             ("i" . ielm)
             ("r" . eval-region))
  ;; miscellaneous format commands
  (bind-keys :map space-miscellaneous-map
             :prefix "f"
             :prefix-map space-miscellaneous-format-map
             :menu-name "Format Commands"
             ("j" . json-pretty-print)
             ("x" . xml-pretty-print))
  ;; miscellaneous toggle commands
  (bind-keys :map space-miscellaneous-map
             :prefix "t"
             :prefix-map space-miscellaneous-toggle-map
             :menu-name "Toggle Commands"
             ("d" . toggle-debug-on-error)
             ("q" . toggle-debug-on-quit)
             ("s" . toggle-case-fold-search)
             ("t" . toggle-truncate-lines)
             ("v" . visual-line-mode))

  ;; package commands
  (bind-keys :map space-map
             :prefix "p"
             :prefix-map space-package-map
             :menu-name "Package Commands"
             ("i" . package-install)
             ("l" . package-list-packages-no-fetch)
             ("L" . package-list-packages)
             ("R" . straight-pull-recipe-repositories)
             ("P" . straight-pull-all)
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

  ;; ;; window resize commands
  ;; (bind-keys :map space-map
  ;;            :prefix "w"
  ;;            :prefix-map space-window-resize-map
  ;;            :menu-name "Window Resize")
  ;; (when (fboundp 'window-shrink-vertically)
  ;;   (bind-keys :map space-window-resize-map ("i" . window-shrink-vertically)))
  ;; (when (fboundp 'window-enlarge-vertically)
  ;;   (bind-keys :map space-window-resize-map ("k" . window-enlarge-vertically)))
  ;; (when (fboundp 'window-shrink-horizontally)
  ;;   (bind-keys :map space-window-resize-map ("j" . window-shrink-horizontally)))
  ;; (when (fboundp 'window-enlarge-horizontally)
  ;;   (bind-keys :map space-window-resize-map ("l" . window-enlarge-horizontally)))

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

;; [[file:init-emacs.org::#key-bindings-set-all-custom-key-bindings][Set All Custom Key Bindings:1]]
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

;; [[file:init-emacs.org::#org-mode][Org Mode:1]]
;;==============================================================================
;;; Org Mode
;;==============================================================================

(init-message 1 "Org Mode")
;; Org Mode:1 ends here

;; [[file:init-emacs.org::#org-mode-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Configuration")

(use-package org
  :straight (:type built-in)
  :demand t
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :bind* (("C-c a" . org-agenda)
          ("C-c c" . org-capture)
          ;;("C-c l" . org-store-link)
          ("C-c j" . org-babel-tangle-jump-to-org))
  :custom
  ;; org directory
  (org-directory (file-truename (expand-file-name "~/org")))
  ;; ;; use org indent mode
  ;; (org-indent-mode 1)
  ;; indent blocks to outline node level
  (org-adapt-indentation t)
  ;; ;; do not indent blocks to outline node level
  ;; (org-adapt-indentation nil)
  ;; odd level headings only
  (org-odd-levels-only t)
  ;; ;; odd and even level headings
  ;; (org-odd-levels-only nil)
  ;; hide leading stars on headings
  (org-hide-leading-stars t)
  ;; startup in "overview" (folded) by default
  (org-startup-folded 'overview)
  ;; remap disputed keys (see `org-disputed-keys')
  (org-replace-disputed-keys t)
  ;; shift-cursor commands select text when possible
  (org-support-shift-select 'always)
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
  ;; always show lineage
  (org-show-context-detail
   '((default . lineage)
     (agenda . local)))
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
   `(org-block ((t (:inherit shadow :foreground ,color-foreground)))))
  ;;  `(org-level-1 ((t (:foreground ,color-1))))
  ;;  `(org-level-2 ((t (:foreground ,color-2))))
  ;;  `(org-level-3 ((t (:foreground ,color-3))))
  ;;  `(org-level-4 ((t (:foreground ,color-4))))
  ;;  `(org-level-5 ((t (:foreground ,color-5))))
  ;;  `(org-level-6 ((t (:foreground ,color-6))))
  ;;  `(org-level-7 ((t (:foreground ,color-7))))
  ;;  `(org-level-8 ((t (:foreground ,color-8)))))

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
  (add-hook 'after-save-hook #'after-save-hook--generate-init-emacs-elisp-file :append)

  (defun org-insert-heading--fix-newline-bug (orig-fun &rest args)
    "Fix extra newline bug in org."
    ;; make sure empty lines above new headline are not removed
    (if (= (point) (line-beginning-position))
        (let ((start (point)))
          (apply orig-fun args)
          (forward-line 0)
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

  ;;----------------------------------------------------------------------------
  ;; define some deprecated functions that are needed
  ;;----------------------------------------------------------------------------

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
;; Configuration:1 ends here

;; [[file:init-emacs.org::#org-mode-outline][Outline:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Outline
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Outline")

(use-package outline
  :straight (:type built-in)
  :after (org)
  :commands (outline-up-heading
             outline-forward-same-level
             outline-show-subtree)
  :config
  ;; ;; faces
  ;; (custom-set-faces
  ;;  `(outline-1 ((t (:foreground ,color-1))))
  ;;  `(outline-2 ((t (:foreground ,color-2))))
  ;;  `(outline-3 ((t (:foreground ,color-3))))
  ;;  `(outline-4 ((t (:foreground ,color-4))))
  ;;  `(outline-5 ((t (:foreground ,color-5))))
  ;;  `(outline-6 ((t (:foreground ,color-6))))
  ;;  `(outline-7 ((t (:foreground ,color-7))))
  ;;  `(outline-8 ((t (:foreground ,color-8)))))

  ;; advise `outline-up-heading' to suppress errors
  (advice-add 'outline-up-heading :around #'advice--ignore-interactive-errors))
;; Outline:1 ends here

;; [[file:init-emacs.org::#org-mode-agenda][Agenda:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Agenda
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Agenda")

(use-package org-agenda
  :when (file-exists-p org-directory)
  :straight (:type built-in)
  :after (org)
  :config
  ;; agenda key bindings
  ;;(define-key org-agenda-mode-map (kbd "C-n") 'next-line)
  ;;(define-key org-agenda-keymap (kbd "C-n") 'next-line)
  ;;(define-key org-agenda-mode-map (kbd "C-p") 'previous-line)
  ;;(define-key org-agenda-keymap (kbd "C-p") 'previous-line)

  ;; record time when todo's are marked done
  (setq org-log-done 'time)

  ;; agenda files
  (setq org-agenda-file-regexp "agenda-.*\\.org\\'"
        org-agenda-files (mapcar (lambda (x) (expand-file-name x (file-name-as-directory org-directory)))
                                 (cl-remove-if-not (lambda (x) (string-match org-agenda-file-regexp x))
                                                   (directory-files org-directory))))

  ;; default notes file
  (setq org-default-notes-file (car org-agenda-files))

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

;; [[file:init-emacs.org::#org-mode-capture][Capture:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Capture
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Capture")

(use-package org-capture
  :when (file-exists-p org-directory)
  :straight (:type built-in)
  :after (org
          org-agenda)
  :config
  (let ((capture-file (car org-agenda-files))
        (capture-headline "Inbox"))
    (setq org-capture-templates
          `(("i" "Inbox" entry
             (file+headline ,capture-file ,capture-headline)
             "* TODO %?\nOPENED: %U"
             :prepend t)
            ("@" "Inbox [mu4e]" entry
             (file+headline ,capture-file ,capture-headline)
             "* TODO Email: \"%a\" %?\nOPENED: %U"
             :prepend t)))))
;; Capture:1 ends here

;; [[file:init-emacs.org::#org-mode-appear][Appear:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Appear
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Appear")

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-delay 0.5)
  (org-appear-trigger #'always))
;; Appear:1 ends here

;; [[file:init-emacs.org::#org-mode-latex][LaTeX:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: LaTeX
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: LaTeX")
;; LaTeX:1 ends here

;; [[file:init-emacs.org::#org-mode-latex-ox-latex][ox-latex:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: LaTeX: ox-latex
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: LaTeX: ox-latex")

(use-package ox-latex
  :straight (:type built-in)
  :custom
  (org-latex-listings t)
  :init
  (add-to-list 'org-latex-classes
               `("org-latex-plain"
                 ,(concat
                  "\\documentclass{article}\n"
                  "[NO-DEFAULT-PACKAGES]\n"
                  "[PACKAGES]\n"
                  "[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; ox-latex:1 ends here

;; [[file:init-emacs.org::#org-mode-latex-ob-latex-as-png][ob-latex-as-png:2]]
;;------------------------------------------------------------------------------
;;;; Org Mode: LaTeX: ob-latex-as-png
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: LaTeX: ob-latex-as-png")

(use-package ob-latex-as-png
  :straight t)
;; ob-latex-as-png:2 ends here

;; [[file:init-emacs.org::#org-mode-modules][Modules:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Modules
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Modules")

(org-load-modules-maybe t)
;; Modules:1 ends here

;; [[file:init-emacs.org::#org-mode-functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-get-property-list][org-get-property-list:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-get-element-tree][org-get-element-tree:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-get-file-data][org-get-file-data:1]]
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
  (let* ((property-headline-regexp "^[ \t]*\\** Org\\([ \t]*:noexport:\\)?$")
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

;; [[file:init-emacs.org::#org-mode-functions-org-get-buffer-data][org-get-buffer-data:1]]
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
  (let* ((property-headline-regexp "^[ \t]*\\** Org\\([ \t]*:noexport:\\)?$")
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
         ;; add headlines
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

;; [[file:init-emacs.org::#org-mode-functions-org-get-buffer-tags-statistics][org-get-buffer-tags-statistics:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-get-buffer-tags-statistics
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-get-buffer-tags-statistics")

(defun org-get-buffer-tags-statistics (&optional files)
  "Return an alist of tags and counts for all tags in FILES.

FILES can be nil, a single file, or a list of files.
If FILES is nil, the current buffer is used instead."
  (let ((files
         (mapcar #'file-truename
                 (cond
                  ((not files) (list (buffer-file-name)))
                  ((stringp files) (list files))
                  ((listp files) files)
                  (t (user-error "Invalid value for FILES: %S" files)))))
        (loaded-files
         (cl-remove-if #'null (mapcar #'buffer-file-name (buffer-list))))
        stats)
    (save-mark-and-excursion
      (dolist (file files)
        (find-file file)
        (org-with-point-at 1
          (let (tags)
            (while (re-search-forward org-tag-line-re nil t)
              (push (split-string (match-string-no-properties 2) ":") tags))
            (dolist (tag (flatten-list tags))
              (let ((node (assoc tag stats)))
                (if node
                    (setcdr node (1+ (cdr node)))
                  (push (cons tag 1) stats))))))
        (unless (member file loaded-files)
          (kill-buffer))))
    (sort stats (lambda (a b) (< (cdr a) (cdr b))))))
;; org-get-buffer-tags-statistics:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-safe-meta][org-safe-meta:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-sort-multi][org-sort-multi:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-sort-multi
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-sort-multi")

(defun org-sort-multi (sort-types)
  "Multiple sorts on a certain level of an outline tree, list items, or plan text.

SORT-TYPES is a list where each entry is either a character or a
list of parmeters to be passed to `org-sort-entries'.

If COMPARE-FUNC is provided, but GETKEY-FUNC is nil, then the
header string will be used.

Example: To sort first by TODO status, then by priority, then by
date, then alphabetically (case-sensitive) use the following
call:

  (org-sort-multi '(?o ?p ?t (t ?a))

Example: To sort using `string<' use the following call:

  (org-sort-multi '((nil ?f nil #'string<)))"
  (save-mark-and-excursion
    (forward-line 0)
    (let ((type (car (org-element-at-point))))
      (dolist (x (nreverse sort-types))
        (when (characterp x)
          (setq x (list nil x)))
        (when (and (listp x)
                   (> (length x) 3)
                   (not (nth 2 x)))
          (setq x `(,(car x)
                    ,(cadr x)
                    (lambda ()
                      (replace-regexp-in-string
                       "^[\*]*[ \t]*" ""
                       (buffer-substring-no-properties
                        (point-at-bol) (point-at-eol))))
                    (lambda (a b) (funcall ,(cadddr x) a b))
                    ,@(cddddr x))))
        (cl-case type
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
               (ignore-errors
                 (apply #'org-sort-entries x))
               (goto-char end)
               (when (eobp)
                 (forward-line -1))
               (when (looking-at "^\\s-*$")
                 (delete-line))
               (goto-char beg)
               (dotimes (_ 2)
                 (org-cycle)))))
          ('paragraph
           (let* ((plist (cadr (org-element-at-point)))
                  (beg (plist-get plist :contents-begin))
                  (end (plist-get plist :contents-end)))
             (sort-lines nil beg end)))
          (t
           (ignore-errors
             (apply #'org-sort-list x))))))))
;; org-sort-multi:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-sort-current][org-sort-current:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-sort-current
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-sort-current")

(defun org-sort-current (&optional sort-types)
  "Sort the current org level.

SORT-TYPES is a list where each entry is either a character or a
list of parmeters to be passed to `org-sort-entries'.

If COMPARE-FUNC is provided, but GETKEY-FUNC is nil, then the
header string will be used. Entries are applied in back to front
order.

If entry at point has TODO and PRIORITY tags, then default
SORT-TYPE is \"?o ?p ?t (nil ?f nil #'string<)\" which is to sort
by TODO status, then by priority, then by timestamp, and finally
by ASCII code. Otherwise, default SORT-TYPE is \"(nil ?f nil
#'string<)\" which is to sort by ASCII code."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let ((sort-types (or sort-types
                          (if (and (eq (car (org-element-at-point)) 'headline)
                                   (org-entry-get nil "TODO")
                                   (org-entry-get nil "PRIORITY"))
                              '(?o ?p ?t (nil ?f nil #'string<))
                            '((nil ?f nil #'string<))))))
      (org-sort-multi sort-types))))
;; org-sort-current:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-fill-element-adapt-indentation][org-fill-element--adapt-indentation:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-copy-to-clipboard][org-copy-to-clipboard:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-fix-custom-ids][org-fix-custom-ids:1]]
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
            (forward-line 0)
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
                (forward-line 0)
                (forward-line 1)))))))))
;; org-fix-custom-ids:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-update-last-modified-property][org-update-last-modified-property:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-export-to-json][org-export-to-json:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-toggle-headline-checkbox][org-toggle-headline-checkbox:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-toggle-headline-checkbox
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-toggle-headline-checkbox")

(defun org-toggle-headline-checkbox (&optional beg end)
  "Toggle between an Org headline and checkbox on current line or region."
  (interactive)
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-at-bol))))
        (end (or end (if (use-region-p) (region-end) (point-at-eol)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (not (eobp))
            (forward-line 0)
            (if (re-search-forward "^\\*+ " (line-end-position) :noerror)
                (replace-match (concat
                                (make-string (- (point) (line-beginning-position) 2) ? )
                                "- [ ] "))
              (forward-line 0)
              (when (re-search-forward "^[ \t]*- \\[[ X]\\] " (line-end-position) :noerror)
                (replace-match (concat
                                (make-string (- (point) (line-beginning-position) 5) ?*)
                                " "))))
            (forward-line 1)))))))
;; org-toggle-headline-checkbox:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-table-remove-commas][org-table-remove-commas:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-days-between-dates][org-days-between-dates:1]]
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

;; [[file:init-emacs.org::#org-mode-functions-org-babel-tangle-block][org-babel-tangle-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-babel-tangle-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-babel-tangle-block")

(defun org-babel-tangle-block ()
  "Tangle blocks for the tangle file of the block at point."
  (interactive)
  (org-babel-tangle '(16)))
;; org-babel-tangle-block:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-babel-tangle-file-async][org-babel-tangle-file-async:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-babel-tangle-file-async
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-babel-tangle-file-async")

(defun org-babel-tangle-file-async (file &optional target-file lang-re)
  "Asynchronous version of `org-babel-tangle-file'."
  (interactive)
  (let* ((file-hash (secure-hash 'md5 file))
         (lock-file (expand-file-name
                     (concat "emacs-tangle-file-async-lock-file-" file-hash)
                     temporary-file-directory))
         (run-file (expand-file-name
                    (concat "emacs-tangle-file-async-run-file-" file-hash)
                    temporary-file-directory)))
    (if (file-exists-p lock-file)
        (progn
          (message "Tangle running: %s" file)
          (when (not (file-exists-p run-file))
            (make-empty-file run-file)))
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
           (delete-file ,lock-file)))))))

;; delete any existing lock/run files in case they were not cleaned up
(mapc (lambda (x)
        (mapc (lambda (f)
                (delete-file f))
              x))
      (mapcar (lambda (x)
                (file-expand-wildcards
                 (expand-file-name x temporary-file-directory)
                 :full))
              '("emacs-tangle-file-async-lock-file-*"
                "emacs-tangle-file-async-run-file-*")))
;; org-babel-tangle-file-async:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-copy-tangled-sections][org-copy-tangled-sections:1]]
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
        (forward-line 0)
        (forward-line -1)
        (unless (looking-at "^[ \t]*;;--")
          (forward-line 1))
        (let ((beg (point))
              (end (progn
                     (re-search-forward (concat "^[ \t]*;; .* ends here$"))
                     (forward-line 0)
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
      (forward-line 0)
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

;; [[file:init-emacs.org::#org-mode-functions-org-screenshot][org-screenshot:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-screenshot
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-screenshot")

;; take a screenshot and insert org link
(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; (let ((file (concat
  ;;              (make-temp-name
  ;;               (concat (buffer-file-name)
  ;;                       "_"
  ;;                       (format-time-string "%Y%m%d_%H%M%S_")))
  ;;              ".png")))
  (let ((file (concat (buffer-file-name)
                      "_"
                      (format-time-string "%Y%m%d_%H%M%S")
                      ".png")))
    (call-process "import" nil nil nil file)
    (insert (concat "[[" file "]]\n"))))
;; org-screenshot:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-convert-headings-from-odd-indented-to-oddeven-unindented][org-convert-headings-from-odd-indented-to-oddeven-unindented:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-convert-headings-from-odd-indented-to-oddeven-unindented
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-convert-headings-from-odd-indented-to-oddeven-unindented")

(defun org-convert-headings-from-odd-indented-to-oddeven-unindented (&optional buffer)
  "Convert Org BUFFER from having only odd heading levels and
indented body data (`org-odd-levels-only' and
`org-adapt-indentation') to having odd and even heading levels
and non-indented body data (`org-indent-mode').

If BUFFER is nil, current buffer is used."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (with-current-buffer (or buffer (current-buffer))
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) " nil :noerror)
        (when (evenp (length (match-string 1)))
          (user-error "Even headings found")))
      (goto-char (point-min))
      (let ((spaces 0))
        (while (not (eobp))
          (cond
           ((looking-at "^\\(\\*+\\) ")
            (let ((level (/ (1+ (length (match-string 1))) 2)))
              (replace-match (make-string level ?*) nil nil nil 1)
              (setq spaces (* level 2))))
           ((looking-at (concat "^" (make-string spaces ? ) "\\*"))
            (replace-match ",*"))
           ((looking-at (concat "^" (make-string spaces ? )))
            (replace-match "")))
          (forward-line 1))))))
;; org-convert-headings-from-odd-indented-to-oddeven-unindented:1 ends here

;; [[file:init-emacs.org::#org-mode-functions-org-convert-headings-from-oddeven-unindented-to-odd-indented][org-convert-headings-from-oddeven-unindented-to-odd-indented:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Functions: org-convert-headings-from-oddeven-unindented-to-odd-indented
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Functions: org-convert-headings-from-oddeven-unindented-to-odd-indented")

(defun org-convert-headings-from-oddeven-unindented-to-odd-indented (&optional buffer)
  "Convert Org BUFFER from having odd and even heading levels and
  non-indented body data (`org-indent-mode') to having only odd
  heading levels and intended body data (`org-odd-levels-only'
  and `org-adapt-indentation').

  If BUFFER is nil, current buffer is used."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (with-current-buffer (or buffer (current-buffer))
    (save-mark-and-excursion
      (goto-char (point-min))
      (let (even subheading)
        (while (re-search-forward "^\\(\\*+\\) " nil :noerror)
          (when (evenp (length (match-string 1)))
            (setq even t))
          (when (> (length (match-string 1)) 2)
            (setq subheading t)))
        (when (and subheading (not even))
          (user-error "Only odd headings found")))
      (goto-char (point-min))
      (let ((spaces 0))
        (while (not (eobp))
          (cond
           ((looking-at "^\\(\\*+\\) ")
            (let ((level (length (match-string 1))))
              (replace-match (make-string (1- (* level 2)) ?*) nil nil nil 1)
              (setq spaces (* level 2))))
           ((looking-at "^,\\*")
            (replace-match (concat (make-string spaces ? ) "*")))
           ((not (looking-at "^$"))
            (insert (make-string spaces ? ))))
          (forward-line 1))))))
;; org-convert-headings-from-oddeven-unindented-to-odd-indented:1 ends here

;; [[file:init-emacs.org::#org-mode-hook][Hook:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Hook
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Hook")

(defun custom-org-mode-hook ()
  "Custom `org-mode' hook."
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
             ("C-c p" . org-priority)                                   ; "C-c ," gets overridden by `semantic-complete-analyze-inline'
             ("C-c s" . org-sort-current)                               ; sort current level
             ("C-c z" . org-agenda-archive-done-tasks)                  ; archive done tasks
             ;;("C-c C-j" . counsel-org-goto)                           ; default: `org-goto'
             ("C-c C-j" . consult-org-heading)                          ; default: `org-goto'
             ;;("C-c C-z" . switch-to-lisp)                             ; default: `org-add-note'
             ("C-c C-z" . geiser-mode-switch-to-repl)                   ; default: `org-add-note'
             ("C-c C-x C-l" . org-toggle-link-display)                  ; toggle showing or hiding links
             ("C-c C-x t" . org-toggle-headline-checkbox)               ; toggle between headline and checkbox
             ("C-c C-x T" . org-toggle-literate-programming-code-block) ; toggle literate programming code block on/off
             ("C-c C-x F" . org-fix-literate-programming-heading)       ; fix literate programming heading of current org section
             ("C-c C-v q" . org-babel-tangle-block)                     ; tangle current source block
             ("C-c C-v C-q" . org-babel-tangle-block))                  ; tangle current source block
  ;; custom movement keys
  (custom-key-bindings-movement-keys org-mode-map)

  ;; ;; work functions
  ;; (when (fboundp 'work-linkify-jira-card)
  ;;   (bind-keys :map org-mode-map ("C-c C-x L" . work-linkify-jira-card)))

  ;; make sure tabs are not inserted
  (setq indent-tabs-mode nil)

  ;; turn off auto-fill
  (turn-off-auto-fill)

  ;; turn off auto-save
  (auto-save-mode nil)

  ;; turn off flyspell
  ;; (when (fboundp 'flyspell-mode-off)
  ;;   (flyspell-mode-off))
  )

(use-package org
  :straight (:type built-in)
  :hook (org-mode . custom-org-mode-hook))
;; Hook:1 ends here

;; [[file:init-emacs.org::#org-mode-babel][Babel:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Babel
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Babel")
;; Babel:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Configuration
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Configuration")

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
;; do not set `org-src-fontify-natively' to true as this breaks org-table-align->font-lock-fontify-region
;; `org-src-fontify-natively' also creates temp files that are not always cleaned up
(setq org-use-property-inheritance t
      org-babel-use-quick-and-dirty-noweb-expansion t
      org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      org-src-fontify-natively t
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
;; Configuration:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-structure-templates][Structure Templates:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Structure Templates
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Structure Templates")

(use-package org-tempo
  :straight (:type built-in)
  :custom
  ;; set keyword completion elements
  (org-tempo-keywords-alist
   '(("A" . "ascii")
     ;;("c" . "call")
     ("H" . "html")
     ("i" . "index")
     ("L" . "latex")
     ("n" . "name")))
  ;; set block types
  (org-structure-template-alist
   '(("c" . "center")
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
     ("sl" . "src latex")
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
    "Add #+INCLUDE: and a file name."
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
  (org-tempo-add-templates)

  ;; disable `org-src-tab-acts-natively' during template indentation
  (defun tempo-insert--disable-org-src-tab-acts-natively (orig-fun &rest args)
    "Disable `org-src-tab-acts-natively' during template indentation."
    (let ((org-src-tab-acts-natively nil))
      (apply orig-fun args)))
  ;; advise `tempo-insert'
  (advice-add 'tempo-insert :around #'tempo-insert--disable-org-src-tab-acts-natively))

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

;; [[file:init-emacs.org::#org-mode-babel-edit-source][Edit Source:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-tangle-case-sensitive][Tangle Case-Sensitive:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-tangle-update-timestamps][Tangle Update Timestamps:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-tangle-delete-trailing-whitespace][Tangle Delete Trailing Whitespace:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-tangle-generate-pdf-from-tex][Tangle Generate PDF from TEX:1]]
;;------------------------------------------------------------------------------
  ;;;; Org Mode: Babel: Tangle Generate PDF from TEX
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Tangle Generate PDF from TEX")

(defun org-babel-post-tangle-hook--generate-pdf-from-tex ()
  "Generate PDF from tangled TEX file."
  (let ((case-fold-search t)
        (filename (expand-file-name (buffer-file-name)))
        (output "*PDFLaTeX Output*"))
    (when (string= (file-name-extension filename) "tex")
      (shell-command
       (concat "pdflatex -output-directory=" temporary-file-directory
               " -halt-on-error \"" filename "\" && "
               "cp \"" temporary-file-directory
               (file-name-sans-extension
                (file-name-nondirectory filename)) ".pdf" "\" "
               "\"" (file-name-sans-extension filename) ".pdf" "\"")
       output))))
(add-hook 'org-babel-post-tangle-hook #'org-babel-post-tangle-hook--generate-pdf-from-tex)
;; Tangle Generate PDF from TEX:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-racket][Racket:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-java][Java:1]]
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
                            (user-error ":classname parameter is required")))
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

;; [[file:init-emacs.org::#org-mode-babel-kotlin][Kotlin:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-python][Python:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-rust][Rust:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-v][V:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-basic-commander-x16][Basic (Commander X16):1]]
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

;; [[file:init-emacs.org::#org-mode-babel-assembly-language-commander-x16][Assembly Language (Commander X16):1]]
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

;; [[file:init-emacs.org::#org-mode-babel-plantuml][PlantUML:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: PlantUML
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: PlantUML")

;; set plantuml.jar location
(setq org-plantuml-jar-path "~/dev/java/lib/plantuml.jar")
;; PlantUML:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-load-languages][Load Languages:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel: Load Languages
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel: Load Languages")

;; babel languages (usable in source blocks)
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (clojure . t)
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
                             (latex . t)
                             (lilypond . t)
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

;; [[file:init-emacs.org::#org-mode-babel-functions][Babel Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Babel Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Babel Functions")
;; Babel Functions:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-functions-org-generate-custom-id-from-title][org-generate-custom-id-from-title:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-functions-org-fix-literate-programming-heading][org-fix-literate-programming-heading:1]]
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
         (forward-line 0)
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
                                       comment-2 "+ .*\n"))
                      (replace-match
                       (concat
                        comment-2 (make-string 78 (if (> level 1) ?- ?=)) "\n"
                        (if (> level 2) comment-4 comment-3) " " title "\n"))
                      (forward-line 0)
                      (while (looking-at comment-2)
                        (when (looking-at (concat comment-2 "[=-]+\n"))
                          (replace-match
                           (concat comment-2 (make-string 78 (if (> level 1) ?- ?=)) "\n")))
                        (forward-line 1))))
                  ;; set `init-message' text
                  (when (re-search-forward "(init-message .*$" nil :noerror)
                    (replace-match (concat "(init-message " (number-to-string level) " \"" title "\")")))))))))))))
;; org-fix-literate-programming-heading:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-functions-org-fix-literate-programming-heading-region][org-fix-literate-programming-heading-region:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-fix-literate-programming-heading-region
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-fix-literate-programming-heading-region")

(defun org-fix-literate-programming-heading-region (&optional beg end)
  "Fix 'literate programming' headings contained with given region.

Reset the CUSTOM_ID property, title comment, and `init-message'."
  (interactive "*")
  (let ((case-fold-search t)
        (beg (or beg (if (use-region-p)
                         (region-beginning)
                       (progn (beginning-of-line-text) (point)))))
        (end (or end (if (use-region-p)
                         (region-end)
                       (line-end-position)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (org-with-wide-buffer
           (narrow-to-region beg end)
           (goto-char (point-min))
           (while (and (re-search-forward "^[ \t]*:CUSTOM_ID:" nil :noerror)
                       (< (line-end-position) end))
             (org-fix-literate-programming-heading)
             (forward-line 1))))))))
;; org-fix-literate-programming-heading-region:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-functions-org-toggle-literate-programming-code-block][org-toggle-literate-programming-code-block:1]]
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
                 (dotimes (_ 2)
                   (when (re-search-forward "\+" (line-end-position))
                     (replace-match "")))
               (progn
                 (insert "+")
                 (goto-char (line-end-position))
                 (insert "+"))))))))))
;; org-toggle-literate-programming-code-block:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-functions-org-insert-literate-programming-statics][org-insert-literate-programming-statics:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-functions-org-insert-literate-programming-block][org-insert-literate-programming-block:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Babel Functions: org-insert-literate-programming-block
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Babel Functions: org-insert-literate-programming-block")

(defun org-insert-literate-programming-block (&optional title)
  "Insert 'literate programming' block consisting of a heading,
properties, and source block."
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
    (insert "#+BEGIN_SRC conf-unix\n")
    (insert "#+END_SRC\n")
    (indent-region point (point))
    (org-previous-visible-heading 1)
    (goto-char (line-end-position))
    (org-fix-literate-programming-heading)
    (forward-line 12)))
;; org-insert-literate-programming-block:1 ends here

;; [[file:init-emacs.org::#org-mode-babel-functions-org-insert-literate-programming-init-emacs-block][org-insert-literate-programming-init-emacs-block:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-functions-org-insert-literate-programming-code-block][org-insert-literate-programming-code-block:1]]
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

;; [[file:init-emacs.org::#org-mode-babel-functions-org-insert-literate-programming-project-euler-problem-block][org-insert-literate-programming-project-euler-problem-block:1]]
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

;; [[file:init-emacs.org::#org-mode-visibility][Visibility:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Visibility
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Visibility")

(use-package org-visibility
  :straight t
  ;;:load-path (lambda () (file-truename (expand-file-name "~/code/github-nullman/emacs-org-visibility")))
  :after (org)
  :demand t
  :bind* (:map org-visibility-mode-map
               ("C-x C-v" . org-visibility-force-save) ; default: `find-alternative-file'
               ("C-x M-v" . org-visibility-remove))    ; default: undefined
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
                                  ,(file-truename "~/org/test")))
  :init
  (org-visibility-mode 1))
;; Visibility:1 ends here

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-guid][org-bookmarks-guid:1]]
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

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-timestamp][org-bookmarks-timestamp:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-timestamp
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-timestamp")

(defun org-bookmarks-timestamp ()
  "Return time since the epoch in microseconds."
  (floor (* (float-time (current-time)) 1000000)))
;; org-bookmarks-timestamp:1 ends here

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-parse][org-bookmarks-parse:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Bookmarks: org-bookmarks-parse
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Bookmarks: org-bookmarks-parse")

(defun org-bookmarks-parse (file)
  "Return a tree structure representing the org folders and
bookmarks found in FILE.

Example input:

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
     (:type \"folder\" :title \"Folder 3\")]))"
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

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-export-to-json][org-bookmarks-export-to-json:1]]
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
                 (cl-incf global-id))
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
                      (setq entry (append entry (list :children (map 'vector (lambda (x) (parse x tree (cl-incf idx))) children))))))
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

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-export-to-html][org-bookmarks-export-to-html:1]]
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

;; [[file:init-emacs.org::#org-mode-bookmarks-org-bookmarks-export-to-nyxt][org-bookmarks-export-to-nyxt:1]]
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

;; [[file:init-emacs.org::#org-mode-finances][Finances:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Finances
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Finances")
;; Finances:1 ends here

;; [[file:init-emacs.org::#org-mode-finances-export-taxes][export-taxes:1]]
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
                     (forward-line 0)
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
      (forward-line 0)
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

;; [[file:init-emacs.org::#org-mode-finances-nwm-add-monthly-account-data][nwm-add-monthly-account-data:1]]
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
    (let ((buffer (get-buffer "personal-encrypted.org.gpg"))
          (tables '("NWM_A40_344433"
                    "NWM_A40_344458"
                    "NWM_A40_345190"
                    "NWM_B40_300756"
                    "NWM_B40_300798"
                    "NWM_PX1_012685"
                    "NWM_PX1_012692"))
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

;; [[file:init-emacs.org::#org-mode-magic-the-gathering][Magic the Gathering:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Magic the Gathering
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Magic the Gathering")
;; Magic the Gathering:1 ends here

;; [[file:init-emacs.org::#org-mode-magic-the-gathering-mtg-cards-owned-file-name][mtg-cards-owned-file-name:1]]
(defconst mtg-cards-owned-file-name (file-truename (expand-file-name "~/org/magic-the-gathering-cards-owned.org")))
;; mtg-cards-owned-file-name:1 ends here

;; [[file:init-emacs.org::#org-mode-magic-the-gathering-mtg-card-list][mtg-card-list:1]]
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
    (forward-line 0)
    (kill-region (point-min) (point))
    (unless (search-forward "* mtg card sets" nil :noerror)
      (goto-char (point-max)))
    (forward-line 0)
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

;; [[file:init-emacs.org::#org-mode-magic-the-gathering-mtg-deck-search][mtg-deck-search:1]]
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

;; [[file:init-emacs.org::#org-mode-magic-the-gathering-mtg-set-to-table][mtg-set-to-table:1]]
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

;; [[file:init-emacs.org::#org-mode-mechwarrior-online][MechWarrior Online:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: MechWarrior Online
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: MechWarrior Online")
;; MechWarrior Online:1 ends here

;; [[file:init-emacs.org::#org-mode-mechwarrior-online-mwo-export-mech][mwo-export-mech:1]]
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

;; [[file:init-emacs.org::#org-mode-dungeons-and-dragons-online][Dungeons and Dragons Online:1]]
;;------------------------------------------------------------------------------
;;; Org Mode: Dungeons and Dragons Online
;;------------------------------------------------------------------------------

(init-message 2 "Org Mode: Dungeons and Dragons Online")
;; Dungeons and Dragons Online:1 ends here

;; [[file:init-emacs.org::#org-mode-dungeons-and-dragons-online-ddo-get-item-info][ddo-get-item-info:1]]
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

;; [[file:init-emacs.org::#org-mode-dungeons-and-dragons-online-ddo-fix-wiki-description][ddo-fix-wiki-description:1]]
;;------------------------------------------------------------------------------
;;;; Org Mode: Dungeons and Dragons Online: ddo-fix-wiki-description
;;------------------------------------------------------------------------------

(init-message 3 "Org Mode: Dungeons and Dragons Online: ddo-fix-wiki-description")

(defun ddo-fix-wiki-description ()
  "Fix wiki description around point."
  (interactive)
  (forward-line 0)
  (while (not (looking-at "\\*"))
    (insert ", ")
    (forward-line 0)
    (delete-char -1)
    (forward-line 0))
  (while (re-search-forward "Icon tooltip\.png" (line-end-position) :noerror)
    (replace-match ""))
  (forward-line 0)
  (while (re-search-forward " +," (line-end-position) :noerror)
    (replace-match ","))
  (forward-line 0)
  (while (re-search-forward ",," (line-end-position) :noerror)
    (replace-match ","))
  (forward-line 0)
  (while (re-search-forward "  +" (line-end-position) :noerror)
    (replace-match " "))
  (forward-line 0)
  (while (re-search-forward " +$" (line-end-position) :noerror)
    (replace-match ""))
  (goto-char (line-end-position))
  (insert ")"))
;; ddo-fix-wiki-description:1 ends here

;; [[file:init-emacs.org::#org-website][Org Website:1]]
;;==============================================================================
;;; Org Website
;;==============================================================================

(init-message 1 "Org Website")
;; Org Website:1 ends here

;; [[file:init-emacs.org::#org-website-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Configuration")
;; Configuration:1 ends here

;; [[file:init-emacs.org::#org-website-configuration-publish-configuration][Publish Configuration:1]]
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

;; [[file:init-emacs.org::#org-website-configuration-menu-lists][Menu Lists:1]]
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

;; [[file:init-emacs.org::#org-website-configuration-gopher-configuration][Gopher Configuration:1]]
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

;; [[file:init-emacs.org::#org-website-functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::#org-website-functions-get-property-list][Get Property List:1]]
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

;; [[file:init-emacs.org::#org-website-functions-get-property-element][Get Property Element:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Property Element
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Property Element")

(defun org-website-get-property-element (property-list element)
  "Return ELEMENT from PROPERTY-LIST returned from `org-website-get-property-list', or empty string if element was not found."
  (or (cdr (assoc element property-list)) ""))
;; Get Property Element:1 ends here

;; [[file:init-emacs.org::#org-website-functions-get-url][Get URL:1]]
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

;; [[file:init-emacs.org::#org-website-functions-blog-url][Blog URL:1]]
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

;; [[file:init-emacs.org::#org-website-functions-is-blog-post][Is Blog Post:1]]
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

;; [[file:init-emacs.org::#org-website-functions-get-level][Get Level:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Functions: Get Level
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Functions: Get Level")

(defun org-website-get-level (property-list)
  "Return level of current project file."
  (- (length (split-string (org-website-get-url property-list) "/")) 3))
;; Get Level:1 ends here

;; [[file:init-emacs.org::#org-website-functions-format-headline][Format Headline:1]]
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

;; [[file:init-emacs.org::#org-website-functions-get-gopher-selector-hostname-port][Get Gopher Selector Hostname Port:1]]
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

;; [[file:init-emacs.org::#org-website-functions-convert-url-to-gopher-selector-hostname-port][Convert URL to Gopher Selector Hostname Port:1]]
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

;; [[file:init-emacs.org::#org-website-functions-gopher-justify-lines][Gopher Justify Lines:1]]
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
            (forward-line 0)
            (forward-line 1))))
      ;; return justified string
      (buffer-string))))
;; Gopher Justify Lines:1 ends here

;; [[file:init-emacs.org::#org-website-publish-html][Publish HTML:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish HTML
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish HTML")
;; Publish HTML:1 ends here

;; [[file:init-emacs.org::#org-website-publish-html-derived-backend][Derived Backend:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-publish-to-html][Publish to HTML:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-template][Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-blog-contents-template][Blog Contents Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-inner-template][Inner Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-headline][Headline:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-section][Section:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-footnote-reference][Footnote Reference:1]]
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

;; [[file:init-emacs.org::#org-website-publish-html-footnote-section][Footnote Section:1]]
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

;; [[file:init-emacs.org::#org-website-publish-rss][Publish RSS:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish RSS
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish RSS")
;; Publish RSS:1 ends here

;; [[file:init-emacs.org::#org-website-publish-rss-derived-backend][Derived Backend:1]]
;;------------------------------------------------------------------------------
;;;; Org Website: Publish RSS: Derived Backend
;;------------------------------------------------------------------------------

(init-message 3 "Org Website: Publish RSS: Derived Backend")

;; create derived backend with customizations to html backend
(org-export-define-derived-backend 'org-website-rss 'html
  :translate-alist '((template . org-website-rss-template)
                     (inner-template . org-website-rss-inner-template)))
;; Derived Backend:1 ends here

;; [[file:init-emacs.org::#org-website-publish-rss-publish-to-rss][Publish to RSS:1]]
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

;; [[file:init-emacs.org::#org-website-publish-rss-template][Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-rss-inner-template][Inner Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher][Publish Gopher:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Publish Gopher
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Publish Gopher")
;; Publish Gopher:1 ends here

;; [[file:init-emacs.org::#org-website-publish-gopher-derived-backend][Derived Backend:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-publish-to-gopher][Publish to Gopher:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-template][Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-inner-template][Inner Template:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-headline][Headline:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-section][Section:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-footnote-reference][Footnote Reference:1]]
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

;; [[file:init-emacs.org::#org-website-publish-gopher-footnote-section][Footnote Section:1]]
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

;; [[file:init-emacs.org::#org-website-helper-functions][Helper Functions:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Helper Functions
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Helper Functions")
;; Helper Functions:1 ends here

;; [[file:init-emacs.org::#org-website-helper-functions-publish][Publish:1]]
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

;; [[file:init-emacs.org::#org-website-helper-functions-tangle-publish][Tangle Publish:1]]
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

;; [[file:init-emacs.org::#org-website-helper-functions-blog-post-create][Blog Post Create:1]]
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

;; [[file:init-emacs.org::#org-website-helper-functions-blog-post-update-posted][Blog Post Update Posted:1]]
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

;; [[file:init-emacs.org::#org-website-helper-functions-unflatten][Unflatten:1]]
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

;; [[file:init-emacs.org::#org-website-generate-website-emacs-initialization-file][Generate Website Emacs Initialization File:1]]
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
                                ";;;; Environment: Files: General"
                                ";;;; Org Mode: Functions: org-get-file-data"
                                ";;;; Org Mode: Babel: Configuration"
                                ";;;; Org Mode: Babel: Tangle Update Timestamps"
                                ";;;; Org Mode: Babel: Tangle Case-Sensitive"
                                ";;;; Functions: Emacs Functions: delete-line"
                                ";;; Packages: htmlize"
                                ";;; Packages: w3m")
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

;; [[file:init-emacs.org::#org-website-remote-synchronization][Remote Synchronization:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Remote Synchronization
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Remote Synchronization")
;; Remote Synchronization:1 ends here

;; [[file:init-emacs.org::#org-website-remote-synchronization-rsync-to-morpheus][Rsync to Morpheus:1]]
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

;; [[file:init-emacs.org::#org-website-remote-synchronization-rsync-to-digitalocean][Rsync to DigitalOcean:1]]
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

;; [[file:init-emacs.org::#org-website-deployment][Deployment:1]]
;;------------------------------------------------------------------------------
;;; Org Website: Deployment
;;------------------------------------------------------------------------------

(init-message 2 "Org Website: Deployment")
;; Deployment:1 ends here

;; [[file:init-emacs.org::#functions][Functions:1]]
;;==============================================================================
;;; Functions
;;==============================================================================

(init-message 1 "Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::#functions-initialization-functions][Initialization Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Initialization Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Initialization Functions")
;; Initialization Functions:1 ends here

;; [[file:init-emacs.org::#functions-initialization-functions-require-if-available][require-if-available:1]]
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

;; [[file:init-emacs.org::#functions-initialization-functions-load-file-if-available][load-file-if-available:1]]
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

;; [[file:init-emacs.org::#functions-initialization-functions-compile-file-if-needed][compile-file-if-needed:1]]
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

;; [[file:init-emacs.org::#functions-initialization-functions-with-eval-after-load][with-eval-after-load:1]]
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

;; [[file:init-emacs.org::#functions-initialization-functions-eval-after-load-with-byte-compile][eval-after-load-with-byte-compile:1]]
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

;; [[file:init-emacs.org::#functions-initialization-functions-safe-load][safe-load:1]]
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

;; [[file:init-emacs.org::#functions-general-functions][General Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: General Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: General Functions")
;; General Functions:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-list-to-string][list-to-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: list-to-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: list-to-string")

(defun list-to-string (list &optional delimiter)
  "Return concatenated characters in LIST using optional DELIMITER."
  (let ((delimiter (or delimiter "")))
    (mapconcat 'string list delimiter)))
;; list-to-string:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-string-to-list][string-to-list:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-join-strings][join-strings:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-file-to-string][file-to-string:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-safe-substring][safe-substring:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: safe-substring
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: safe-substring")

(defun safe-substring (string from &optional to)
  "Calls the `substring' function safely.

No errors will be returned for out of range values of FROM and
TO. Instead the entire string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        string
      (substring string from to))))
;; safe-substring:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-for-each][for-each:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-is-single][is-single:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: is-single
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: is-single")

(defun is-single (list)
  "Return true if LIST is a list of one element."
  (and (consp list) (null (cdr list))))
;; is-single:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-append-element][append-element:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: append-element
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: append-element")

(defun append-element (list elm)
  "Append ELM to end of list LIST."
  (append list (list elm)))
;; append-element:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-map-integer][map-integer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: General Functions: map-integer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: General Functions: map-integer")

(defun map-integer (fn n)
  "Call function FN once for every number from 0 to N-1."
  (let ((acc nil))
    (dotimes (x n)
      (push (funcall fn x) acc))
    (nreverse acc)))
;; map-integer:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-filter][filter:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-most][most:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-queue][queue:1]]
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
          (t (user-error "Illegal command given to queue object: %s" cmd)))))))
;; queue:1 ends here

;; [[file:init-emacs.org::#functions-general-functions-quicksort][quicksort:1]]
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

;; [[file:init-emacs.org::#functions-general-functions-hash-table-dump][hash-table-dump:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions][Emacs Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Emacs Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Emacs Functions")
;; Emacs Functions:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-inside-string][inside-string:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: inside-string
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: inside-string")

(defun inside-string ()
  "Return non-nil if point is inside a string."
  (not (not (nth 3 (syntax-ppss)))))
;; inside-string:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-inside-comment][inside-comment:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: inside-comment
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: inside-comment")

(defun inside-comment ()
  "Return non-nil if point is inside a comment."
  (nth 4 (syntax-ppss)))
;; inside-comment:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-try-finally][try-finally:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-save-buffer-always][save-buffer-always:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-save-buffer-always-maybe][save-buffer-always-maybe:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-describe-function-or-variable-at-point][describe-function-or-variable-at-point:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-mode-line-add][mode-line-add:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: mode-line-add
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: mode-line-add")

(defun mode-line-add (item)
  "Add ITEM to `global-mode-string' part of the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (add-to-list 'global-mode-string item t))
;; mode-line-add:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-insert-line-below][insert-line-below:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-insert-line-above][insert-line-above:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-move-line-down][move-line-down:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-move-line-up][move-line-up:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-kill-region-or-word][kill-region-or-word:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-kill-duplicate-lines][kill-duplicate-lines:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: kill-duplicate-lines
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: kill-duplicate-lines")

(defun kill-duplicate-lines (&optional beg end)
  "Kill duplicate lines in the pre-sorted selected region or entire buffer (if none)."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (not (eobp))
            (kill-line 1)
            (yank)
            (let ((next-line (point)))
              (while (re-search-forward (format "^%s" (regexp-quote (car kill-ring))) nil :noerror)
                (replace-match "" nil nil))
              (goto-char next-line))))))))
;; kill-duplicate-lines:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-indent-or-expand][indent-or-expand:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-swap-windows][swap-windows:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-toggle-window-split][toggle-window-split:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-window-enlarge-vertically][window-enlarge-vertically:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: window-enlarge-vertically
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: window-enlarge-vertically")

(defun window-enlarge-vertically (arg)
  "Make current window 5 lines bigger vertically."
  (interactive "P")
  (if arg
      (enlarge-window (* 5 arg))
    (enlarge-window 5)))
;; window-enlarge-vertically:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-window-shrink-vertically][window-shrink-vertically:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: window-shrink-vertically
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: window-shrink-vertically")

(defun window-shrink-vertically (arg)
  "Make current window 5 lines smaller vertically."
  (interactive "P")
  (if arg
      (shrink-window (* 5 arg))
    (shrink-window 5)))
;; window-shrink-vertically:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-window-enlarge-horizontally][window-enlarge-horizontally:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: window-enlarge-horizontally
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: window-enlarge-horizontally")

(defun window-enlarge-horizontally (arg)
  "Make current window 5 lines bigger horizontally."
  (interactive "P")
  (if arg
      (enlarge-window (* 5 arg) :horizontal)
    (enlarge-window 5 :horizontal)))
;; window-enlarge-horizontally:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-window-shrink-horizontally][window-shrink-horizontally:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: window-shrink-horizontally
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: window-shrink-horizontally")

(defun window-shrink-horizontally (arg)
  "Make current window 5 lines smaller horizontally."
  (interactive "P")
  (if arg
      (shrink-window (* 5 arg) :horizontal)
    (shrink-window 5 :horizontal)))
;; window-shrink-horizontally:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-compile-elisp][compile-elisp:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-join-next-line][join-next-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: join-next-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: join-next-line")

(defun join-next-line (arg)
  "Join next line with current one."
  (interactive "*P")
  (dotimes (_ (or arg 1))
    (join-line -1)))
;; join-next-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-sort-all-lines][sort-all-lines:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-sort-lines-removing-duplicates][sort-lines-removing-duplicates:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-delete-word][delete-word:1]]
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
    (dotimes (_ arg)
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

;; [[file:init-emacs.org::#functions-emacs-functions-backward-delete-word][backward-delete-word:1]]
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
    (dotimes (_ arg)
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

;; [[file:init-emacs.org::#functions-emacs-functions-copy-line][copy-line:1]]
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
    (forward-line 0)
    (let ((beg (point)))
      (if (eobp)
          (goto-char (line-end-position))
        (forward-line 1))
      (copy-region-as-kill beg (point)))))
;; copy-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-cut-line][cut-line:1]]
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
    (forward-line 0)
    (let ((beg (point)))
      (if (eobp)
          (goto-char (line-end-position))
        (forward-line 1))
      (kill-region beg (point)))))
;; cut-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-delete-line][delete-line:1]]
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
    (forward-line 0)
    (delete-region (point)
                   (progn
                     (forward-line 1)
                     (point)))
    (if (<= (+ (point) col) (line-end-position))
        (forward-char col)
      (goto-char (line-end-position)))))
;; delete-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-delete-to-end-of-line][delete-to-end-of-line:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-duplicate-line][duplicate-line:1]]
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
            (forward-line 0)
            (forward-sexp)
            (when (looking-at "\\()+\\)[ \t]*;")
              (replace-match (make-string (length (match-string 1)) ?\s) nil nil nil 1))
            (when (looking-at ")+")
              (replace-match ""))
            (forward-line 1)))))))
;; duplicate-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-duplicate-line-inc][duplicate-line-inc:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-yank-as-rectangle][yank-as-rectangle:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: yank-as-rectangle
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: yank-as-rectangle")

(defun yank-as-rectangle ()
  "Yank the most recently killed text as a rectangle with upper left corner at point."
  (interactive "*")
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (rectangle-mark-mode 1)
    (let ((width 0))
      (while (not (eobp))
        (end-of-line)
        (when (> (current-column) width)
          (setq width (current-column)))
        (forward-line 1))
      (forward-line 0)
      (rectangle-right-char width))
    (kill-rectangle (point-min) (point)))
  (yank-rectangle))
;; yank-as-rectangle:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-display-line-numbers-type-toggle][display-line-numbers-type-toggle:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-goto-line-enhanced][goto-line-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-forward-sexp-enhanced][forward-sexp-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-backward-sexp-enhanced][backward-sexp-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-scroll-up-enhanced][scroll-up-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-scroll-down-enhanced][scroll-down-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-scroll-up-command-enhanced][scroll-up-command-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-scroll-down-command-enhanced][scroll-down-command-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-downcase-region-enhanced][downcase-region-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-upcase-region-enhanced][upcase-region-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-downcase-word-enhanced][downcase-word-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-upcase-word-enhanced][upcase-word-enhanced:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-capitalize-word-enhanced][capitalize-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: capitalize-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: capitalize-word-enhanced")

(defun capitalize-word-enhanced (arg)
  "Capitalize word at point."
  (interactive "*P")
  (let ((syntax-table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "." syntax-table)
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?' "w" syntax-table)
    (with-syntax-table syntax-table
      (let ((p (point)))
        (forward-word 1)
        (when (> (point) p)
          (forward-word -1)))
      (capitalize-word (or arg 1)))))
(bind-keys* ([remap capitalize-word] . capitalize-word-enhanced))
;; capitalize-word-enhanced:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-toggle-word-case][toggle-word-case:1]]
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
      (modify-syntax-entry ?_ "." syntax-table)
      (modify-syntax-entry ?- "." syntax-table)
      (modify-syntax-entry ?' "w" syntax-table)
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
                    (capitalize-word-enhanced arg))
                   ;; uppercase -> lowercase
                   ((s-uppercase-p word)
                    (downcase-word-enhanced arg))
                   ;; other -> uppercase
                   (t
                    (upcase-word-enhanced arg))))))))))))
(bind-keys* ("M-c" . toggle-word-case))
;; toggle-word-case:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-eval-current-sexp][eval-current-sexp:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-eval-sexp-buffer][eval-sexp-buffer:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-eval-and-replace-last-sexp][eval-and-replace-last-sexp:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-eval-and-replace-current-sexp][eval-and-replace-current-sexp:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-macroexpand-and-replace][macroexpand-and-replace:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-calc-eval-and-replace-region][calc-eval-and-replace-region:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-calc-eval-and-replace-line][calc-eval-and-replace-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: calc-eval-and-replace-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: calc-eval-and-replace-line")

(defun calc-eval-and-replace-line ()
  "Evaluate line using `calc-eval' and replace it with the result."
  (interactive "*")
  (calc-eval-and-replace-region (line-beginning-position) (line-end-position)))
;; calc-eval-and-replace-line:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-indent-current-sexp][indent-current-sexp:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-indent-sexp-buffer][indent-sexp-buffer:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-comment-or-uncomment-sexp][comment-or-uncomment-sexp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: comment-or-uncomment-sexp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: comment-or-uncomment-sexp")

(defun uncomment-sexp (&optional n)
  "Uncomment an sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (point)
         (end (save-mark-and-excursion
                (save-match-data
                  (when (elt (syntax-ppss) 4)
                    (search-backward-regexp comment-start-skip
                                            (line-beginning-position)
                                            :noerror))
                  (setq point (point-marker))
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
      (goto-char point)
      ;; identify the top-level sexp inside the comment
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq point (point-marker)))
      ;; re-comment everything before it
      (ignore-errors
        (comment-region beg point))
      ;; and everything after it
      (goto-char point)
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

;; [[file:init-emacs.org::#functions-emacs-functions-rename-buffer-and-file][rename-buffer-and-file:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: rename-buffer-and-file
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: rename-buffer-and-file")

(defun rename-buffer-and-file (name &optional confirm)
  "Rename current buffer and file to NAME."
  (interactive
   (list (if buffer-file-name
             (read-file-name "Rename buffer to: " default-directory)
           (user-error "Current buffer is not visiting a file"))
         (not current-prefix-arg)))
  (or (not name) (string= name "")
      (let ((source-file buffer-file-name))
        (unless source-file
          (user-error "Current buffer is not visiting a file"))
        (write-file name confirm)
        (when (file-exists-p name)
          (delete-file source-file)))))
;; rename-buffer-and-file:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-move-buffer-and-file][move-buffer-and-file:1]]
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
           (user-error "Current buffer is not visiting a file"))
         (not current-prefix-arg)))
  (let* ((source-file buffer-file-name)
         (dir (if (or (string= dir "")
                      (not (string= (substring dir -1) "/")))
                  (concat dir "/")
                dir))
         (file (concat dir (file-name-nondirectory source-file))))
    (if (not source-file)
        (message "Buffer '%s' is not visiting a file" (buffer-name))
      (progn
        (unless (and confirm
                     (file-exists-p file)
                     (not (yes-or-no-p (format "File `%s' exists; overwrite? " file)))
                     (message "Canceled"))
          (copy-file source-file file t)
          (delete-file source-file)
          (set-visited-file-name file)
          (set-buffer-modified-p nil)
          (vc-refresh-state))))))
;; move-buffer-and-file:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-delete-buffer-and-file][delete-buffer-and-file:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: delete-buffer-and-file
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: delete-buffer-and-file")

(defun delete-buffer-and-file (&optional buffer)
  "Delete BUFFER and file associated with it.

BUFFER defaults to the current buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (if (not (and file (file-exists-p file)))
        (if (fboundp 'ido-kill-buffer)
            (ido-kill-buffer)
          (kill-buffer))
      (unless (and
               (not (yes-or-no-p (format "Are you sure you want to delete '%s'? " file)))
               (message "Canceled"))
        (delete-file file)
        (kill-buffer buffer)
        (message "File '%s' successfully deleted" file)))))
;; delete-buffer-and-file:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-expand-relative-file-name][expand-relative-file-name:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: expand-relative-file-name
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: expand-relative-file-name")

(defun expand-relative-file-name (name)
  "Expand FILE-NAME found in current directory."
  (file-truename (expand-file-name name (file-name-directory (or load-file-name buffer-file-name)))))
;; expand-relative-file-name:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-remove-trailing-blanks][remove-trailing-blanks:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: remove-trailing-blanks
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: remove-trailing-blanks")

;; mode hooks are added in their sections
(defun remove-trailing-blanks (&optional ask)
  (interactive "*")
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

;; [[file:init-emacs.org::#functions-emacs-functions-remove-tabs][remove-tabs:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: remove-tabs
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: remove-tabs")

;; mode hooks are added in their sections
(defun remove-tabs (&optional ask)
  (interactive "*")
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
                                    (:mode . 'make-mode))
  "List of mode name and file name regexp patterns to exclude
from tab removal on file save."
  :type 'list
  :group 'files)

(defun remove-tabs-with-exceptions (&optional ask)
  (let ((file (file-name-nondirectory buffer-file-name)))
    (unless
        (cl-remove-if (lambda (x)
                        (let ((type (car x))
                              (name (cdr x)))
                          (and
                           (not (and (eq type :mode)
                                     (derived-mode-p name)))
                           (not (and (eq type :file)
                                     (string-match name file))))))
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

;; [[file:init-emacs.org::#functions-emacs-functions-indent-down][indent-down:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-server-start-maybe][server-start-maybe:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-load-bookmarks][load-bookmarks:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-find-file-updir][find-file-updir:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-find-file-eof][find-file-eof:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: find-file-eof
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: find-file-eof")

(defun find-file-eof (file)
  "Run `find-file' with FILE, then move the point to the end of buffer."
  (find-file file)
  (goto-char (point-max)))
;; find-file-eof:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-mark-full-word][mark-full-word:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-term-buffer][term-buffer:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-term-ansi][term-ansi:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-pop-up-shell][pop-up-shell:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-pop-up-shell-toggle][pop-up-shell-toggle:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-switch-to-scratch][switch-to-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-scratch")

(defun switch-to-scratch ()
  "Switch to `*scratch*' buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))
;; switch-to-scratch:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-switch-to-scratch-for-current-mode][switch-to-scratch-for-current-mode:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-scratch-for-current-mode
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-scratch-for-current-mode")

(defun switch-to-scratch-for-current-mode ()
  "Switch to `*scratch-MODE*' buffer, creating it if needed."
  (interactive)
  (let* ((mode major-mode)
         (buffer (concat "*scratch-" (symbol-name mode) "*")))
    (switch-to-buffer buffer)
    (funcall mode)))
;; switch-to-scratch-for-current-mode:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-new-scratch][new-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: new-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: new-scratch")

(defun new-scratch ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*")))
;; new-scratch:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-new-emacs-lisp-scratch][new-emacs-lisp-scratch:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: new-emacs-lisp-scratch
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: new-emacs-lisp-scratch")

(defun new-emacs-lisp-scratch (&optional use-existing)
  "Create a new scratch buffer with `emacs-lisp-mode'.

If USE-EXISTING is non-nil, switch to an existing buffer if one
exists, otherwise create a new one."
  (interactive)
  (let ((buffer "*scratch-emacs-lisp-mode*"))
    (if (and use-existing (get-buffer buffer))
        (switch-to-buffer buffer)
      (switch-to-buffer (generate-new-buffer-name buffer))
      (emacs-lisp-mode))))
;; new-emacs-lisp-scratch:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-recreate-scratch-when-killed][recreate-scratch-when-killed:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: recreate-scratch-when-killed
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: recreate-scratch-when-killed")

(defun recreate-scratch-when-killed ()
  "Recreate scratch buffer, when it is killed.

Add the following to your init.el file for this to work:

  \(add-hook 'kill-buffer-query-functions #'recreate-scratch-when-killed)"
  (interactive)
  (let ((buffer "*scratch*"))
    (if (string= (buffer-name (current-buffer)) buffer)
        (let ((kill-buffer-query-functions kill-buffer-query-functions))
          (remove-hook 'kill-buffer-query-functions 'recreate-scratch-when-killed)
          (kill-buffer (current-buffer))
          (set-buffer (get-buffer-create buffer))
          nil)
      t)))

(add-hook 'kill-buffer-query-functions #'recreate-scratch-when-killed)
;; recreate-scratch-when-killed:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-switch-to-messages][switch-to-messages:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: switch-to-messages
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: switch-to-messages")

(defun switch-to-messages ()
  "Switch to `*Messages*' buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*Messages*"))
;; switch-to-messages:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-diff-current-buffer][diff-current-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: diff-current-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: diff-current-buffer")

(defun diff-current-buffer ()
  "Show a diff of the current buffer with its file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
;; diff-current-buffer:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-get-char-property-here][get-char-property-here:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-comments-in-buffer][comments-in-buffer:1]]
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
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (comment-search-forward (point-max) t)
            (push (buffer-substring-no-properties (point) (line-end-position)) comments))))
      (nreverse comments))))
;; comments-in-buffer:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-count-words][count-words:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-count-words-paragraph][count-words-paragraph:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-count-lines-of-code][count-lines-of-code:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: count-lines-of-code
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: count-lines-of-code")

(defun count-lines-of-code (&optional beg end)
  "Count the number of code lines in the selected region or entire buffer (if none)."
  (interactive)
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        (count 0))
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (not (eobp))
            (unless (comment-only-p (line-beginning-position) (line-end-position))
              (cl-incf count))
            (forward-line 1)))))
    (when (called-interactively-p 'any)
      (message "%s" count))
    count))
;; count-lines-of-code:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-date-offset][date-offset:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-memory-use-counts-pretty][memory-use-counts-pretty:1]]
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
    (dotimes (x (length tags))
      (setq str (concat str (if (zerop (length str)) "" ", ")
                        (symbol-name (nth x tags)) ": " (number-to-string (nth x muc)))))
    str))
;; memory-use-counts-pretty:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-git-paste-cleanup][git-paste-cleanup:1]]
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
      (forward-line 0))
    (while (< (point) end)
      (when (looking-at "^+")
        (delete-char 1))
      (forward-line 1))))
;; git-paste-cleanup:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-execute-buffer][execute-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: execute-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: execute-buffer")

;; TODO: need to test
(defun execute-buffer ()
  "Execute or compile current file."
  (interactive)
  (let* ((file (shell-quote-argument buffer-file-name))
         (file-type (substring (shell-command-to-string (concat "file " file)) 0 -1))
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
    (shell-command (concat cmd " " file))))
;; execute-buffer:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-file-in-exec-path][file-in-exec-path:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-unicode-shell][unicode-shell:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-async-spinner][async-spinner:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-with-time][with-time:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-package-desc-summary-to-kill-ring][package-desc-summary-to-kill-ring:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-toggle-case-fold-search][toggle-case-fold-search:1]]
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

;; [[file:init-emacs.org::#functions-emacs-functions-derived-modes][derived-modes:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: derived-modes
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: derived-modes")

(defun derived-modes (&optional mode)
  "Return a list of the ancestor modes that MODE is derived from.

MODE defaults to `major-mode'."
  (let* ((mode (or mode major-mode))
         (mode-list (list mode)))
    (while (setq mode (get mode 'derived-mode-parent))
      (push mode mode-list))
    (nreverse mode-list)))
;; derived-modes:1 ends here

;; [[file:init-emacs.org::#functions-emacs-functions-list-charset-unicode][list-charset-unicode:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Functions: list-charset-unicode
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Functions: list-charset-unicode")

(defun list-charset-unicode ()
  "Display a list of characters in character set `unicode-bmp'."
  (interactive)
  (list-charset-chars 'unicode-bmp))
;; list-charset-unicode:1 ends here

;; [[file:init-emacs.org::#functions-emacs-grouped-functions][Emacs Grouped Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Emacs Grouped Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Emacs Grouped Functions")
;; Emacs Grouped Functions:1 ends here

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-buffer-kill][Buffer Kill:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Buffer Kill
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Buffer Kill")
;; Buffer Kill:1 ends here

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-buffer-kill-kill-buffer-query-functions-maybe-bury][kill-buffer-query-functions-maybe-bury:1]]
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

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-buffer-kill-kill-other-window-buffer][kill-other-window-buffer:1]]
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

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-buffer-kill-kill-other-window-buffer-and-delete-window][kill-other-window-buffer-and-delete-window:1]]
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

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-clipboard][Clipboard:2]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Clipboard
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Clipboard")

;; Modified versions of the similarly named functions `clipboard-kill-region',
;; `clipboard-kill-ring-save', and `clipboard-yank'. These functions use the
;; Linux command line tool `xsel' (which must be installed) to get the same
;; functionality when running Emacs in command line mode.

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
;; Clipboard:2 ends here

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-occur][Occur:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Emacs Grouped Functions: Occur
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Emacs Grouped Functions: Occur")
;; Occur:1 ends here

;; [[file:init-emacs.org::#functions-emacs-grouped-functions-occur-occur-inverse][occur-inverse:1]]
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

;; [[file:init-emacs.org::#functions-text-conversion-functions][Text Conversion Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Text Conversion Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Text Conversion Functions")
;; Text Conversion Functions:1 ends here

;; [[file:init-emacs.org::#functions-text-conversion-functions-set-coding-system][set-coding-system:1]]
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

;; [[file:init-emacs.org::#functions-text-conversion-functions-escape-xml][escape-xml:1]]
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

;; [[file:init-emacs.org::#functions-text-conversion-functions-unescape-xml][unescape-xml:1]]
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

;; [[file:init-emacs.org::#functions-text-conversion-functions-titleize][titleize:1]]
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
  (interactive "*")
  (let ((case-fold-search nil)          ; case sensitive search
        (words-single
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
          ;;(abbreviation-word-regexp "\\b[A-Z][.A-Z]+[^ \t]*\\b")
          (abbreviation-word-regexp (rx word-boundary
                                        (any digit upper)
                                        (one-or-more (any "." digit upper))
                                        (zero-or-more (not space))
                                        word-boundary))
          ;;(mixed-word-regexp "\\b[A-Z]*[a-z]+[A-Z]+[^ \t]*\\b")
          (mixed-word-regexp (rx word-boundary
                                 (zero-or-more upper)
                                 (one-or-more lower)
                                 (one-or-more upper)
                                 (zero-or-more (not space))
                                 word-boundary))
          ;;(first-word-regexp "\\(^[ \t]*\\(\\w+\\)\\|[\.!\?:&\*()/][ \t]*\\(\\w+\\)\\)")
          (first-word-regexp (rx (or (seq bos
                                          (zero-or-more (not space))
                                          (group (one-or-more word)))
                                     ;;(seq (or "." "!" "?" ":" "&" "*" "(" ")" "/")
                                     (seq (or (seq punct (one-or-more space))
                                              (seq (or "(" ")" "/") (zero-or-more space)))
                                          (group (one-or-more word))))))
          ;;(last-word-regexp "\\(\\(\\w+\\)[ \t]*$\\|\\(\\w+\\)[ \t]*[\.!\?:&\*()\[]\\)")
          (last-word-regexp (rx (or (seq (group (one-or-more word))
                                         (zero-or-more space)
                                         eos)
                                    (seq (group (one-or-more word))
                                         (zero-or-more space)
                                         ;;(one-or-more "." "!" "?" ":" "&" "*" "(" ")" "[" "]")
                                         (one-or-more punct))))))
      (cl-labels
          ((get-fixed (string regexp)
                      (let ((case-fold-search nil)
                            (pos 0)
                            fixed)
                        (while (string-match regexp string pos)
                          (push (list (match-beginning 0)
                                      (match-end 0)
                                      (match-string 0 string))
                                fixed)
                          (setq pos (match-end 0)))
                        (nreverse fixed)))
           (set-fixed (string fixed)
                      (let ((string string))
                        (dolist (a fixed)
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
        (let ((fixed (append (get-fixed string abbreviation-word-regexp)
                             (get-fixed string mixed-word-regexp))))
          (if do-not-cap-ends
              (set-fixed (cap string) fixed)
            (set-fixed
             (replace-regexp-in-string
              first-word-regexp 'capitalize
              (replace-regexp-in-string
               last-word-regexp 'capitalize
               (cap string) t t) t t) fixed)))))))
;; titleize:1 ends here

;; [[file:init-emacs.org::#functions-text-conversion-functions-titleize-word-enhanced][titleize-word-enhanced:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: titleize-word-enhanced
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: titleize-word-enhanced")

(defun titleize-word-enhanced (arg)
  "Titleize word at point."
  (interactive "*p")
  (let ((syntax-table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "." syntax-table)
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?' "w" syntax-table)
    (with-syntax-table syntax-table
      (dotimes (_ (or arg 1))
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

;; [[file:init-emacs.org::#functions-text-conversion-functions-titleize-line-or-region][titleize-line-or-region:1]]
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
  (let ((pos (point)))
    (save-match-data
      (let* ((beg (or beg (cond
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
        (deactivate-mark)
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (let ((syntax-table (copy-syntax-table (syntax-table)))
                (str (buffer-substring-no-properties beg end)))
            (modify-syntax-entry ?_ "." syntax-table)
            (modify-syntax-entry ?- "." syntax-table)
            (modify-syntax-entry ?' "w" syntax-table)
            (with-syntax-table syntax-table
              (delete-region beg end)
              (goto-char beg)
              (insert (titleize str))
              (goto-char (+ beg col)))))))
    (goto-char pos)))
;; titleize-line-or-region:1 ends here

;; [[file:init-emacs.org::#functions-text-conversion-functions-unfill-paragraph][unfill-paragraph:1]]
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

;; [[file:init-emacs.org::#functions-text-conversion-functions-single-space-punctuation][single-space-punctuation:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: single-space-punctuation
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: single-space-punctuation")

(defun single-space-punctuation (&optional beg end)
  "Single-space sentence ending punctuation in the current
paragraph or selected region."
  (interactive "*")
  (let ((beg (or beg (and (use-region-p) (region-beginning))))
        (end (or end (and (use-region-p) (region-end)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (unless (and beg end)
            (mark-paragraph)
            (setq beg (point)
                  end (mark-marker)))
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\([^[:blank:]][.?!]['\"”)]?\\)[[:blank:]]\\([^[:blank:]]\\)" end :noerror)
            (replace-match "\\1 \\2")))))))
;; single-space-punctuation:1 ends here

;; [[file:init-emacs.org::#functions-text-conversion-functions-double-space-punctuation][double-space-punctuation:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: double-space-punctuation
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: double-space-punctuation")

(defun double-space-punctuation (&optional beg end)
  "Double-space sentence ending punctuation in the current
paragraph or selected region."
  (interactive "*")
  (let ((beg (or beg (and (use-region-p) (region-beginning))))
        (end (or end (and (use-region-p) (region-end)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (unless (and beg end)
            (mark-paragraph)
            (setq beg (point)
                  end (mark-marker)))
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\([^[:blank:]][.?!]['\"”)]?\\)[[:blank:]]\\([^[:blank:]]\\)" end :noerror)
            (replace-match "\\1  \\2")))))))
;; double-space-punctuation:1 ends here

;; [[file:init-emacs.org::#functions-text-conversion-functions-convert-unicode-characters][convert-unicode-characters:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Conversion Functions: convert-unicode-characters
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Conversion Functions: convert-unicode-characters")

(defun convert-unicode-characters (&optional beg end)
  "Convert common unicode characters of document or selected region."
  (interactive "*")
  (let ((beg (or beg (and (use-region-p) (region-beginning)) (point-min)))
        (end (or end (and (use-region-p) (region-end)) (point-max))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (dolist (x '(("✴" . "•")
                       ("…" . "...")
                       ("’" . "'")
                       ("“" . "\"")
                       ("”" . "\"")
                       ;;("—" . "-")
                       ))
            (goto-char (point-min))
            (while (re-search-forward (car x) end :noerror)
              (replace-match (cdr x)))))))))
;; convert-unicode-characters:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions][Text Inserting Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Text Inserting Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Text Inserting Functions")
;; Text Inserting Functions:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-timestamp][insert-timestamp:1]]
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

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-path][insert-path:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-path
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-path")

(defun insert-path (path)
  "Insert path."
  (interactive "*FPath: ")
  (insert (expand-file-name path)))
;; insert-path:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-uuid][uuid:1]]
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
    `(user-error "Could not find a suitable system command to produce a UUID"))))
(defalias 'guid 'uuid)
;; uuid:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-uuid][insert-uuid:1]]
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

;; [[file:init-emacs.org::#functions-text-inserting-functions-uuid-decimal][uuid-decimal:1]]
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

;; [[file:init-emacs.org::#functions-text-inserting-functions-uuid-string][uuid-string:1]]
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
    (cl-do* ((n 0 (mod u 64))
             (s "" (concat (n-to-s n) s))
             (u (uuid-decimal) (/ u 64)))
        ((= u 0) s))))

(defalias 'guid-decimal 'uuid-string)
;; uuid-string:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-uuid-xml][uuid-xml:1]]
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
      `(user-error "Could not find system command: %s" ,cmd))))
;; uuid-xml:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-uuid-xml][insert-uuid-xml:1]]
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

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-incrementing-vertical-numbers][insert-incrementing-vertical-numbers:1]]
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

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-column-position-ruler][insert-column-position-ruler:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-column-position-ruler
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-column-position-ruler")

(defun insert-column-position-ruler (&optional tens)
  "Insert a column poisition ruler.
If TENS is non-nil, insert that many ruler segments of ten digits.
TENS defaults to 12."
  (interactive "*")
  (let ((tens (or tens 12)))
    (dotimes (_ tens)
      (insert "1234567890"))))
;; insert-column-position-ruler:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-append-char-to-column][append-char-to-column:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: append-char-to-column
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: append-char-to-column")

(defun append-char-to-column (char col)
  "Append character CHAR up to column COL and delete any past that point."
  (save-mark-and-excursion
    (goto-char (line-end-position))
    (while (< (- (point) (line-beginning-position)) col)
      (insert char))
    (goto-char (+ (line-beginning-position) col))
    (while (and
            (char-after)
            (char-equal (char-after) (string-to-char char)))
      (delete-char 1))))
;; append-char-to-column:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-append-equal-to-column-80][append-equal-to-column-80:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: append-equal-to-column-80
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: append-equal-to-column-80")

(defun append-equal-to-column-80 ()
  "Insert equal characters up to column 80."
  (interactive "*")
  (append-char-to-column "=" 80))
;; append-equal-to-column-80:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-append-dash-to-column-80][append-dash-to-column-80:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: append-dash-to-column-80
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: append-dash-to-column-80")

(defun append-dash-to-column-80 ()
  "Insert dash characters up to column 80."
  (interactive "*")
  (append-char-to-column "-" 80))
;; append-dash-to-column-80:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-append-asterisk-to-column-80][append-asterisk-to-column-80:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: append-asterisk-to-column-80
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: append-asterisk-to-column-80")

(defun append-asterisk-to-column-80 ()
  "Insert asterisk characters up to column 80."
  (interactive "*")
  (append-char-to-column "*" 80))
;; append-asterisk-to-column-80:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-lisp-comment-block-equal][insert-lisp-comment-block-equal:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-lisp-comment-block-equal
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-lisp-comment-block-equal")

(defun insert-lisp-comment-block-equal ()
  "Insert lisp comment block (equal)."
  (interactive "*")
  (indent-according-to-mode)
  (insert ";;")
  (append-equal-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert ";;")
  (newline-and-indent)
  (insert ";;")
  (append-equal-to-column-80)
  (end-of-line)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))
;; insert-lisp-comment-block-equal:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-lisp-comment-block-dash][insert-lisp-comment-block-dash:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-lisp-comment-block-dash
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-lisp-comment-block-dash")

(defun insert-lisp-comment-block-dash ()
  "Insert lisp comment block (dash)."
  (interactive "*")
  (indent-according-to-mode)
  (insert ";;")
  (append-dash-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert ";;")
  (newline-and-indent)
  (insert ";;")
  (append-dash-to-column-80)
  (end-of-line)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))
;; insert-lisp-comment-block-dash:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-center-lisp-comment][insert-center-lisp-comment:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-center-lisp-comment
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-center-lisp-comment")

(defun insert-center-lisp-comment ()
  "Insert center lisp comment (in comment block)."
  (interactive "*")
  (save-mark-and-excursion
    (save-match-data
      (forward-line 0)
      (while (looking-at " +") (forward-char 1))
      (while (looking-at ";+") (forward-char 1))
      (let ((start (point)))
        (forward-line -1)
        (let ((len (- (line-end-position) (line-beginning-position)))
              (spacer (char-before (line-end-position))))
          (forward-line 1)
          (while (search-forward (char-to-string spacer) (line-end-position) :noerror)
            (replace-match ""))
          (goto-char start)
          (while (looking-at " ")
            (delete-char 1 t))
          (goto-char (line-end-position))
          (while (eq (char-before (point)) ? )
            (delete-char -1 t))
          (let ((spacers (- (floor (/ (- len (- (point) start)) 2)) 4)))
            (goto-char start)
            (insert " " (make-string spacers spacer) " ")
            (goto-char (line-end-position))
            (insert " ")
            (insert (make-string (- len (- (point) (line-beginning-position))) ?=))))))))
;; insert-center-lisp-comment:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-c-comment-block][insert-c-comment-block:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-c-comment-block
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-c-comment-block")

(defun insert-c-comment-block ()
  "Insert c/c++/java comment block."
  (interactive "*")
  (indent-according-to-mode)
  (insert "/")
  (append-asterisk-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (append-asterisk-to-column-80)
  (end-of-line)
  (delete-char -1)
  (insert "/")
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))
;; insert-c-comment-block:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-c-comment-stub][insert-c-comment-stub:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-c-comment-stub
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-c-comment-stub")

(defun insert-c-comment-stub ()
  "Insert c/c++/java comment stub."
  (interactive "*")
  (end-of-line)
  (indent-according-to-mode)
  (insert "/**")
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (newline-and-indent)
  (insert "*/")
  (indent-according-to-mode)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))
;; insert-c-comment-stub:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-db-change-log-template-line][insert-db-change-log-template-line:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-db-change-log-template-line
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-db-change-log-template-line")

(defun insert-db-change-log-template-line ()
  "Insert Everest DB Change Log template line at point."
  (interactive "*")
  ;;(insert (cons '(format-time-string "%m/%d" (current-time)) " | | | E_ | .D.Q.S.T.P. | yes\n"))
  (insert (format-time-string "%m/%d" (current-time)))
  (insert " |  |  | E_ | .D.Q.S.T.P. | yes")
  (newline)
  (forward-line -1)
  (forward-char 8))
;; insert-db-change-log-template-line:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-db-change-log-template-line-legacy][insert-db-change-log-template-line-legacy:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-db-change-log-template-line-legacy
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-db-change-log-template-line-legacy")

(defun insert-db-change-log-template-line-legacy ()
  "Insert Legacy DB Change Log template line at point."
  (interactive "*")
  (insert (format-time-string "%m/%d" (current-time)))
  (insert " |  |  | AwardCafe_Client | .D.S.P. | yes")
  (newline)
  (forward-line -1)
  (forward-char 8))
;; insert-db-change-log-template-line-legacy:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-xml-header][insert-xml-header:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-xml-header
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-xml-header")

(defun insert-xml-header ()
  "Insert standard XML header.

Specifically: <?xml version=\"1.0\" encoding=\"utf-8\"?>"
  (interactive "*")
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
;; insert-xml-header:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-lexical-binding][insert-lexical-binding:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-lexical-binding
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-lexical-binding")

(defun insert-lexical-binding ()
  "Insert file local variable `lexical-binding' in the first line
of the current buffer."
  (interactive "*")
  (save-mark-and-excursion
    (goto-char (point-min))
    (if (looking-at ";; -\\*-.*-\\*-")
        (unless (looking-at ";; -\\*-.*lexical-binding:.*-\\*-")
          (goto-char (line-end-position))
          (forward-char -4)
          (when (looking-at " -\\*-")
            (forward-char -1)
            (if (looking-at ";")
                (forward-char 1)
              (progn
                (forward-char 1)
                (insert ";")))
            (insert " lexical-binding: t;")))
      (insert ";; -*- lexical-binding: t; -*-\n;;\n"))))
;; insert-lexical-binding:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-org-header][insert-org-header:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-org-header
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-org-header")

(defun insert-org-header ()
  "Insert `org-mode' header."
  (interactive "*")
  (let ((text
         `("* Org                                                              :noexport:"
           "#+TITLE: TITLE"
           "#+AUTHOR: Kyle W. T. Sherman"
           ,(concat "#+EMAIL: " user-mail-address)
           "#+FILENAME: FILENAME.org"
           "#+DESCRIPTION: DESCRIPTION"
           "#+KEYWORDS: KEYWORD, emacs, org-mode, babel, literate programming, reproducible research"
           "#+LANGUAGE: en"
           "#+PROPERTY: header-args :noweb yes :padline yes :comments no :results silent output :mkdirp yes :cache yes"
           "#+OPTIONS: num:nil toc:nil d:(HIDE) tags:not-in-toc html-preamble:nil html-postamble:nil"
           "#+STARTUP: noindent odd overview"
           "#+TIMESTAMP: <>"
           "")))
    (dolist (x text)
      (insert x)
      (newline))))
;; insert-org-header:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-toc-header][insert-toc-header:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-toc-header
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-toc-header")

(defun insert-toc-header ()
  "Insert `org-mode' table of contents (TOC) header."
  (interactive "*")
  (let ((text
         `("* Table of Contents"
           "  :PROPERTIES:"
           "  :CUSTOM_ID: table-of-contents"
           "  :TOC: :include all"
           "  :END:"
           ""
           "  :CONTENTS:"
           "  :END:"
           "")))
    (dolist (x text)
      (insert x)
      (newline))))
;; insert-toc-header:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-figlet][insert-figlet:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-figlet
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-figlet")

(defun insert-figlet (text)
  "Insert figlet version of TEXT, if figlet is installed."
  (interactive "*sText: ")
  (let ((figlet "figlet"))
    (unless (executable-find figlet)
      (user-error "Could not find system command: %s" figlet))
    (insert (shell-command-to-string (concat figlet " " text)))))
;; insert-figlet:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-password][insert-password:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-password
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-password")

(defun insert-password (length)
  "Insert generated password of LENGTH characters."
  (interactive "*nLength: ")
  (cl-labels
      ((string-to-list (string) (cl-loop for x across string collect x))
       (list-to-string (list) (mapconcat 'string list "")))
    (let* ((upper (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
           (lower (string-to-list "abcdefghijklmnopqrstuvwxyz"))
           (numbers (string-to-list "0123456789")) ; 0 is omitted to prevent confusion with O
           (symbols (string-to-list "!@#$%^&*"))
           (letters (append upper lower numbers symbols))
           (letters-size (length letters))
           (bookends (append upper lower numbers))
           (bookends-size (length bookends))
           (min-numbers 1)
           (min-symbols 1)
           (number-count 0)
           (symbol-count 0)
           password
           valid)
      (when (< length 6)
        (user-error "Password LENGTH must be at least 6."))
      (while (or (< (length password) length)
                 (not valid))
        (when (= (length password) length)
          ;; password is invalid, so clear it and try again
          (setq password nil))
        ;; password will start and end with a non-symbol
        (let ((char (if (and (> (length password) 0)
                             (< (length password) (1- length)))
                        (elt letters (random letters-size))
                        (elt bookends (random bookends-size)))))
          (push char password)
          (when (member char numbers)
            (cl-incf number-count))
          (when (member char symbols)
            (cl-incf symbol-count))
          (when (and (> number-count 0)
                     (> symbol-count 0))
            (setq valid t))))
      (insert (list-to-string password)))))

(defun insert-password-20 ()
  "Call `insert-password' with a LENGTH of 20"
  (interactive "*")
  (insert-password 20))
;; insert-password:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-password-phrase][insert-password-phrase:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-password-phrase
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-password-phrase")

(defun insert-password-phrase (count &optional output)
  "Insert generated password phrase containing COUNT words.

OUTPUT (defaults to 'phrase):

  'phrase   \"threewordphrase\"
  'space    \"three word phrase\"
  'hyphen   \"three-word-phrase\"
  'list     (\"three\" \"word\" \"phrase\")"
  (interactive
   (list
    (read-number "Count: ")
    (intern (completing-read "Output: " '("phrase" "space" "hyphen" "list") nil t))))
  (let* ((words
          '("abandon" "ability" "able" "about" "above" "absent" "absorb" "abstract" "absurd"
            "abuse" "access" "accident" "account" "accuse" "achieve" "acid" "acoustic"
            "acquire" "across" "act" "action" "actor" "actress" "actual" "adapt" "add" "addict"
            "address" "adjust" "admit" "adult" "advance" "advice" "aerobic" "affair" "afford"
            "afraid" "again" "age" "agent" "agree" "ahead" "aim" "air" "airport" "aisle" "alarm"
            "album" "alcohol" "alert" "alien" "all" "alley" "allow" "almost" "alone" "alpha"
            "already" "also" "alter" "always" "amateur" "amazing" "among" "amount" "amused"
            "analyst" "anchor" "ancient" "anger" "angle" "angry" "animal" "ankle" "announce"
            "annual" "another" "answer" "antenna" "antique" "anxiety" "any" "apart" "apology"
            "appear" "apple" "approve" "april" "arch" "arctic" "area" "arena" "argue" "arm" "armed"
            "armor" "army" "around" "arrange" "arrest" "arrive" "arrow" "art" "artefact" "artist"
            "artwork" "ask" "aspect" "assault" "asset" "assist" "assume" "asthma" "athlete"
            "atom" "attack" "attend" "attitude" "attract" "auction" "audit" "august" "aunt"
            "author" "auto" "autumn" "average" "avocado" "avoid" "awake" "aware" "away" "awesome"
            "awful" "awkward" "axis" "baby" "bachelor" "bacon" "badge" "bag" "balance" "balcony"
            "ball" "bamboo" "banana" "banner" "bar" "barely" "bargain" "barrel" "base" "basic"
            "basket" "battle" "beach" "bean" "beauty" "because" "become" "beef" "before" "begin"
            "behave" "behind" "believe" "below" "belt" "bench" "benefit" "best" "betray" "better"
            "between" "beyond" "bicycle" "bid" "bike" "bind" "biology" "bird" "birth" "bitter"
            "black" "blade" "blame" "blanket" "blast" "bleak" "bless" "blind" "blood" "blossom"
            "blouse" "blue" "blur" "blush" "board" "boat" "body" "boil" "bomb" "bone" "bonus" "book"
            "boost" "border" "boring" "borrow" "boss" "bottom" "bounce" "box" "boy" "bracket"
            "brain" "brand" "brass" "brave" "bread" "breeze" "brick" "bridge" "brief" "bright"
            "bring" "brisk" "broccoli" "broken" "bronze" "broom" "brother" "brown" "brush"
            "bubble" "buddy" "budget" "buffalo" "build" "bulb" "bulk" "bullet" "bundle" "bunker"
            "burden" "burger" "burst" "bus" "business" "busy" "butter" "buyer" "buzz" "cabbage"
            "cabin" "cable" "cactus" "cage" "cake" "call" "calm" "camera" "camp" "can" "canal"
            "cancel" "candy" "cannon" "canoe" "canvas" "canyon" "capable" "capital" "captain"
            "car" "carbon" "card" "cargo" "carpet" "carry" "cart" "case" "cash" "casino" "castle"
            "casual" "cat" "catalog" "catch" "category" "cattle" "caught" "cause" "caution"
            "cave" "ceiling" "celery" "cement" "census" "century" "cereal" "certain" "chair"
            "chalk" "champion" "change" "chaos" "chapter" "charge" "chase" "chat" "cheap" "check"
            "cheese" "chef" "cherry" "chest" "chicken" "chief" "child" "chimney" "choice"
            "choose" "chronic" "chuckle" "chunk" "churn" "cigar" "cinnamon" "circle" "citizen"
            "city" "civil" "claim" "clap" "clarify" "claw" "clay" "clean" "clerk" "clever" "click"
            "client" "cliff" "climb" "clinic" "clip" "clock" "clog" "close" "cloth" "cloud" "clown"
            "club" "clump" "cluster" "clutch" "coach" "coast" "coconut" "code" "coffee" "coil"
            "coin" "collect" "color" "column" "combine" "come" "comfort" "comic" "common"
            "company" "concert" "conduct" "confirm" "congress" "connect" "consider" "control"
            "convince" "cook" "cool" "copper" "copy" "coral" "core" "corn" "correct" "cost"
            "cotton" "couch" "country" "couple" "course" "cousin" "cover" "coyote" "crack"
            "cradle" "craft" "cram" "crane" "crash" "crater" "crawl" "crazy" "cream" "credit"
            "creek" "crew" "cricket" "crime" "crisp" "critic" "crop" "cross" "crouch" "crowd"
            "crucial" "cruel" "cruise" "crumble" "crunch" "crush" "cry" "crystal" "cube"
            "culture" "cup" "cupboard" "curious" "current" "curtain" "curve" "cushion" "custom"
            "cute" "cycle" "dad" "damage" "damp" "dance" "danger" "daring" "dash" "daughter" "dawn"
            "day" "deal" "debate" "debris" "decade" "december" "decide" "decline" "decorate"
            "decrease" "deer" "defense" "define" "defy" "degree" "delay" "deliver" "demand"
            "demise" "denial" "dentist" "deny" "depart" "depend" "deposit" "depth" "deputy"
            "derive" "describe" "desert" "design" "desk" "despair" "destroy" "detail" "detect"
            "develop" "device" "devote" "diagram" "dial" "diamond" "diary" "dice" "diesel" "diet"
            "differ" "digital" "dignity" "dilemma" "dinner" "dinosaur" "direct" "dirt"
            "disagree" "discover" "disease" "dish" "dismiss" "disorder" "display" "distance"
            "divert" "divide" "divorce" "dizzy" "doctor" "document" "dog" "doll" "dolphin"
            "domain" "donate" "donkey" "donor" "door" "dose" "double" "dove" "draft" "dragon"
            "drama" "drastic" "draw" "dream" "dress" "drift" "drill" "drink" "drip" "drive" "drop"
            "drum" "dry" "duck" "dumb" "dune" "during" "dust" "dutch" "duty" "dwarf" "dynamic"
            "eager" "eagle" "early" "earn" "earth" "easily" "east" "easy" "echo" "ecology"
            "economy" "edge" "edit" "educate" "effort" "egg" "eight" "either" "elbow" "elder"
            "electric" "elegant" "element" "elephant" "elevator" "elite" "else" "embark"
            "embody" "embrace" "emerge" "emotion" "employ" "empower" "empty" "enable" "enact"
            "end" "endless" "endorse" "enemy" "energy" "enforce" "engage" "engine" "enhance"
            "enjoy" "enlist" "enough" "enrich" "enroll" "ensure" "enter" "entire" "entry"
            "envelope" "episode" "equal" "equip" "era" "erase" "erode" "erosion" "error" "erupt"
            "escape" "essay" "essence" "estate" "eternal" "ethics" "evidence" "evil" "evoke"
            "evolve" "exact" "example" "excess" "exchange" "excite" "exclude" "excuse"
            "execute" "exercise" "exhaust" "exhibit" "exile" "exist" "exit" "exotic" "expand"
            "expect" "expire" "explain" "expose" "express" "extend" "extra" "eye" "eyebrow"
            "fabric" "face" "faculty" "fade" "faint" "faith" "fall" "false" "fame" "family"
            "famous" "fan" "fancy" "fantasy" "farm" "fashion" "fat" "fatal" "father" "fatigue"
            "fault" "favorite" "feature" "february" "federal" "fee" "feed" "feel" "female"
            "fence" "festival" "fetch" "fever" "few" "fiber" "fiction" "field" "figure" "file"
            "film" "filter" "final" "find" "fine" "finger" "finish" "fire" "firm" "first" "fiscal"
            "fish" "fit" "fitness" "fix" "flag" "flame" "flash" "flat" "flavor" "flee" "flight"
            "flip" "float" "flock" "floor" "flower" "fluid" "flush" "fly" "foam" "focus" "fog" "foil"
            "fold" "follow" "food" "foot" "force" "forest" "forget" "fork" "fortune" "forum"
            "forward" "fossil" "foster" "found" "fox" "fragile" "frame" "frequent" "fresh"
            "friend" "fringe" "frog" "front" "frost" "frown" "frozen" "fruit" "fuel" "fun" "funny"
            "furnace" "fury" "future" "gadget" "gain" "galaxy" "gallery" "game" "gap" "garage"
            "garbage" "garden" "garlic" "garment" "gas" "gasp" "gate" "gather" "gauge" "gaze"
            "general" "genius" "genre" "gentle" "genuine" "gesture" "ghost" "giant" "gift"
            "giggle" "ginger" "giraffe" "girl" "give" "glad" "glance" "glare" "glass" "glide"
            "glimpse" "globe" "gloom" "glory" "glove" "glow" "glue" "goat" "goddess" "gold" "good"
            "goose" "gorilla" "gospel" "gossip" "govern" "gown" "grab" "grace" "grain" "grant"
            "grape" "grass" "gravity" "great" "green" "grid" "grief" "grit" "grocery" "group"
            "grow" "grunt" "guard" "guess" "guide" "guilt" "guitar" "gun" "gym" "habit" "hair" "half"
            "hammer" "hamster" "hand" "happy" "harbor" "hard" "harsh" "harvest" "hat" "have" "hawk"
            "hazard" "head" "health" "heart" "heavy" "hedgehog" "height" "hello" "helmet" "help"
            "hen" "hero" "hidden" "high" "hill" "hint" "hip" "hire" "history" "hobby" "hockey" "hold"
            "hole" "holiday" "hollow" "home" "honey" "hood" "hope" "horn" "horror" "horse"
            "hospital" "host" "hotel" "hour" "hover" "hub" "huge" "human" "humble" "humor"
            "hundred" "hungry" "hunt" "hurdle" "hurry" "hurt" "husband" "hybrid" "ice" "icon"
            "idea" "identify" "idle" "ignore" "ill" "illegal" "illness" "image" "imitate"
            "immense" "immune" "impact" "impose" "improve" "impulse" "inch" "include" "income"
            "increase" "index" "indicate" "indoor" "industry" "infant" "inflict" "inform"
            "inhale" "inherit" "initial" "inject" "injury" "inmate" "inner" "innocent" "input"
            "inquiry" "insane" "insect" "inside" "inspire" "install" "intact" "interest" "into"
            "invest" "invite" "involve" "iron" "island" "isolate" "issue" "item" "ivory" "jacket"
            "jaguar" "jar" "jazz" "jealous" "jeans" "jelly" "jewel" "job" "join" "joke" "journey"
            "joy" "judge" "juice" "jump" "jungle" "junior" "junk" "just" "kangaroo" "keen" "keep"
            "ketchup" "key" "kick" "kid" "kidney" "kind" "kingdom" "kiss" "kit" "kitchen" "kite"
            "kitten" "kiwi" "knee" "knife" "knock" "know" "lab" "label" "labor" "ladder" "lady"
            "lake" "lamp" "language" "laptop" "large" "later" "latin" "laugh" "laundry" "lava"
            "law" "lawn" "lawsuit" "layer" "lazy" "leader" "leaf" "learn" "leave" "lecture" "left"
            "leg" "legal" "legend" "leisure" "lemon" "lend" "length" "lens" "leopard" "lesson"
            "letter" "level" "liar" "liberty" "library" "license" "life" "lift" "light" "like"
            "limb" "limit" "link" "lion" "liquid" "list" "little" "live" "lizard" "load" "loan"
            "lobster" "local" "lock" "logic" "lonely" "long" "loop" "lottery" "loud" "lounge"
            "love" "loyal" "lucky" "luggage" "lumber" "lunar" "lunch" "luxury" "lyrics" "machine"
            "mad" "magic" "magnet" "maid" "mail" "main" "major" "make" "mammal" "man" "manage"
            "mandate" "mango" "mansion" "manual" "maple" "marble" "march" "margin" "marine"
            "market" "marriage" "mask" "mass" "master" "match" "material" "math" "matrix"
            "matter" "maximum" "maze" "meadow" "mean" "measure" "meat" "mechanic" "medal" "media"
            "melody" "melt" "member" "memory" "mention" "menu" "mercy" "merge" "merit" "merry"
            "mesh" "message" "metal" "method" "middle" "midnight" "milk" "million" "mimic" "mind"
            "minimum" "minor" "minute" "miracle" "mirror" "misery" "miss" "mistake" "mix" "mixed"
            "mixture" "mobile" "model" "modify" "mom" "moment" "monitor" "monkey" "monster"
            "month" "moon" "moral" "more" "morning" "mosquito" "mother" "motion" "motor"
            "mountain" "mouse" "move" "movie" "much" "muffin" "mule" "multiply" "muscle" "museum"
            "mushroom" "music" "must" "mutual" "myself" "mystery" "myth" "naive" "name" "napkin"
            "narrow" "nasty" "nation" "nature" "near" "neck" "need" "negative" "neglect"
            "neither" "nephew" "nerve" "nest" "net" "network" "neutral" "never" "news" "next"
            "nice" "night" "noble" "noise" "nominee" "noodle" "normal" "north" "nose" "notable"
            "note" "nothing" "notice" "novel" "now" "nuclear" "number" "nurse" "nut" "oak" "obey"
            "object" "oblige" "obscure" "observe" "obtain" "obvious" "occur" "ocean" "october"
            "odor" "off" "offer" "office" "often" "oil" "okay" "old" "olive" "olympic" "omit" "once"
            "one" "onion" "online" "only" "open" "opera" "opinion" "oppose" "option" "orange"
            "orbit" "orchard" "order" "ordinary" "organ" "orient" "original" "orphan" "ostrich"
            "other" "outdoor" "outer" "output" "outside" "oval" "oven" "over" "own" "owner"
            "oxygen" "oyster" "ozone" "pact" "paddle" "page" "pair" "palace" "palm" "panda" "panel"
            "panic" "panther" "paper" "parade" "parent" "park" "parrot" "party" "pass" "patch"
            "path" "patient" "patrol" "pattern" "pause" "pave" "payment" "peace" "peanut" "pear"
            "peasant" "pelican" "pen" "penalty" "pencil" "people" "pepper" "perfect" "permit"
            "person" "pet" "phone" "photo" "phrase" "physical" "piano" "picnic" "picture" "piece"
            "pig" "pigeon" "pill" "pilot" "pink" "pioneer" "pipe" "pistol" "pitch" "pizza" "place"
            "planet" "plastic" "plate" "play" "please" "pledge" "pluck" "plug" "plunge" "poem"
            "poet" "point" "polar" "pole" "police" "pond" "pony" "pool" "popular" "portion"
            "position" "possible" "post" "potato" "pottery" "poverty" "powder" "power"
            "practice" "praise" "predict" "prefer" "prepare" "present" "pretty" "prevent"
            "price" "pride" "primary" "print" "priority" "prison" "private" "prize" "problem"
            "process" "produce" "profit" "program" "project" "promote" "proof" "property"
            "prosper" "protect" "proud" "provide" "public" "pudding" "pull" "pulp" "pulse"
            "pumpkin" "punch" "pupil" "puppy" "purchase" "purity" "purpose" "purse" "push" "put"
            "puzzle" "pyramid" "quality" "quantum" "quarter" "question" "quick" "quit" "quiz"
            "quote" "rabbit" "raccoon" "race" "rack" "radar" "radio" "rail" "rain" "raise" "rally"
            "ramp" "ranch" "random" "range" "rapid" "rare" "rate" "rather" "raven" "raw" "razor"
            "ready" "real" "reason" "rebel" "rebuild" "recall" "receive" "recipe" "record"
            "recycle" "reduce" "reflect" "reform" "refuse" "region" "regret" "regular" "reject"
            "relax" "release" "relief" "rely" "remain" "remember" "remind" "remove" "render"
            "renew" "rent" "reopen" "repair" "repeat" "replace" "report" "require" "rescue"
            "resemble" "resist" "resource" "response" "result" "retire" "retreat" "return"
            "reunion" "reveal" "review" "reward" "rhythm" "rib" "ribbon" "rice" "rich" "ride"
            "ridge" "rifle" "right" "rigid" "ring" "riot" "ripple" "risk" "ritual" "rival" "river"
            "road" "roast" "robot" "robust" "rocket" "romance" "roof" "rookie" "room" "rose"
            "rotate" "rough" "round" "route" "royal" "rubber" "rude" "rug" "rule" "run" "runway"
            "rural" "sad" "saddle" "sadness" "safe" "sail" "salad" "salmon" "salon" "salt" "salute"
            "same" "sample" "sand" "satisfy" "satoshi" "sauce" "sausage" "save" "say" "scale"
            "scan" "scare" "scatter" "scene" "scheme" "school" "science" "scissors" "scorpion"
            "scout" "scrap" "screen" "script" "scrub" "sea" "search" "season" "seat" "second"
            "secret" "section" "security" "seed" "seek" "segment" "select" "sell" "seminar"
            "senior" "sense" "sentence" "series" "service" "session" "settle" "setup" "seven"
            "shadow" "shaft" "shallow" "share" "shed" "shell" "sheriff" "shield" "shift" "shine"
            "ship" "shiver" "shock" "shoe" "shoot" "shop" "short" "shoulder" "shove" "shrimp"
            "shrug" "shuffle" "shy" "sibling" "sick" "side" "siege" "sight" "sign" "silent" "silk"
            "silly" "silver" "similar" "simple" "since" "sing" "siren" "sister" "situate" "six"
            "size" "skate" "sketch" "ski" "skill" "skin" "skirt" "skull" "slab" "slam" "sleep"
            "slender" "slice" "slide" "slight" "slim" "slogan" "slot" "slow" "slush" "small"
            "smart" "smile" "smoke" "smooth" "snack" "snake" "snap" "sniff" "snow" "soap" "soccer"
            "social" "sock" "soda" "soft" "solar" "soldier" "solid" "solution" "solve" "someone"
            "song" "soon" "sorry" "sort" "soul" "sound" "soup" "source" "south" "space" "spare"
            "spatial" "spawn" "speak" "special" "speed" "spell" "spend" "sphere" "spice" "spider"
            "spike" "spin" "spirit" "split" "spoil" "sponsor" "spoon" "sport" "spot" "spray"
            "spread" "spring" "spy" "square" "squeeze" "squirrel" "stable" "stadium" "staff"
            "stage" "stairs" "stamp" "stand" "start" "state" "stay" "steak" "steel" "stem" "step"
            "stereo" "stick" "still" "sting" "stock" "stomach" "stone" "stool" "story" "stove"
            "strategy" "street" "strike" "strong" "struggle" "student" "stuff" "stumble"
            "style" "subject" "submit" "subway" "success" "such" "sudden" "suffer" "sugar"
            "suggest" "suit" "summer" "sun" "sunny" "sunset" "super" "supply" "supreme" "sure"
            "surface" "surge" "surprise" "surround" "survey" "suspect" "sustain" "swallow"
            "swamp" "swap" "swarm" "swear" "sweet" "swift" "swim" "swing" "switch" "sword" "symbol"
            "symptom" "syrup" "system" "table" "tackle" "tag" "tail" "talent" "talk" "tank" "tape"
            "target" "task" "taste" "tattoo" "taxi" "teach" "team" "tell" "ten" "tenant" "tennis"
            "tent" "term" "test" "text" "thank" "that" "theme" "then" "theory" "there" "they" "thing"
            "this" "thought" "three" "thrive" "throw" "thumb" "thunder" "ticket" "tide" "tiger"
            "tilt" "timber" "time" "tiny" "tip" "tired" "tissue" "title" "toast" "tobacco" "today"
            "toddler" "toe" "together" "toilet" "token" "tomato" "tomorrow" "tone" "tongue"
            "tonight" "tool" "tooth" "top" "topic" "topple" "torch" "tornado" "tortoise" "toss"
            "total" "tourist" "toward" "tower" "town" "toy" "track" "trade" "traffic" "tragic"
            "train" "transfer" "trap" "trash" "travel" "tray" "treat" "tree" "trend" "trial"
            "tribe" "trick" "trigger" "trim" "trip" "trophy" "trouble" "truck" "true" "truly"
            "trumpet" "trust" "truth" "try" "tube" "tuition" "tumble" "tuna" "tunnel" "turkey"
            "turn" "turtle" "twelve" "twenty" "twice" "twin" "twist" "two" "type" "typical" "ugly"
            "umbrella" "unable" "unaware" "uncle" "uncover" "under" "undo" "unfair" "unfold"
            "unhappy" "uniform" "unique" "unit" "universe" "unknown" "unlock" "until" "unusual"
            "unveil" "update" "upgrade" "uphold" "upon" "upper" "upset" "urban" "urge" "usage"
            "use" "used" "useful" "useless" "usual" "utility" "vacant" "vacuum" "vague" "valid"
            "valley" "valve" "van" "vanish" "vapor" "various" "vast" "vault" "vehicle" "velvet"
            "vendor" "venture" "venue" "verb" "verify" "version" "very" "vessel" "veteran"
            "viable" "vibrant" "vicious" "victory" "video" "view" "village" "vintage" "violin"
            "virtual" "virus" "visa" "visit" "visual" "vital" "vivid" "vocal" "voice" "void"
            "volcano" "volume" "vote" "voyage" "wage" "wagon" "wait" "walk" "wall" "walnut" "want"
            "warfare" "warm" "warrior" "wash" "wasp" "waste" "water" "wave" "way" "wealth" "weapon"
            "wear" "weasel" "weather" "web" "wedding" "weekend" "weird" "welcome" "west" "wet"
            "whale" "what" "wheat" "wheel" "when" "where" "whip" "whisper" "wide" "width" "wife"
            "wild" "will" "win" "window" "wine" "wing" "wink" "winner" "winter" "wire" "wisdom"
            "wise" "wish" "witness" "wolf" "woman" "wonder" "wood" "wool" "word" "work" "world"
            "worry" "worth" "wrap" "wreck" "wrestle" "wrist" "write" "wrong" "yard" "year" "yellow"
            "you" "young" "youth" "zebra" "zero" "zone" "zoo"))
         (size (length words))
         phrase)
    (while (< (length phrase) count)
      (cl-pushnew (elt words (random size)) phrase))
    (cl-case output
      ('list (insert (format "%S" phrase)))
      ('space (insert (cl-reduce (lambda (x y) (concat x " " y)) phrase)))
      ('hyphen (insert (cl-reduce (lambda (x y) (concat x "-" y)) phrase)))
      (t (insert (cl-reduce (lambda (x y) (concat x y)) phrase))))))

(defun insert-password-phrase-three-space ()
  "Call `insert-password-phrase' with a COUNT of 3 and an OUTPUT
of 'space."
  (interactive "*")
  (insert-password-phrase 3 'space))

(defun insert-password-phrase-three-hyphen ()
  "Call `insert-password-phrase' with a COUNT of 3 and an OUTPUT
of 'hyphen."
  (interactive "*")
  (insert-password-phrase 3 'hyphen))
;; insert-password-phrase:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-license-gpl][insert-license-gpl:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-license-gpl
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-license-gpl")

(defun insert-license-gpl ()
  "Insert GPL2 license block to be used at the top of code files."
  (interactive "*")
  (let ((text
         `("General Public License (Version 2)"
           ""
           ,(concat "Copyright © " (format-time-string "%Y" nil t) " " user-full-name)
           ""
           "This program is free software; you can redistribute it and/or modify"
           "it under the terms of the GNU General Public License as published by"
           "the Free Software Foundation; either version 2 of the License, or"
           "(at your option) any later version."
           ""
           "This program is distributed in the hope that it will be useful,"
           "but WITHOUT ANY WARRANTY; without even the implied warranty of"
           "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the"
           "GNU General Public License for more details."
           ""
           "You should have received a copy of the GNU General Public License along"
           "with this program; if not, write to the Free Software Foundation, Inc.,"
           "51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.")))
    (dolist (x text)
      (call-interactively #'comment-dwim)
      (insert x)
      (newline))))
;; insert-license-gpl:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-license-mit][insert-license-mit:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-license-mit
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-license-mit")

(defun insert-license-mit ()
  "Insert MIT license block to be used at the top of code files."
  (interactive "*")
  (let ((text
         `("MIT License"
           ""
           ,(concat "Copyright © " (format-time-string "%Y" nil t) " " user-full-name)
           ""
           "Permission is hereby granted, free of charge, to any person obtaining"
           "a copy of this software and associated documentation files (the"
           "\"Software\"), to deal in the Software without restriction, including"
           "without limitation the rights to use, copy, modify, merge, publish,"
           "distribute, sublicense, and/or sell copies of the Software, and to"
           "permit persons to whom the Software is furnished to do so, subject to"
           "the following conditions:"
           ""
           "The above copyright notice and this permission notice shall be"
           "included in all copies or substantial portions of the Software."
           ""
           "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"
           "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF"
           "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND"
           "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE"
           "LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION"
           "OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION"
           "WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")))
    (dolist (x text)
      (call-interactively #'comment-dwim)
      (insert x)
      (newline))))
;; insert-license-mit:1 ends here

;; [[file:init-emacs.org::#functions-text-inserting-functions-insert-license-apache][insert-license-apache:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Text Inserting Functions: insert-license-apache
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Text Inserting Functions: insert-license-apache")

(defun insert-license-apache ()
  "Insert Apache license block to be used at the top of code files."
  (interactive "*")
  (let ((text
         `("Apache License"
           ""
           ,(concat "Copyright © " (format-time-string "%Y" nil t) " " user-full-name)
           ""
           "Licensed under the Apache License, Version 2.0 (the \"License\");"
           "you may not use this file except in compliance with the License."
           "You may obtain a copy of the License at"
           ""
           "    http://www.apache.org/licenses/LICENSE-2.0"
           ""
           "Unless required by applicable law or agreed to in writing, software"
           "distributed under the License is distributed on an \"AS IS\" BASIS,"
           "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
           "See the License for the specific language governing permissions and"
           "limitations under the License.")))
    (dolist (x text)
      (call-interactively #'comment-dwim)
      (insert x)
      (newline))))
;; insert-license-apache:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions][External Program Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: External Program Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: External Program Functions")
;; External Program Functions:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-date][insert-date:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-date
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-date")

(defun insert-date ()
  "Insert current date in YYYY-MM-DD format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y-%m-%d")
  (delete-char -1))
;; insert-date:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-datetime][insert-datetime:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-datetime
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-datetime")

(defun insert-datetime ()
  "Insert current date and time in YYYY-MM-DD HH:MM:SS format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y-%m-%d %H:%M:%S")
  (delete-char -1))
;; insert-datetime:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-time][insert-time:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-time
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-time")

(defun insert-time ()
  "Insert current time in HH:MM:SS format."
  (interactive "*")
  (call-process "date" nil t nil "+%H:%M:%S")
  (delete-char -1))
;; insert-time:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-date-stamp][insert-date-stamp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-date-stamp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-date-stamp")

(defun insert-date-stamp ()
  "Insert current date in YYYYMMDD format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y%m%d")
  (delete-char -1))
;; insert-date-stamp:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-fortune][insert-fortune:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-fortune
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-fortune")

(defun insert-fortune (&optional file)
  "Insert a random fortune.

If FILE is non-nil, use that fortune file."
  (interactive "*")
  (call-process "fortune" nil t nil "-a" (if file (shell-quote-argument file) "")))
;; insert-fortune:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-quote][insert-quote:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-quote
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-quote")

(defun insert-quote ()
  "Insert a random quote."
  (interactive "*")
  (insert-fortune (expand-file-name "~/quotes")))
;; insert-quote:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-insert-arch-package-description][insert-arch-package-description:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: insert-arch-package-description
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: insert-arch-package-description")

(defun insert-arch-package-description (package &optional max-length)
  "Insert Arch OS package description for given PACKAGE.

Optional parameter, MAX-LENGTH will truncate the description if
it is longer."
  (interactive "*")
  (cl-labels
      ((command-path (command)
                     (let ((path (string-trim (shell-command-to-string
                                               (format "command -v \"%s\" 2>&1" command)))))
                       (if (string= path "")
                           nil
                         path))))
    (let ((package-manager (or
                            (command-path "yay")
                            (command-path "yaourt")
                            (command-path "pacman")
                            (command-path "pamac"))))
      (if package-manager
          (let ((cmd (format
                      "%s %s %s | sed -n '/^[a-z]*\\/%s /{n;p}' | tr -d '\\n' | tr -s '[:blank:]'"
                      package-manager
                      (if (string= (substring package-manager -5) "pamac")
                          "search -a"
                        "-Ss")
                      package
                      package)))
            (message "Searching for Arch package: %s" cmd)
            (let ((desc (string-trim (shell-command-to-string cmd))))
              (insert (if (and max-length
                               (> (length desc) max-length))
                          (concat (substring desc 0 (- max-length 3)) "...")
                        desc))))
        (user-error "Neither 'pacman', 'yay', 'pamac', or 'yaourt' where found in system path")))))
;; insert-arch-package-description:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-set-arch-package-description][set-arch-package-description:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: set-arch-package-description
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: set-arch-package-description")

;; old version
;; (defun set-arch-package-description ()
;;   "Set Arch OS package description for package install command found on current line."
;;   (interactive "*")
;;   (save-mark-and-excursion
;;     (forward-line 0)
;;     (when (and
;;            (re-search-forward "\\b\\(pacman\\|yay\\|yaourt\\|pamac\\)\\([ \t]+\\(-S\\|install\\|build\\)\\)\\([ \t]+-[^ \t]*\\)*" (point-at-eol) :noerror)
;;            (re-search-forward "[ \t]*\\b\\([^ \t]+\\)\\b" (point-at-eol) :noerror))
;;       (let ((package (match-string-no-properties 1)))
;;         ;; remove any existing description
;;         (if (re-search-forward "#" (point-at-eol) :noerror)
;;             (progn
;;               (delete-region (point) (line-end-position))
;;               (insert " "))
;;           (progn
;;             (end-of-line)
;;             (insert " # ")))
;;         (insert-arch-package-description package)
;;         (align-comments)))))

(defun set-arch-package-description (&optional fast)
  "Set Arch OS package description for package found on current line.

If FAST is non-nil, `org-table-align' will not be called before
or after."
  (interactive "*")
  (save-mark-and-excursion
    (unless fast
      (org-table-align))
    (forward-line 0)
    (let ((package (re-search-forward "|[^|]*|[^|]*|[ \t]*\\([^ \t|]*\\)[ \t]*|[^|]*|[^|]*|" (point-at-eol))))
      (let ((package (match-string-no-properties 1)))
        ;; remove any existing description
        (forward-line 0)
        (re-search-forward "|[^|]*|[^|]*|[^|]*|[^|]*|" (point-at-eol))
        (org-table-blank-field)
        (insert-arch-package-description package 80)
        (unless fast
          (org-table-align))))))
;; set-arch-package-description:1 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-define-word][define-word:2]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: define-word
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: define-word")

(defun define-word (&optional word)
  "Return definition of WORD and put it on the `kill-ring'."
  (interactive "MWord: ")
  (message
   (kill-new
    (with-temp-buffer
      (call-process "trans" nil t nil (shell-quote-argument word))
      (goto-char (point-min))
      (when (re-search-forward "^Examples" nil :noerror)
        (delete-region (1- (line-beginning-position)) (point-max)))
      (buffer-string)))))

(defun define-word-after-spell-check (word)
  "Define WORD after spell checking.

Uses `ispell--run-on-word' to spell check word."
  (interactive "MWord: ")
  (ispell-set-spellchecker-params)
  (ispell-accept-buffer-local-defs)
  (let ((check (ispell--run-on-word word)))
    (cond
     ((or (eq check t)
          (stringp check))
      (define-word word))
     (t
      (let ((buffer (generate-new-buffer-name "*define-word-after-spell-check*")))
        (switch-to-buffer (get-buffer-create buffer))
        (insert word)
        (ispell-word)
        (let ((checked-word (buffer-substring-no-properties (point-min) (point-max))))
          (when (string= buffer (buffer-name))
            (kill-buffer (current-buffer)))
          (define-word checked-word)))))))

(defun define-word-at-point-after-spell-check ()
  "Use `define-word-after-spell-check' to define word at point.

When the region is active, define the marked phrase."
  (interactive)
  (let ((word
         (cond
          ((eq major-mode 'pdf-view-mode)
           (car (pdf-view-active-region-text)))
          ((use-region-p)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
          (t
           (substring-no-properties
            (thing-at-point 'word))))))
    (define-word-after-spell-check word)))
;; define-word:2 ends here

;; [[file:init-emacs.org::#functions-external-program-functions-run-command][run-command:1]]
;;------------------------------------------------------------------------------
;;;; Functions: External Program Functions: run-command
;;------------------------------------------------------------------------------

(init-message 3 "Functions: External Program Functions: run-command")

(defun run-command (command &optional destination)
  "Use `call-process' to run COMMAND with optional DESTINATION.

See `call-process' documentation for instructions on how to use
DESTINATION."
  (let ((destination (or destination 0))
        (parts (split-string command (rx (one-or-more space)))))
    (apply #'call-process `(,(car parts) nil ,destination nil ,@(cdr parts)))))
;; run-command:1 ends here

;; [[file:init-emacs.org::#functions-newer-emacs-functionality-functions][Newer Emacs Functionality Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Newer Emacs Functionality Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Newer Emacs Functionality Functions")
;; Newer Emacs Functionality Functions:1 ends here

;; [[file:init-emacs.org::#functions-newer-emacs-functionality-functions-line-number-at-pos][line-number-at-pos:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Newer Emacs Functionality Functions: line-number-at-pos
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Newer Emacs Functionality Functions: line-number-at-pos")

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.

If POS is nil, use current buffer location."
    (save-mark-and-excursion
      (when pos
        (goto-char pos))
      (1+ (count-lines (point-min) (line-beginning-position))))))
;; line-number-at-pos:1 ends here

;; [[file:init-emacs.org::#functions-newer-emacs-functionality-functions-save-mark-and-excursion][save-mark-and-excursion:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Newer Emacs Functionality Functions: save-mark-and-excursion
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Newer Emacs Functionality Functions: save-mark-and-excursion")

(unless (fboundp 'save-mark-and-excursion)
  (defmacro save-mark-and-excursion (&rest body)
    "Like `save-excursion', but also save and restore the mark state.
This macro does what `save-excursion' did before Emacs 25.1."
    (declare (indent 0) (debug t))
    (let ((saved-marker-sym (make-symbol "saved-marker")))
      `(let ((,saved-marker-sym (save-mark-and-excursion--save)))
         (unwind-protect
             (save-excursion ,@body)
           (save-mark-and-excursion--restore ,saved-marker-sym))))))
;; save-mark-and-excursion:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions][Grep Search Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Grep Search Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Grep Search Functions")
;; Grep Search Functions:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-elisp][grep-elisp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-elisp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-elisp")

(defun grep-elisp (query &optional extended)
  "Grep custom elisp directories for QUERY.

Run `grep' COMMAND, where COMMAND is:

  `grep-default-command' QUERY FILES

FILES is a list of files generated from the following
files/directories:

  `emacs-home-dir' or ~/.emacs.d (if EXTENDED is t)
  `local-init-dir' if it exists
  `local-modules-dir' if it exists

A file matching pattern of `*.el$' is used."
  (interactive "sGrep custom elisp files: ")
  (let (paths
        path
        files)
    (if extended
        (if (and (boundp 'emacs-home-dir)
                 (file-exists-p emacs-home-dir))
            (push emacs-home-dir paths)
          "~/.emacs.d")
      (progn
        (when (and (boundp 'local-init-dir)
                   local-init-dir
                   (file-exists-p local-init-dir))
          (push local-init-dir paths))
        (when (and (boundp 'local-modules-dir)
                   local-modules-dir
                   (file-exists-p local-modules-dir))
          (push local-modules-dir paths))))
    ;; loop through paths
    (while paths
      (setq path (file-truename (expand-file-name (car paths))))
      (setq paths (cdr paths))
      ;; traverse directories
      (if (file-directory-p path)
          ;; loop through files
          (dolist (file (nreverse (directory-files path t)))
            ;; ignore `.' and `..'
            (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
              ;; add directories to paths
              (if (file-directory-p file)
                  (push file paths)
                ;; add elisp file
                (when (string-match "\\.el\\'" file)
                  (push file files)))))
        ;; add file
        (push path files)))
    ;; build command
    (let ((cmd (or grep-command "grep -n -H -i -r -e ")))
      ;; add query
      (setq cmd (concat cmd " \"" query "\""))
      ;; add files
      (dolist (file files)
        (setq cmd (concat cmd " \"" file "\"")))
      ;; execute command using `grep' command
      (grep cmd))))
;; grep-elisp:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-elisp-extended][grep-elisp-extended:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-elisp-extended
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-elisp-extended")

(defun grep-elisp-extended (query)
  "Call `grep-elisp' with QUERY and EXTENDED set to t."
  (interactive "sGrep custom elisp files (extended): ")
  (grep-elisp query t))
;; grep-elisp-extended:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-custom][grep-custom:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-custom
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-custom")

;; grep custom macro
(defmacro grep-custom (dirs match)
  "Return a custom grep function.
DIRS is a list of the directories to search.
MATCH is the file pattern to match."
  `(lambda (query)
     (let ((paths (reverse (quote ,dirs)))
           (match ,match)
           path
           files)
       ;; loop through paths
       (while paths
         (setq path (file-truename (expand-file-name (car paths))))
         (setq paths (cdr paths))
         ;; traverse directories
         (if (file-directory-p path)
             ;; loop through files
             (dolist (file (nreverse (directory-files path t)))
               ;; ignore `.' and `..' and svn directories
               (unless (or (string-match "^\\.\\.?$" (file-name-nondirectory file))
                           (string-match "\\.svn" (file-name-nondirectory file)))
                 ;; add directories to paths
                 (if (file-directory-p file)
                     (push file paths)
                   ;; add files in directories to files
                   (when (or (not match)
                             (string-match match file))
                     (push file files)))))
           ;; add file
           (push path files)))
       ;; build command
       (let ((cmd (or grep-command "grep -n -H -i -e")))
         ;; add query
         (setq cmd (concat cmd " \"" query "\""))
         ;; add files
         (dolist (file files)
           (setq cmd (concat cmd " \"" file "\"")))
         ;; execute command using `grep' command
         (grep cmd)))))
;; examples:
;; (funcall (grep-custom ("~/.profile") ".*") "path")
;; grep-custom:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-custom-generate][grep-custom-generate:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-custom-generate
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-custom-generate")

(defmacro grep-custom-generate (name prompt dirs match)
  "Generate a custom grep function.

NAME is the function name.

PROMPT is displayed if no query is given.

DIRS is a list of the directories to search.

MATCH is the file pattern to match."
  (let ((dirs dirs)
        (match match))
    `(defun ,name (query)
       ,(concat "Grep custom directories for QUERY.\n\n"
                "Run `grep' COMMAND, where COMMAND is:\n\n"
                "`grep-default-command' QUERY FILES\n\n"
                "FILES is a list of files generated from the following\n"
                "files/directories:\n\n"
                (concat "  " (cl-reduce (lambda (x y) (concat x "\n  " y)) dirs) "\n\n")
                "A file matching pattern of `" match "' is used.")
       (interactive ,(concat "s" prompt))
       (funcall (grep-custom ,dirs ,match) query))))
;; grep-custom-generate:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-bin][grep-bin:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-bin
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-bin")

;; grep bin
(grep-custom-generate grep-bin "Grep HOME bin files: " ("~/bin") nil)
;; grep-bin:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-clojure][grep-clojure:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-clojure
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-clojure")

;; grep clojure
(grep-custom-generate grep-clojure "Grep Clojure files: " ("~/dev/clojure") "\\(\\.org$\\|\\.clj$\\)")
;; grep-clojure:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-clisp][grep-clisp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-clisp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-clisp")

;; grep clisp
(grep-custom-generate grep-clisp "Grep CLISP files: " ("~/dev/clisp")  "\\(\\.org$\\|\\.lisp$\\)")
;; grep-clisp:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-emacs-init][grep-emacs-init:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-emacs-init
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-emacs-init")

;; grep emacs initialization
(grep-custom-generate grep-emacs-init "Grep Emacs Initialization files: "
                      ("~/.emacs.d/init.el"
                       "~/.emacs.d/init-emacs.org"
                       "~/.emacs.d/customization.el")
                      "\\(\\.org$\\|\\.el$\\)")
;; grep-emacs-init:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-home-init][grep-home-init:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-home-init
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-home-init")

;; grep home
(grep-custom-generate grep-home-init "Grep Home Initialization files: "
                      ("~/org/init-home.org") "\\.org\\'")
;; grep-home-init:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-org][grep-org:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-org
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-org")

;; grep org
(grep-custom-generate grep-org "Grep Org files: " ("~/org") "\\.org\\'")
;; grep-org:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-python][grep-python:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-python
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-python")

;; grep python
(grep-custom-generate grep-python "Grep Python files: " ("~/dev/python")  "\\(\\.org$\\|\\.py$\\)")
;; grep-python:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-racket][grep-racket:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-racket
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-racket")

;; grep racket
(grep-custom-generate grep-racket "Grep Racket files: " ("~/dev/racket") "\\.rkt\\'")
;; grep-racket:1 ends here

;; [[file:init-emacs.org::#functions-grep-search-functions-grep-web][grep-web:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Grep Search Functions: grep-web
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Grep Search Functions: grep-web")

;; grep web
(grep-custom-generate grep-web "Grep web files: " ("~/web/org") "\\.org\\'")
;; grep-web:1 ends here

;; [[file:init-emacs.org::#functions-tags-file-functions][TAGS File Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: TAGS File Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: TAGS File Functions")
;; TAGS File Functions:1 ends here

;; [[file:init-emacs.org::#functions-tags-file-functions-etags-create][etags-create:1]]
;;------------------------------------------------------------------------------
;;;; Functions: TAGS File Functions: etags-create
;;------------------------------------------------------------------------------

(init-message 3 "Functions: TAGS File Functions: etags-create")

(defun etags-create (&optional local)
  "Create local TAGS file.

If LOCAL is non-nil, visit the new TAGS file locally only.

First an existing TAGS file is searched for going up the
directory path. If none is found, a \"src\" directory is searched
for and, if found, its parent directory is used. Failing that the
user is prompted for the location."
  (interactive)
  (let ((file (or (find-file-updir "TAGS")
                  (when (find-file-updir "src")
                    (file-truename (expand-file-name (concat (find-file-updir "src") "/../TAGS"))))
                  ;; ask for TAGS directory if not found
                  (concat (read-directory-name "Top of source tree: " default-directory) "/TAGS")))
        (extension (or (file-name-extension buffer-file-name)
                       ;; extension defaults to `el'
                       "el"))
        query)
    ;; generate find command parameters
    ;; for c/c++ files, add extra extensions
    (if (member extension '("h" "c" "hpp" "cpp"))
        (setq query "-name \"*.h\" -o -name \"*.c\" -o -name \"*.hpp\" -o -name \"*.cpp\"")
      (setq query (concat "-name \"*." extension "\"")))
    (let ((cmd (concat "find " (file-name-directory file) " " query " -print0 | "
                       "grep -zZv \"/.svn/\" | "
                       "xargs -0 etags -o " file " && "
                       "echo 'Created TAGS file'")))
      (message (format "Running command: %s" cmd))
      ;; create tags file
      (shell-command cmd)
      ;; kill TAGS buffer
      (when (get-buffer "TAGS")
        (kill-buffer "TAGS"))
      ;; set tags file
      (visit-tags-table file local))))
;; etags-create:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions][Code Formatting Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Code Formatting Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Code Formatting Functions")
;; Code Formatting Functions:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-indent-region-or-thing][indent-region-or-thing:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: indent-region-or-thing
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: indent-region-or-thing")

(defun indent-region-or-thing (&optional beg end)
  "Indent selected region, or org code block, or sexp surrounding
point, or line."
  (interactive)
  (let ((case-fold-search t)
        (beg (or beg (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil)))
        (eol (line-end-position)))
    (deactivate-mark)
    (save-window-excursion
      (save-mark-and-excursion
        (save-match-data
          (cond
           ;; region
           ((and beg end)
            (indent-region beg end))
           ;; org code block
           ((and (eq major-mode 'org-mode)
                 (eq (org-element-type (org-element-at-point)) 'src-block))
            (condition-case nil
                (let ((case-fold-search t)
                      (re "^[ \t]*#\\+BEGIN_\\(\\sw+\\)"))
                  (forward-line 0)
                  (unless (looking-at re)
                    (re-search-backward re))
                  (org-indent-block))
              ('error nil)))
           ;; sexp
           ((save-excursion
              (and (beginning-of-defun)
                   (progn
                     (end-of-defun)
                     (>= (point) eol))))
            (beginning-of-defun)
            (indent-region (line-beginning-position) (line-end-position))
            (let* ((bounds (bounds-of-thing-at-point 'sexp))
                   (beg (car bounds))
                   (end (cdr bounds)))
              (indent-region beg end)))
           (t
            (indent-according-to-mode))))))))
;; indent-region-or-thing:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-indent-buffer][indent-buffer:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: indent-buffer
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: indent-buffer")

(defun indent-buffer ()
  "Indent current buffer."
  (indent-region (point-min) (point-max)))
;; indent-buffer:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-find-code-block][find-code-block:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: find-code-block
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: find-code-block")

(defun find-code-block (&optional regexp indent)
  "Find the begin and end of code block containing point.

When run interactively, then begin and end points of the block
are printed in the minibuffer. Otherwise, a list containing them
is returned.

A code block is defined as contiguous lines of text having the
same indentation. So a code block ends when either the
indentation changes or a blank line is reached.

The begin point will be at the start of a line and the end point
will be at the end of a line, unless point is not in a code block
in which case nil is returned for both.

The optional parameter REGEXP is an additional regular expression
to match on. If non-nil, every line in the code block must also
match REGEXP.

If optional parameter INDENT is non-nil then each line will be
indented via `indent-according-to-mode'."
  (interactive "*")
  (let (beg
        end
        (ind 0)
        (blank-line-regexp "^[ \t]*$"))
    (save-mark-and-excursion
      ;; indent if INDENT is t
      ;;(when indent
      ;;  (indent-according-to-mode))
      (setq ind (current-indentation))
      (forward-line 0)
      ;; continue if we are in a code block
      (unless (or
               (looking-at blank-line-regexp)
               (if regexp
                   (not (looking-at regexp))
                 nil))
        ;; move up to first line in block
        (while (and
                (not (bobp))
                (not (looking-at blank-line-regexp))
                (= ind (current-indentation))
                (if regexp
                    (looking-at regexp)
                  t))
          (forward-line -1)
          ;; indent if INDENT is t
          ;;(when indent
          ;;  (indent-according-to-mode))
          (forward-line 0))
        ;; if current line is not part of range, then move down
        (unless (and
                 (not (looking-at blank-line-regexp))
                 (= ind (current-indentation))
                 (if regexp
                     (looking-at regexp)
                   t))
          (forward-line 1))
        (forward-line 0)
        (setq beg (point))
        ;; indent if INDENT is t
        (when indent
          (indent-according-to-mode))
        (setq ind (current-indentation))
        ;; move down to last line in block
        (while (and
                (not (eobp))
                (not (looking-at blank-line-regexp))
                (= ind (current-indentation))
                (if regexp
                    (looking-at regexp)
                  t))
          (forward-line 1)
          ;; indent if INDENT is t
          (when indent
            (indent-according-to-mode))
          (forward-line 0))
        (unless (and
                 (not (looking-at blank-line-regexp))
                 (= ind (current-indentation))
                 (if regexp
                     (looking-at regexp)
                   t))
          (forward-line -1))
        (end-of-line)
        (setq end (point))))
    (if (called-interactively-p 'any)
        (message "%s %s" beg end)
      (list beg end))))
;; find-code-block:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-align-assignment-commands][align-assignment-commands:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: align-assignment-commands
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: align-assignment-commands")

;; TODO: look at `align' and `align-regexp' functions to help with this
(defun align-assignment-commands (&optional indent)
  "Align a block of variable assignment commands.

Match any contiguous block of code (presumably assignment
commands) and align the equal signs.

If optional parameter INDENT is non-nil then each line will be
indented via `indent-according-to-mode'.

Example:

  // assignments
  var1 = value1; // var1
  variable2 = value2; // var2

Becomes:

  // assignments
  var1      = value1;             // var1
  variable2 = value2;             // var2"
  (interactive "*")
  (let* ((range (find-code-block nil indent)) ; range
         (beg (car range))                    ; beginning of range
         (end (cadr range))                   ; end of range
         (pos 0)
         (equal-regexp "[ \t]*=[ \t]*"))
    (save-mark-and-excursion
      ;; if there are lines in range, continue
      (when (> end beg)
        ;; preserve block range
        (save-restriction
          (narrow-to-region beg end)
          ;; preserve search data
          (save-match-data
            ;; move down code block
            (goto-char (point-min))
            (while (< (point) (point-max))
              ;; store farthest equal sign that is not in a comment
              (when (and
                     (re-search-forward equal-regexp (line-end-position) :noerror)
                     (not (equal (get-char-property (point) 'face)
                                 'font-lock-comment-face)))
                ;; remove extra whitespace
                (forward-line 0)
                (re-search-forward equal-regexp (line-end-position))
                (replace-match " = ")
                ;; put point before the equal sign
                (backward-char 2)
                ;; store point if larger than others
                (when (> (- (point) (line-beginning-position)) pos)
                  (setq pos (- (point) (line-beginning-position)))))
              (forward-line 0)
              (forward-line 1))
            ;; move through the block, padding as needed
            (goto-char (point-min))
            (while (< (point) (point-max))
              (when (and
                     (re-search-forward equal-regexp (line-end-position) :noerror)
                     (not (equal (get-char-property (point) 'face)
                                 'font-lock-comment-face)))
                (backward-char 2)
                ;; pad as needed
                (while (< (- (point) (line-beginning-position)) pos)
                  (insert " ")))
              (forward-line 0)
              (forward-line 1))
            ;; handle lines that ends in a comment
            (goto-char (point-min))
            (while (< (point) (point-max))
              (forward-line 0)
              (forward-char (current-indentation))
              ;; if line is not a comment line and line ends in a comment, then
              ;; call comment-indent
              (when (and
                     (not (equal (get-char-property (line-end-position) 'face)
                                 'font-lock-comment-face))
                     (equal (get-char-property (line-end-position) 'face)
                            'font-lock-comment-face))
                (comment-indent))
              (forward-line 1))))))))
;; align-assignment-commands:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-align-assignment-commands-indent][align-assignment-commands-indent:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: align-assignment-commands-indent
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: align-assignment-commands-indent")

(defun align-assignment-commands-indent ()
  (interactive "*")
  (align-assignment-commands t))
;; align-assignment-commands-indent:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-align-declaration-commands][align-declaration-commands:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: align-declaration-commands
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: align-declaration-commands")

;; TODO: fix removal of extra whitespace in block
;; TODO: fix adding whitespace where needed in block
(defun align-declaration-commands (&optional indent)
  "Align a block of variable declaration commands.

If optional parameter INDENT is non-nil then each line will be
indented via `indent-according-to-mode'.

Example:

  // variables
  public Integer i; // int example
  public String s; // string example
  private Integer i2; // int 2
  private String s2; // string 2
  protected Date dte; // date example

Becomes:

  // variables
  public    Integer i;            // int example
  public    String  s;            // string example
  private   Integer i2;           // int 2
  private   String  s2;           // string 2
  protected Date    dte;          // date example"
  (interactive "*")
  (let* ((range (find-code-block nil indent)) ; range
         (beg (car range))                    ; beginning of range
         (end (cadr range))                   ; end of range
         face                                 ; current font face
         face-prev                            ; previous font face
         face-change                          ; has face changed
         (change -1)                          ; current column to align on
         (change-prev -2)                     ; previous column to align on
         (whitespace-regexp "[ \t]+"))        ; regexp to match whitespace
    (save-mark-and-excursion
      ;; if there are lines in range, continue
      (when (> end beg)
        ;; preserve block range
        (save-restriction
          (narrow-to-region beg end)
          ;; preserve search data
          (save-match-data
            ;; remove extra whitespace
            (goto-char (point-min))
            (while (< (point) (point-max))
              (forward-char (current-indentation))
              (while (re-search-forward whitespace-regexp (line-end-position) :noerror)
                (replace-match " "))
              (forward-line 0)
              (forward-line 1))
            ;; loop until all face changes have been analyzed
            (while (> change change-prev)
              ;; update change-prev
              (setq change-prev change)
              ;; set change-prev to indent on first pass
              (when (< change-prev 0)
                (setq change-prev (current-indentation)))
              ;; clear face
              (setq face nil)
              ;; clear face-prev
              (setq face-prev nil)
              ;; goto start of range
              (goto-char (point-min))
              ;; loop through all lines in range
              (while (< (point) (point-max))
                ;; goto start of line + indentation
                (forward-line 0)
                (forward-char (current-indentation))
                ;; ignore comment lines
                (unless (member (get-char-property (point) 'face)
                                (list 'font-lock-comment-face
                                      'nxml-comment-delimiter-face
                                      'nxml-comment-content-face))
                  ;; goto start of line + change-prev
                  (forward-line 0)
                  (forward-char change-prev)
                  ;; get face-prev of first non-whitespace character
                  (unless face-prev
                    (save-mark-and-excursion
                      (while (and (< (point) (line-end-position))
                                  (looking-at whitespace-regexp))
                        (forward-char 1))
                      (when (< (point) (line-end-position))
                        (setq face-prev (get-char-property (point) 'face)))))
                  ;; move forward until we hit whitespace, tracking any face change
                  (setq face-change nil)
                  (while (and (< (point) (line-end-position))
                              (not (looking-at whitespace-regexp)))
                    (when (not (equal face-prev (get-char-property (point) 'face)))
                      (setq face-change t))
                    (forward-char 1))
                  ;; move forward until a non-whitespace character is reached that
                  ;; changes the face
                  (while (and (< (point) (line-end-position))
                              (or
                               (looking-at whitespace-regexp)
                               (and (not face-change)
                                    (equal face-prev (get-char-property (point) 'face)))))
                    (forward-char 1))
                  ;; if the face changed and it is farther than the current
                  ;; change, set the face and change value; face is set on the
                  ;; first pass after which a match is expected
                  (when (and (< (point) (line-end-position))
                             (< change (- (point) (line-beginning-position)))
                             (if face
                                 (equal face (get-char-property (point) 'face))
                               t))
                    ;; only set face on first pass
                    (unless face
                      (setq face (get-char-property (point) 'face)))
                    (setq change (- (point) (line-beginning-position)))))
                ;; move to next line and continue
                (forward-line 1))
              ;; if a face change is found, align lines
              (when (> change change-prev)
                ;; goto start of range
                (goto-char (point-min))
                ;; loop through all lines in range
                (while (< (point) (point-max))
                  ;; goto start of line + indentation
                  (forward-line 0)
                  (forward-char (current-indentation))
                  ;; ignore comment lines
                  (unless (member (get-char-property (point) 'face)
                                  (list 'font-lock-comment-face
                                        'nxml-comment-delimiter-face
                                        'nxml-comment-content-face))
                    ;; goto start of line + change-prev
                    (forward-line 0)
                    (forward-char change-prev)
                    ;; find start of face change
                    ;; move forward until whitespace is reached
                    (while (and (< (point) (line-end-position))
                                (not (looking-at whitespace-regexp)))
                      (forward-char 1))
                    ;; move forward until a non-whitespace character is reached
                    ;; that matches the face change
                    (while (and (< (point) (line-end-position))
                                (or
                                 (looking-at whitespace-regexp)
                                 (not (equal face (get-char-property (point) 'face)))))
                      (forward-char 1))
                    ;; space as needed
                    (while (< (- (point) (line-beginning-position)) change)
                      ;; TODO: as a precaution check that previous character is
                      ;; whitespace
                      (insert " ")))
                  ;; move to next line and continue
                  (forward-line 1))))
            ;; handle lines that ends in a comment
            (goto-char (point-min))
            (while (< (point) (point-max))
              (forward-line 0)
              (forward-char (current-indentation))
              ;; if line is not a comment line and line ends in a comment, then
              ;; call comment-indent
              (when (and
                     (not (equal (get-char-property (line-end-position) 'face)
                                 'font-lock-comment-face))
                     (equal (get-char-property (line-end-position) 'face)
                            'font-lock-comment-face))
                (comment-indent))
              (forward-line 1))))))))
;; align-declaration-commands:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-align-declaration-commands-indent][align-declaration-commands-indent:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: align-declaration-commands-indent
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: align-declaration-commands-indent")

(defun align-declaration-commands-indent ()
  (interactive "*")
  (align-declaration-commands t))
;; align-declaration-commands-indent:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-align-comments][align-comments:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: align-comments
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: align-comments")

(defun align-comments (&optional beg end)
  "Align a block of commented lines.

If region is not given, one of the following blocks will be
used (tried in order):

- Org source block
- Symbolic expression
- Current line

Example:

  // variables
  public Integer i; // int example
  public String s; // string example
  private Integer i2; // int 2
  private String s2; // string 2
  protected Date dte; // date example

Becomes:

  // variables
  public Integer i;             // int example
  public String s;              // string example
  private Integer i2;           // int 2
  private String s2;            // string 2
  protected Date dte;           // date example"
  (interactive "*")
  (let ((case-fold-search t)
        (comment-regexp
         (concat "\\(\\s-*\\)"
                 comment-start
                 (if (string= comment-start ";") " " "")))
        (point (point))
        (beg (or beg (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil))))
    (deactivate-mark)
    (save-window-excursion
      (save-mark-and-excursion
        (save-match-data
          (cond
           ;; region
           ((and beg end)
            (align-regexp beg end comment-regexp))
           ;; org code block
           ((and (eq major-mode 'org-mode)
                 (eq (org-element-type (org-element-at-point)) 'src-block))
            (condition-case nil
                (let ((case-fold-search t)
                      (re "^\\([ \t]*\\)#\\+BEGIN_\\(\\sw+\\)"))
                  (forward-line 0)
                  (unless (looking-at re)
                    (re-search-backward re))
                  (re-search-forward (concat "^\\([ \t]*\\)#\\+END_" (match-string 2))))
              ('error nil))
            (org-babel-do-in-edit-buffer (align-regexp (point-min) (point-max) comment-regexp)))
           ;; sexp
           ((beginning-of-defun)
            (let* ((bounds (bounds-of-thing-at-point 'sexp))
                   (beg (car bounds))
                   (end (cdr bounds)))
              (align-regexp beg end comment-regexp)))
           (t
            (align-regexp (point-at-bol) (point-at-eol) comment-regexp))))))
    (goto-char point)))
;; align-comments:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-java-toggle-comment-type][java-toggle-comment-type:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: java-toggle-comment-type
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: java-toggle-comment-type")

(defun java-toggle-comment-type ()
  "Toggle between single and multi-line Java/C comments.

Examples:

                  /*
  // comment  =>   * comment
                   */

  /*
   * comment  =>  // comment
   */"
  (interactive "*")
  (let ((indentation-regexp "^[ \t]*")
        (empty-line-regexp "^[ \t]*$")
        (single-line-comment-regexp "^[ \t]*// ")
        (multi-line-comment-regexp "^[ \t]*\\* ")
        (multi-line-comment-begin-regexp "^[ \t]*/\\*")
        (multi-line-comment-end-regexp "^[ \t]*\\*/")
        (class-regexp "\\bclass\\b"))
    (save-mark-and-excursion
      (save-match-data
        (forward-line 0)
        (if (looking-at single-line-comment-regexp)
            ;; convert single line comments into a multi-line comment
            (let ((beg (progn
                         (while (and (not (bobp))
                                     (looking-at single-line-comment-regexp))
                           (forward-line -1))
                         (when (not (looking-at single-line-comment-regexp))
                           (forward-line 1))
                         (line-beginning-position)))
                  (end (progn
                         (while (and (not (eobp))
                                     (looking-at single-line-comment-regexp))
                           (forward-line 1))
                         (when (not (looking-at single-line-comment-regexp))
                           (forward-line -1))
                         (line-end-position)))
                  (space (make-string (- (re-search-forward indentation-regexp) (line-beginning-position)) ? ))
                  (class (progn
                           (forward-line 0)
                           (forward-line 1)
                           (while (looking-at empty-line-regexp)
                             (forward-line 1))
                           (looking-at class-regexp))))
              (goto-char beg)
              ;;(insert (concat space "/*" (if class "*" "")))
              (insert (concat space "/*"))
              (newline)
              (while (re-search-forward single-line-comment-regexp end :noerror)
                (replace-match (concat space " * ")))
              (goto-char (line-end-position))
              (newline)
              (insert (concat space " */")))
          ;; convert multi-line comment to single line comments
          (let ((beg (progn
                       (while (and (not (bobp))
                                   (or (looking-at multi-line-comment-regexp)
                                       (looking-at multi-line-comment-end-regexp)))
                         (forward-line -1))
                       (if (looking-at multi-line-comment-begin-regexp)
                           (line-beginning-position)
                         nil)))
                (end (progn
                       (while (and (not (eobp))
                                   (or (looking-at multi-line-comment-regexp)
                                       (looking-at multi-line-comment-begin-regexp)))
                         (forward-line 1))
                       (if (looking-at multi-line-comment-end-regexp)
                           (line-end-position)
                         nil)))
                (space (make-string (- (re-search-forward indentation-regexp) (line-beginning-position) 1) ? )))
            (when (and beg end)
              (goto-char beg)
              (delete-region (point) (progn (forward-line 1) (point)))
              (while (re-search-forward multi-line-comment-regexp end :noerror)
                (replace-match (concat space "// ")))
              (goto-char (line-end-position))
              (delete-region (point) (progn (forward-line 1) (goto-char (line-end-position)) (point))))))))))

(init-message 3 "Functions: Code Formatting Functions: c-toggle-comment-type")

(defalias 'c-toggle-comment-type 'java-toggle-comment-type)
;; java-toggle-comment-type:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-java-remove-comments][java-remove-comments:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: java-remove-comments
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: java-remove-comments")

(defun java-remove-comments (&optional beg end)
  "Remove all Java comments from buffer or region."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (narrow-to-region beg end)
        (let (quote                     ; whether inside a quote
              escape                    ; whether inside an escape
              comment)                  ; whether inside a block comment
          (goto-char (point-min))
          (while (not (eobp))
            (forward-char 1)
            (let ((char (preceding-char))
                  (next-char (following-char)))
              (cl-case char
                (?\"                    ; quote
                 (unless (or comment escape)
                   (setq quote (not quote))))
                (?\\                    ; escape
                 (unless comment
                   (setq escape (not escape))))
                (t
                 (if comment            ; inside block comment
                     (when (and (char-equal char ?*) (char-equal next-char ?/)) ; check for comment end
                       (forward-char 1)
                       (when (and (eolp) (not (eobp)))
                         (forward-char 1))
                       (delete-region comment (point))
                       (setq comment nil))
                   (unless (or quote escape) ; check for comment start
                     (cond
                      ;; inline comment
                      ((and (char-equal char ?/) (char-equal next-char ?/))
                       (forward-char -1)
                       (while (and (not (bolp))
                                   (or (char-equal (preceding-char) ? )
                                       (char-equal (preceding-char) ?\t)))
                         (forward-char -1))
                       (let ((mark (point)))
                         (goto-char (line-end-position))
                         (when (not (eobp))
                           (forward-char 1))
                         (delete-region mark (point))))
                      ;; block comment
                      ((and (char-equal char ?/) (char-equal next-char ?*))
                       (setq comment (1- (point))))))))))))))))
;; java-remove-comments:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-lisp-to-camel-case][lisp-to-camel-case:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: lisp-to-camel-case
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: lisp-to-camel-case")

(defun lisp-to-camel-case ()
  "Convert word under point from lisp notation to camel case notation."
  (interactive "*")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds)))
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "-" nil :noerror)
            (let ((p (point)))
              (capitalize-word 1)
              (goto-char p)))
          (goto-char (point-min))
          (while (re-search-forward "-" nil :noerror)
            (replace-match "")))))))
;; lisp-to-camel-case:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-camel-case-to-lisp][camel-case-to-lisp:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: camel-case-to-lisp
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: camel-case-to-lisp")

(defun camel-case-to-lisp ()
  "Convert word under point from camel case notation to lisp notation."
  (interactive "*")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds))
         (case-fold-search nil))
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[A-Z]" nil :noerror)
            (forward-char -1)
            (insert "-")
            (forward-char 1))
          (goto-char (point-min))
          (downcase-word 1))))))
;; camel-case-to-lisp:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-c-pretty-print][c-pretty-print:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: c-pretty-print
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: c-pretty-print")

(defun c-pretty-print (&optional beg end)
  "Pretty-print selected region."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          ;; remove tabs
          (remove-tabs)
          ;; remove trailing blanks
          (remove-trailing-blanks)
          ;; remove extra spaces from functions
          (goto-char (point-min))
          (while (re-search-forward "([ \t]*" nil :noerror)
            (replace-match "("))
          (goto-char (point-min))
          (while (re-search-forward "[ \t]*)" nil :noerror)
            (replace-match ")"))
          ;; remove extra spaces from template references
          ;;(goto-char (point-min))
          ;;(while (re-search-forward "<[ \t]*\\(.*?\\)[ \t]*>" nil :noerror)
          ;;  (replace-match "<\\1>"))
          ;; add spaces after commas
          (goto-char (point-min))
          (while (re-search-forward "\\,\\([^ ]\\)" nil :noerror)
            (replace-match ", \\1"))
          ;; remove double or more spaces
          (goto-char (point-min))
          (while (re-search-forward "\\([^ \.]\\)  +" nil :noerror)
            (replace-match "\\1 "))
          ;; remove double or more blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil :noerror)
            (replace-match "\n\n"))
          ;; remove blank line before block start
          (goto-char (point-min))
          (while (re-search-forward "\n\n[ \t]*{" nil :noerror)
            (replace-match "\n{")
            (indent-according-to-mode))
          ;; indent buffer
          (indent-region (point-min) (point-max) nil))))))
;; c-pretty-print:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-ruby-pretty-print][ruby-pretty-print:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: ruby-pretty-print
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: ruby-pretty-print")

(defun ruby-pretty-print (&optional beg end)
  "Pretty-print selected region."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          ;; remove tabs
          (remove-tabs)
          ;; remove trailing blanks
          (remove-trailing-blanks)
          ;; remove extra spaces from functions
          (goto-char (point-min))
          (while (re-search-forward "([ \t]*\\(.*?\\)[ \t]*)" nil :noerror)
            (replace-match "(\\1)"))
          ;; remove extra spaces from variable references
          (goto-char (point-min))
          (while (re-search-forward "#{[ \t]*\\(.*?\\)[ \t]*}" nil :noerror)
            (replace-match "#{\\1}"))
          ;; remove extra spaces from hash references
          (goto-char (point-min))
          (while (re-search-forward "\\[[ \t]*\\(.*?\\)[ \t]*\\]" nil :noerror)
            (replace-match "[\\1]"))
          ;; remove extra spaces from each clauses
          (goto-char (point-min))
          (while (re-search-forward "|[ \t]*\\(.*?\\)[ \t]*|" nil :noerror)
            (replace-match "|\\1|"))
          ;; add spaces after commas
          (goto-char (point-min))
          (while (re-search-forward "\\,\\([^ ]\\)" nil :noerror)
            (replace-match ", \\1"))
          ;; remove double or more spaces
          (goto-char (point-min))
          (while (re-search-forward "\\([^ \.]\\)  +" nil :noerror)
            (replace-match "\\1 "))
          ;; remove double or more blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil :noerror)
            (replace-match "\n\n"))
          ;; indent buffer
          (indent-region (point-min) (point-max) nil))))))
;; ruby-pretty-print:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-java-pretty-print][java-pretty-print:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: java-pretty-print
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: java-pretty-print")

(defun java-pretty-print (&optional beg end)
  "Pretty-print selected region."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max)))))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          ;; remove tabs
          (remove-tabs)
          ;; remove trailing blanks
          (remove-trailing-blanks)
          ;; remove extra spaces from functions
          (goto-char (point-min))
          (while (re-search-forward "([ \t]*" nil :noerror)
            (replace-match "("))
          (goto-char (point-min))
          (while (re-search-forward "[ \t]*)" nil :noerror)
            (replace-match ")"))
          ;; add spaces after commas
          (goto-char (point-min))
          (while (re-search-forward "\\,\\([^ ]\\)" nil :noerror)
            (replace-match ", \\1"))
          ;; add spaces after special forms keywords names
          (dolist (name '("catch" "else" "for" "if" "return" "switch" "while"))
            (goto-char (point-min))
            (while (re-search-forward (concat name "(") nil :noerror)
              (replace-match (concat name " ("))))
          ;; join "} CONNECTOR {" into one line
          (dolist (name '("catch" "else"))
            (goto-char (point-min))
            (while (re-search-forward (concat "}[ \t\n]*" name) nil :noerror)
              (replace-match (concat "} " name))
              (indent-according-to-mode)))
          ;; remove double or more spaces
          (goto-char (point-min))
          (while (re-search-forward "\\([^ \.]\\)  +" nil :noerror)
            (replace-match "\\1 "))
          ;; remove double or more blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil :noerror)
            (replace-match "\n\n"))
          ;; remove blank line before block start
          (goto-char (point-min))
          (while (re-search-forward "\n\n[ \t]*{" nil :noerror)
            (replace-match "\n{")
            (indent-according-to-mode))
          ;; indent buffer
          (indent-region (point-min) (point-max) nil))))))
;; java-pretty-print:1 ends here

;; [[file:init-emacs.org::#functions-code-formatting-functions-xml-pretty-print][xml-pretty-print:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Formatting Functions: xml-pretty-print
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Formatting Functions: xml-pretty-print")

(defun xml-pretty-print (&optional beg end)
  "Pretty-print selected region."
  (interactive "*")
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        (mode major-mode))
    (deactivate-mark)
    (save-mark-and-excursion
      (save-restriction
        (save-match-data
          (narrow-to-region beg end)
          (xml-mode)
          (let ((xml-eol "[ \t]*\n[ \t]*")
                (xml-tag-end-regexp ">")
                (xml-close-tag-regexp "</")
                (xml-block-regexp "<[^>]*>[^<]*</[^>]*>"))
            ;; remove all existing EOL characters
            (goto-char (point-min))
            (while (re-search-forward xml-eol nil :noerror)
              (replace-match ""))
            ;; move down code adding newlines were needed
            (goto-char (point-min))
            (while (re-search-forward xml-tag-end-regexp nil :noerror)
              (insert "\n")
              (when (and
                     (looking-at xml-block-regexp)
                     (not (looking-at xml-close-tag-regexp)))
                (re-search-forward xml-block-regexp nil :noerror)
                (forward-char -1)))
            ;; indent buffer
            (indent-region (point-min) (point-max)))
          (funcall mode))))))
;; xml-pretty-print:1 ends here

;; [[file:init-emacs.org::#functions-code-inserting-functions][Code Inserting Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Code Inserting Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Code Inserting Functions")
;; Code Inserting Functions:1 ends here

;; [[file:init-emacs.org::#functions-code-inserting-functions-project-euler-insert-template][project-euler-insert-template:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Inserting Functions: project-euler-insert-template
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Inserting Functions: project-euler-insert-template")

(defun project-euler-insert-template (num &optional count)
  "Insert a Project Euler template for NUM.

If optional COUNT is given, repeat up to NUM+COUNT-1."
  (let ((buffer "project-euler.lisp"))
    (unless (string= (buffer-name) buffer)
      (user-error "Current buffer is not: %s" buffer))
    (dotimes (x (or count 1))
      (let ((strnum (format "%03d" (+ num x))))
        (save-mark-and-excursion
          (save-match-data
            (goto-char (point-min))
            (re-search-forward "^;;; Template")
            (forward-line 0)
            (forward-line 3)
            (let ((beg (point)))
              (forward-sexp 2)
              (forward-line 0)
              (forward-line 2)
              (let ((template (replace-regexp-in-string
                               "\\?" strnum
                               (buffer-substring-no-properties beg (point)))))
                (search-backward-regexp "^;;; New Problems")
                (forward-line -1)
                (insert template)))))))))
;; project-euler-insert-template:1 ends here

;; [[file:init-emacs.org::#functions-code-inserting-functions-insert-tree][insert-tree:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Code Inserting Functions: insert-tree
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Code Inserting Functions: insert-tree")

(defun insert-tree (leaves padding)
  "Insert binary tree with LEAVES at the bottom and PADDING on the left."
  (let ((size (* 3 (expt 2 leaves)))
        (pad (cl-do* ((l 1 (1+ l))
                      (pad 0 (+ pad (* 3 (expt 2 l)))))
                 ((> l leaves) pad))))
    (cl-do ((s size (1- s)))
        ((zerop s))
      (let ((i ""))
        (dotimes (_ (+ padding s))
          (setq i (concat i " ")))
        (setq i (concat i "/"))
        (dotimes (_ (* (- size s) 2))
          (setq i (concat i " ")))
        (setq i (concat i "\\"))
        (insert i)
        (newline)))))

;;                                              /\
;;                                             /  \
;;                                            /    \
;;                                           /      \
;;                                          /        \
;;                                         /          \
;;                                        /            \
;;                                       /              \
;;                                      /                \
;;                                     /                  \
;;                                    /                    \
;;                                 1 /                      \ 1
;;                                  /                        \
;;                                 /                          \
;;                                /                            \
;;                               /                              \
;;                              /                                \
;;                             /                                  \
;;                            /                                    \
;;                           /                                      \
;;                          /                                        \
;;                         /                                          \
;;                        /                                            \
;;                       /                                              \
;;                      /\                                              /\
;;                     /  \                                            /  \
;;                    /    \                                          /    \
;;                   /      \                                        /      \
;;                  /        \                                      /        \
;;               1 /          \ 2                                1 /          \ 2
;;                /            \                                  /            \
;;               /              \                                /              \
;;              /                \                              /                \
;;             /                  \                            /                  \
;;            /                    \                          /                    \
;;           /                      \                        /                      \
;;          /\                      /\                      /\                      /\
;;         /  \                    /  \                    /  \                    /  \
;;      1 /    \ 3              2 /    \ 6              1 /    \ 3              2 /    \ 6
;;       /      \                /      \                /      \                /      \
;;      /        \              /        \              /        \              /        \
;;     /          \            /          \            /          \            /          \
;;    /\          /\          /\          /\          /\          /\          /\          /\
;; 1 /  \ 4    3 /  \ 12   2 /  \ 8    6 /  \ 24   1 /  \ 4    3 /  \ 12   2 /  \ 8    6 /  \ 24
;;  /    \      /    \      /    \      /    \      /    \      /    \      /    \      /    \
;; insert-tree:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions][Esoteric Functions:1]]
;;------------------------------------------------------------------------------
;;; Functions: Esoteric Functions
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Esoteric Functions")
;; Esoteric Functions:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-fahrenheit-celsius-conversions][Fahrenheit/Celsius Conversions:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: Fahrenheit/Celsius Conversions
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Esoteric Functions: Fahrenheit/Celsius Conversions")
;; Fahrenheit/Celsius Conversions:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-fahrenheit-celsius-conversions-fahrenheit-to-celsius][fahrenheit-to-celsius:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: fahrenheit-to-celsius
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: fahrenheit-to-celsius")

(defun fahrenheit-to-celsius (deg)
  "Convert fahrenheit degrees to celsius."
  (/ (* (- deg 32.0) 5.0) 9.0))
;; fahrenheit-to-celsius:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-fahrenheit-celsius-conversions-fahrenheit-to-celsius-query][fahrenheit-to-celsius-query:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: fahrenheit-to-celsius-query
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: fahrenheit-to-celsius-query")

(defun fahrenheit-to-celsius-query (deg)
  "Prompt user for fahrenheit degrees to convert to celsius."
  (interactive "nFahrenheit degrees: ")
  (let ((cel (fahrenheit-to-celsius deg)))
    (when (called-interactively-p 'any)
      (message "Celsius degrees: %s" cel))
    cel))
;; fahrenheit-to-celsius-query:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-fahrenheit-celsius-conversions-celsius-to-fahrenheit][celsius-to-fahrenheit:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: celsius-to-fahrenheit
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: celsius-to-fahrenheit")

(defun celsius-to-fahrenheit (deg)
  "Convert celsius degrees to fahrenheit."
  (+ (* (/ deg 5.0) 9.0) 32.0))
;; celsius-to-fahrenheit:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-fahrenheit-celsius-conversions-celsius-to-fahrenheit-query][celsius-to-fahrenheit-query:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: celsius-to-fahrenheit-query
;;------------------------------------------------------------------------------

(init-message 4 "Functions: Esoteric Functions: Fahrenheit/Celsius Conversions: celsius-to-fahrenheit-query")

(defun celsius-to-fahrenheit-query (deg)
  "Prompt user for celsius degrees to convert to fahrenheit."
  (interactive "nCelsius degrees: ")
  (let ((cel (celsius-to-fahrenheit deg)))
    (when (called-interactively-p 'any)
      (message "Fahrenheit degrees: %s" cel))
    cel))
;; celsius-to-fahrenheit-query:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-base-conversion][base-conversion:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: base-conversion
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Esoteric Functions: base-conversion")

(defun base-conversion (base-from base-to num)
  "Convert NUM from BASE-FROM to BASE-TO."
  (interactive)
  ;; first get base 10 number
  (let ((num
         (cl-do* ((n (mod num 10) (mod num 10))
                  (num (/ num 10) (/ num 10))
                  (pos 1 (* pos base-from))
                  (result (* pos n) (+ result (* pos n))))
             ((zerop num) result))))
    ;; now convert to base-to
    (cl-do* ((n (mod num base-to) (mod num base-to))
             (num (/ num base-to) (/ num base-to))
             (pos 1 (* pos base-to))
             (result (* pos n) (+ result (* pos n))))
        ((zerop num) result))))
;; base-conversion:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-ldif-update-xml][ldif-update-xml:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: ldif-update-xml
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Esoteric Functions: ldif-update-xml")

(defun ldif-update-xml ()
  "Update an LDIF node with the base64 encoded value of an XML block.

Must be run from the attribute being updated, which must be in
the form of `attribute::'."
  (interactive "*")
  (let (beg
        end
        attr
        block
        (blank-line-regexp "^[ \t]*$"))
    (save-mark-and-excursion
      (save-match-data
        ;; save attr position
        (forward-line 0)
        (when (search-forward "::" (line-end-position) :noerror)
          (setq attr (point))
          ;; find xml block
          (when (search-forward "<?xml" nil :noerror)
            ;; get xml block range (fron starting <?xml to first blank line)
            ;; TODO: make this better by search for starting and ending xml
            ;; nodes
            (setq beg (line-beginning-position))
            (while (and
                    (not (eobp))
                    (not (looking-at blank-line-regexp)))
              (forward-line 1))
            (forward-line -1)
            (end-of-line)
            (setq end (point))
            ;; copy block to temp buffer
            (setq block (buffer-substring beg end))
            ;; following two lines used for debugging
            ;;(save-current-buffer
            ;;  (set-buffer (get-buffer-create "*temp*"))
            (with-temp-buffer
              (insert block)
              (goto-char (point-min))
              ;; if block is commented out, then uncomment
              (search-forward "<?xml")
              (forward-line 0)
              (when (char-equal (char-after (point)) ?#)
                (while (char-equal (char-after (point)) ?#)
                  ;; remove leading #
                  (delete-char 1)
                  ;; remove single space if exists
                  (when (char-equal (char-after (point)) ? )
                    (delete-char 1))
                  (forward-line 1)
                  (forward-line 0)))
              ;; remove comment lines
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*#" (point-max) :noerror)
                (delete-region (line-beginning-position) (line-end-position))
                (unless (eobp)
                  (delete-char 1)))
              ;; base64 encode block
              (base64-encode-region (point-min) (point-max))
              ;; append every line with a space
              (goto-char (point-min))
              (while (not (eobp))
                (forward-line 0)
                (insert " ")
                (forward-line 1))
              ;; copy encoded block
              (setq block (buffer-substring (point-min) (point-max))))
            ;; delete attr data
            (goto-char attr)
            (delete-region (point) (line-end-position))
            (forward-line 1)
            (forward-line 0)
            (while (char-equal (char-after (point)) ? )
              (delete-region (line-beginning-position) (line-end-position))
              (delete-char 1))
            ;; paste encoded block
            (goto-char attr)
            (insert block)))))))
;; ldif-update-xml:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-lisp-to-racket-conversion][lisp-to-racket-conversion:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: lisp-to-racket-conversion
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Esoteric Functions: lisp-to-racket-conversion")

(defun lisp-to-racket-conversion ()
  "Find and convert next Lisp function header into Racket format."
  (interactive "*")
  (re-search-forward "^[ \t]*\(\\(defun\\)")
  (backward-kill-word 1)
  (let ((indent (buffer-substring-no-properties (line-beginning-position) (1- (point)))))
    (insert "define")
    (forward-char 1)
    (insert "(")
    (let ((fn-name-start (point)))
      (re-search-forward " ")
      (let ((fn-name
             (replace-regexp-in-string
              "-" " "
              (buffer-substring-no-properties fn-name-start (1- (point))))))
        (delete-char 1)
        (forward-line 0)
        (forward-line 1)
        (let ((start (point)))
          (re-search-forward "^[ \t]*\"" (line-end-position))
          (let ((str-start (point))
                (escape nil))
            ;; find end of quoted string
            (while (not (and (not escape) (char-equal (char-after) ?\")))
              (when (char-equal (char-after) ?\\)
                (setq escape (not escape)))
              (forward-char 1))
            (let ((str (replace-regexp-in-string
                        (concat "\n" indent)
                        (concat "\n" indent ";; ")
                        (buffer-substring-no-properties str-start (point)))))
              (forward-line 0)
              (forward-line 1)
              (delete-region start (point))
              (forward-line -2)
              (end-of-line)
              (insert (concat "\n" indent ";;------------------------------------------------------------------------------\n"))
              (insert (concat indent ";;;; " fn-name))
              (titleize-line-or-region)
              (insert (concat "\n" indent ";;\n" indent ";; " str "\n"))
              (insert (concat indent ";;------------------------------------------------------------------------------\n\n"))
              (forward-line 0)
              (forward-line 2))))))))
;; lisp-to-racket-conversion:1 ends here

;; [[file:init-emacs.org::#functions-esoteric-functions-integer-to-roman-numerals][integer-to-roman-numerals:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Esoteric Functions: integer-to-roman-numerals
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Esoteric Functions: integer-to-roman-numerals")

(defun integer-to-roman-numerals (num)
  "Return romain numeral version of given integer NUM.

Roman numerals use I, V, X, L, C, D, and M, standing respectively
for 1, 5, 10, 50, 100, 500, and 1,000."
  (interactive "NInteger number: ")
  (when (< num 0)
    (user-error "NUM must be 1 or greater"))
  (when (>= num 5000)
    (user-error "NUM must be less than 5,000"))
  (let ((roman))
    (cl-labels
        ((convert (num)
                  (cond
                   ((>= num 1000)
                    (push "M" roman)
                    (- num 1000))
                   ((>= num 900)
                    (push "CM" roman)
                    (- num 900))
                   ((>= num 500)
                    (push "D" roman)
                    (- num 500))
                   ((>= num 400)
                    (push "CD" roman)
                    (- num 400))
                   ((>= num 100)
                    (push "C" roman)
                    (- num 100))
                   ((>= num 90)
                    (push "XC" roman)
                    (- num 90))
                   ((>= num 50)
                    (push "L" roman)
                    (- num 50))
                   ((>= num 40)
                    (push "XL" roman)
                    (- num 40))
                   ((>= num 10)
                    (push "X" roman)
                    (- num 10))
                   ((>= num 9)
                    (push "IX" roman)
                    (- num 9))
                   ((>= num 5)
                    (push "V" roman)
                    (- num 5))
                   ((>= num 4)
                    (push "IV" roman)
                    (- num 4))
                   ((>= num 1)
                    (push "I" roman)
                    (- num 1)))))
      (while (> num 0)
        (setq num (convert num)))
      (mapconcat 'identity (nreverse roman) ""))))
;; integer-to-roman-numerals:1 ends here

;; [[file:init-emacs.org::#functions-table-generators][Table Generators:1]]
;;------------------------------------------------------------------------------
;;; Functions: Table Generators
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Table Generators")
;; Table Generators:1 ends here

;; [[file:init-emacs.org::#functions-table-generators-ascii-table][ascii-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Table Generators: ascii-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Table Generators: ascii-table")

(defun ascii-table ()
  "Display a table of the ASCII characters from 0 to 254 in a buffer."
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
    (buffer-disable-undo)
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
;;   "Display the ASCII characters from 0 to 254 in a buffer."
;;   (interactive)
;;   (switch-to-buffer "*ASCII Table*")
;;   (buffer-disable-undo)
;;   (erase-buffer)
;;   (dotimes (x 255)
;;     (insert (format "%4d %c\n" x x)))
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))
;; ascii-table:1 ends here

;; [[file:init-emacs.org::#functions-table-generators-http-status-code-table][http-status-code-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Table Generators: http-status-code-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Table Generators: http-status-code-table")

(defun http-status-code-table ()
  "Display a table of the HTTP status codes in a buffer."
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
    (buffer-disable-undo)
    (let (buffer-read-only)
      (erase-buffer)
      (insert "CODE   DESCRIPTION\n")
      (insert "----   -----------\n")
      (mapc (lambda (c) (insert (format "%4d   %s\n" (car c) (cdr c))))
            status-codes))
    (setq buffer-read-only t)
    (goto-char (point-min))))
;; http-status-code-table:1 ends here

;; [[file:init-emacs.org::#functions-table-generators-powers-of-two-table][powers-of-two-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Table Generators: powers-of-two-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Table Generators: powers-of-two-table")

(defun powers-of-two-table ()
  "Display a table of the first 64 Powers of two in a buffer."
  (interactive)
  (switch-to-buffer "*Powers of Two Table*")
  (buffer-disable-undo)
  (let (buffer-read-only)
    (erase-buffer)
    (insert "POWER   DECIMAL                HEX\n")
    (insert "-----   --------------------   -----------------\n")
    (dotimes (x 65)
      (if (<= x 60)
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

;; [[file:init-emacs.org::#functions-table-generators-trigonometry-table][trigonometry-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Table Generators: trigonometry-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Table Generators: trigonometry-table")

(defun trigonometry-table ()
  "Display a table of trigonometry values."
  (interactive)
  (switch-to-buffer "*Trigonometry Table*")
  (buffer-disable-undo)
  (let (buffer-read-only)
    (erase-buffer)
    (insert "RADIANS   SIN      COS      TAN       ARCSIN   ARCCOS   ARCTAN\n")
    (insert "-------   ------   ------   -------   ------   ------   -------\n")
    (dotimes (x 81)
      (let ((n (* float-pi (/ (- x 40) 40.0))))
        (insert (format "% 7.3f   % 6.3f   % 6.3f   % 7.3f   % 6.3f   % 6.3f   % 7.3f\n" n
                        (sin n) (cos n)
                        (if (not (or (= x 20) (= x 60))) (tan n) (acos n))
                        (asin n) (acos n) (atan n))))))
  (setq buffer-read-only t)
  (goto-char (point-min)))
;; trigonometry-table:1 ends here

;; [[file:init-emacs.org::#functions-table-generators-vga-colors-table][vga-colors-table:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Table Generators: vga-colors-table
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Table Generators: vga-colors-table")

(defun vga-colors-table ()
  "Display a table of the VGA colors."
  (interactive)
  (let ((vga-colors
         '((0 0 0) (0 0 170) (0 170 0) (0 170 170)
           (170 0 0) (170 0 170) (170 85 0) (170 170 170)
           (85 85 85) (85 85 255) (85 255 85) (85 255 255)
           (255 85 85) (255 85 255) (255 255 85) (255 255 255)
           (0 0 0) (20 20 20) (32 32 32) (44 44 44)
           (56 56 56) (69 69 69) (81 81 81) (97 97 97)
           (113 113 113) (130 130 130) (146 146 146) (162 162 162)
           (182 182 182) (203 203 203) (227 227 227) (255 255 255)
           (0 0 255) (65 0 255) (125 0 255) (190 0 255)
           (255 0 255) (255 0 190) (255 0 125) (255 0 65)
           (255 0 0) (255 65 0) (255 125 0) (255 190 0)
           (255 255 0) (190 255 0) (125 255 0) (65 255 0)
           (0 255 0) (0 255 65) (0 255 125) (0 255 190)
           (0 255 255) (0 190 255) (0 125 255) (0 65 255)
           (125 125 255) (158 125 255) (190 125 255) (223 125 255)
           (255 125 255) (255 125 223) (255 125 190) (255 125 158)
           (255 125 125) (255 158 125) (255 190 125) (255 223 125)
           (255 255 125) (223 255 125) (190 255 125) (158 255 125)
           (125 255 125) (125 255 158) (125 255 190) (125 255 223)
           (125 255 255) (125 223 255) (125 190 255) (125 158 255)
           (182 182 255) (199 182 255) (219 182 255) (235 182 255)
           (255 182 255) (255 182 235) (255 182 219) (255 182 199)
           (255 182 182) (255 199 182) (255 219 182) (255 235 182)
           (255 255 182) (235 255 182) (219 255 182) (199 255 182)
           (182 255 182) (182 255 199) (182 255 219) (182 255 235)
           (182 255 255) (182 235 255) (182 219 255) (182 199 255)
           (0 0 113) (28 0 113) (56 0 113) (85 0 113)
           (113 0 113) (113 0 85) (113 0 56) (113 0 28)
           (113 0 0) (113 28 0) (113 56 0) (113 85 0)
           (113 113 0) (85 113 0) (56 113 0) (28 113 0)
           (0 113 0) (0 113 28) (0 113 56) (0 113 85)
           (0 113 113) (0 85 113) (0 56 113) (0 28 113)
           (56 56 113) (69 56 113) (85 56 113) (97 56 113)
           (113 56 113) (113 56 97) (113 56 85) (113 56 69)
           (113 56 56) (113 69 56) (113 85 56) (113 97 56)
           (113 113 56) (97 113 56) (85 113 56) (69 113 56)
           (56 113 56) (56 113 69) (56 113 85) (56 113 97)
           (56 113 113) (56 97 113) (56 85 113) (56 69 113)
           (81 81 113) (89 81 113) (97 81 113) (105 81 113)
           (113 81 113) (113 81 105) (113 81 97) (113 81 89)
           (113 81 81) (113 89 81) (113 97 81) (113 105 81)
           (113 113 81) (105 113 81) (97 113 81) (89 113 81)
           (81 113 81) (81 113 89) (81 113 97) (81 113 105)
           (81 113 113) (81 105 113) (81 97 113) (81 89 113)
           (0 0 65) (16 0 65) (32 0 65) (48 0 65)
           (65 0 65) (65 0 48) (65 0 32) (65 0 16)
           (65 0 0) (65 16 0) (65 32 0) (65 48 0)
           (65 65 0) (48 65 0) (32 65 0) (16 65 0)
           (0 65 0) (0 65 16) (0 65 32) (0 65 48)
           (0 65 65) (0 48 65) (0 32 65) (0 16 65)
           (32 32 65) (40 32 65) (48 32 65) (56 32 65)
           (65 32 65) (65 32 56) (65 32 48) (65 32 40)
           (65 32 32) (65 40 32) (65 48 32) (65 56 32)
           (65 65 32) (56 65 32) (48 65 32) (40 65 32)
           (32 65 32) (32 65 40) (32 65 48) (32 65 56)
           (32 65 65) (32 56 65) (32 48 65) (32 40 65)
           (44 44 65) (48 44 65) (52 44 65) (60 44 65)
           (65 44 65) (65 44 60) (65 44 52) (65 44 48)
           (65 44 44) (65 48 44) (65 52 44) (65 60 44)
           (65 65 44) (60 65 44) (52 65 44) (48 65 44)
           (44 65 44) (44 65 48) (44 65 52) (44 65 60)
           (44 65 65) (44 60 65) (44 52 65) (44 48 65)
           (0 0 0) (0 0 0) (0 0 0) (0 0 0)
           (0 0 0) (0 0 0) (0 0 0) (0 0 0)))
        (color-names
         '(("#000000" . "Black")
           ("#0000AA" . "Navy")
           ("#00AA00" . "Green")
           ("#00AAAA" . "Teal")
           ("#AA0000" . "Maroon")
           ("#AA00AA" . "Purple")
           ("#AA5500" . "Brown")
           ("#AAAAAA" . "Silver")
           ("#555555" . "Gray")
           ("#5555FF" . "Blue")
           ("#55FF55" . "Lime")
           ("#55FFFF" . "Cyan")
           ("#FF5555" . "Orange")
           ("#FF55FF" . "Magenta")
           ("#FFFF55" . "Yellow")
           ("#FFFFFF" . "White")
           ("#141414" . "Gray")
           ("#202020" . "Gray")
           ("#2C2C2C" . "Gray")
           ("#383838" . "Gray")
           ("#454545" . "Gray")
           ("#515151" . "Gray")
           ("#616161" . "Gray")
           ("#717171" . "Gray")
           ("#828282" . "Gray")
           ("#929292" . "Gray")
           ("#A2A2A2" . "Gray")
           ("#B6B6B6" . "Gray")
           ("#CBCBCB" . "Gray")
           ("#E3E3E3" . "Gray")
           ("#0000FF" . "Blue")
           ("#FF00FF" . "Magenta")
           ("#FF0000" . "Red")
           ("#FFFF00" . "Yellow")
           ("#00FF00" . "Green")
           ("#00FFFF" . "Cyan"))))
        ;; (color-names (mapcar (lambda (x)
        ;;                        (cons
        ;;                         (apply 'format "#%02X%02X%02X"
        ;;                                (mapcar (lambda (x) (ash x -8))
        ;;                                        (color-values x)))
        ;;                         x))
        ;;                      (defined-colors))))
    (switch-to-buffer "*VGA Color Table*")
    (buffer-disable-undo)
    (let (buffer-read-only)
      (erase-buffer)
      (insert "COLOR   RGB (Decimal)   RGB (Hex)   HSV (0.0 - 1.0)   NAME\n")
      (insert "-----   -------------   ---------   ---------------   ------------\n")
      (dotimes (x (length vga-colors))
        (let* ((rgb (nth x vga-colors))
               (hex (apply #'format "#%02X%02X%02X" rgb))
               (hsv (apply #'color-rgb-to-hsv (mapcar (lambda (x) (/ x 256.0)) rgb)))
               (fg (readable-foreground-color hex))
               (name (or (cdr (assoc-string hex color-names)) "")))
          (insert (propertize
                   (format "%5d   %3d, %3d, %3d   %-9s   %-15s   %-12s\n"
                           x
                           (first rgb) (second rgb) (third rgb)
                           hex
                           (apply #'format "%.2f %.2f %.2f" hsv)
                           name)
                   'face `(:background ,hex :foreground ,fg))))))
    (setq buffer-read-only t)
    (goto-char (point-min))))
;; vga-colors-table:1 ends here

;; [[file:init-emacs.org::#functions-programs][Programs:1]]
;;------------------------------------------------------------------------------
;;; Functions: Programs
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Programs")
;; Programs:1 ends here

;; [[file:init-emacs.org::#functions-programs-national-debt][National Debt:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Programs: National Debt
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: National Debt")

(defun national-debt ()
  "Return the current US national debt."
  (interactive)
  (save-current-buffer
    (let ((buffer (url-retrieve-synchronously "http://brillig.com/debt_clock/"))
          result)
      (when buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "debtiv\.gif.*[Aa][Ll][Tt]=\"\\([^\"]*\\)" nil :noerror)
            (setq result
                  (replace-regexp-in-string
                   "\s+" ""
                   (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))))
      (if result
          (if (called-interactively-p 'any)
              (message result)
            (string-to-number
             (replace-regexp-in-string "," ""
                                       (replace-regexp-in-string "\\$" "" result))))
        (message "Error fetching the national debt")))))
;; National Debt:1 ends here

;; [[file:init-emacs.org::#functions-programs-flesch-readability-index][Flesch Readability Index:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Programs: Flesch Readability Index
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: Flesch Readability Index")

(defun flesch-readability-index (&optional beg end)
  "Compute the Flesch Readability Index of the current region or entire buffer.

The Flesch Readability Index is defined on wikipedia here:

  http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test

This function provides that index following the guidelines presented here:

  http://cs.boisestate.edu/~amit/teaching/125/lab/p4.html

Guidelines:

  - Count all words in the text. A word is any sequence of
    characters delimited by white space, whether or not it is an
    actual English word.

  - Count all syllables in each word. To make this simple, use
    the following rules. Each group of adjacent
    vowels (a,e,i,o,u,y) counts as one syllable (for example, the
    \"ea\" in \"real\" contributes one syllable, but the \"e..a\"
    in \"regal\" count as two syllables). However, an \"e\" at
    the end of a word doesn't count as a syllable. For example,
    the word \"eagle\" would be one syllable by Flesch's rules.
    However, the word \"intrigue\" is three syllables (since the
    e is preceded by a vowel). Also, each word has at least one
    syllable, even if the previous rules give a count of zero.

  - Count all sentences. A sentence is ended by a period, colon,
    semicolon, questions mark, or exclamation mark.

  - The index is computed by:

    index = 206.835 - (1.015 * words / sentences) - (84.6 * syllables / words)

  - Index should be rounded to the nearest integer.

The index is a number, usually between 0 and 100, indicating how
difficult the text is to read."
  (interactive)
  (cl-labels
      ((count-words (beg end)
                    (how-many "\\w+" beg end))
       (count-sentences (beg end)
                        (how-many "[\.\:\;\?\!]\\W" beg end))
       ;; (count-sentences (beg end)
       ;;                  (save-mark-and-excursion
       ;;                    (let ((count 0))
       ;;                      (goto-char beg)
       ;;                      (while (< (point) end)
       ;;                        (forward-sentence 1)
       ;;                        (setq count (1+ count))))))
       (count-syllables (beg end)
                        (let ((letter-regexp "[A-Za-z]")
                              (vowel-regexp "[AEIOUYaeiouy]")
                              (e-end-regexp "[Ee]\\W"))
                          (save-mark-and-excursion
                            (let ((count 0))
                              (goto-char beg)
                              (while (< (point) end)
                                (while (and (< (point) end)
                                            (not (looking-at letter-regexp)))
                                  (forward-char 1))
                                (let ((state (if (looking-at vowel-regexp) 2 1)))
                                  (when (= state 2)
                                    (setq count (1+ count)))
                                  (while (looking-at letter-regexp)
                                    (if (and (= state 1)
                                             (looking-at vowel-regexp)
                                             (not (looking-at e-end-regexp)))
                                        (setq state 2
                                              count (1+ count))
                                      (if (and (= state 2)
                                               (not (looking-at vowel-regexp)))
                                          (setq state 1)))
                                    (forward-char 1))))
                              count)))))
    (let* ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
           (end (or end (if (use-region-p) (region-end) (point-max))))
           (words (count-words beg end))
           (sentences (count-sentences beg end))
           (syllables (count-syllables beg end))
           (index (round (- 206.835
                            (/ (* 1.015 words) (if (> sentences 0) sentences 1))
                            (/ (* 84.6 syllables) (if (> words 0) words 1)))))
           (index-desc
            (cond
             ((> index 90) "5th grader")
             ((> index 80) "6th grader")
             ((> index 70) "7th grader")
             ((> index 65) "8th grader")
             ((> index 60) "9th grader")
             ((> index 50) "High school student")
             ((> index 30) "College student")
             ((>= index 0) "College graduate")
             (t "Law school graduate"))))
      (message "Words: %s, Sentences: %s, Syllables %s, Flesch Readability Index: %s (%s)"
               words sentences syllables index index-desc)
      index)))
;; Flesch Readability Index:1 ends here

;; [[file:init-emacs.org::#functions-programs-phone-number-words][Phone Number Words:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Programs: Phone Number Words
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: Phone Number Words")

;; TODO: finish this
(defun phone-number-words (num &optional word-file)
  "Convert phone number NUM into word strings that may be used instead.

Single digits may appear between words.
WORD-FILE defaults to `/usr/share/dict/words'."
  (interactive "sPhone number: ")
  (let ((word-file (or word-file "/usr/share/dict/words")))
    (defun file-to-string (file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))
    (defun string-to-list (string)
      (cl-loop for x across string collect x))
    (defun list-to-string (list)
      (mapconcat 'string list ""))
    ;;(setq word-file "~/tw")
    (unless (file-exists-p word-file)
      (user-error "Word file does not exist: %s" word-file))
    ;; load word file
    (let* ((words (split-string (file-to-string word-file))) ; word list
           (word-hash (make-hash-table :size (length words))) ; number to word hash
           (letter-digit '((?A . ?2) (?B . ?2) (?C . ?2)
                           (?D . ?3) (?E . ?3) (?F . ?3)
                           (?G . ?4) (?H . ?4) (?I . ?4)
                           (?J . ?5) (?K . ?5) (?L . ?5)
                           (?M . ?6) (?N . ?6) (?O . ?6)
                           (?P . ?7) (?Q . ?7) (?R . ?7) (?S . ?7)
                           (?T . ?8) (?U . ?8) (?V . ?8)
                           (?W . ?9) (?X . ?9) (?Y . ?9) (?Z . ?9)))) ; letter to digit mappings
      ;; build word hash
      (dolist (word words)
        (let* ((letters (string-to-list (upcase word)))
               (number (list-to-string (mapcar (lambda (x) (char-to-string (cdr (assoc x letter-digit)))) letters))))
          ;;(message "letters: %S, number: %S" letters number)
          (setf (gethash number word-hash) word))))))
;; Phone Number Words:1 ends here

;; [[file:init-emacs.org::#functions-programs-keyboard-cat-mode][Keyboard Cat Mode:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Programs: Keyboard Cat Mode
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: Keyboard Cat Mode")

(defvar keyboard-cat-overlay nil)

(defun keyboard-cat-next ()
  (interactive)
  (move-overlay keyboard-cat-overlay
                (goto-char (min (1+ (overlay-start keyboard-cat-overlay))
                                (point-max)))
                (overlay-end keyboard-cat-overlay)))

(defvar keyboard-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'keyboard-cat-next)
    (define-key map [remap keyboard-quit] 'keyboard-cat-mode)
    map))

(define-minor-mode keyboard-cat-mode
  "Minor mode that slowly exposes current buffer as keys are pressed."
  :init nil
  :keymap keyboard-cat-mode-map
  (if keyboard-cat-mode
      (overlay-put
       (setq-local keyboard-cat-overlay
                   (make-overlay (point-min) (point-max)))
       'invisible t)
    (delete-overlay keyboard-cat-overlay)))
;; Keyboard Cat Mode:1 ends here

;; [[file:init-emacs.org::#functions-programs-keyboard-display-mode][Keyboard Display Mode:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Programs: Keyboard Display Mode
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: Keyboard Display Mode")

(defvar keyboard-display-process-name "gxmessage")

(defvar keyboard-display-key-log nil)

(defvar keyboard-display-key-log-width 0)

(defvar keyboard-display-key-log-size 10
  "Maximum number of key presses to keep in the log.")

(defvar keyboard-display-key-log-duration 5
  "Number of seconds to keep key presses in the key log.")

(defvar keyboard-display-message-duration 5
  "Number of seconds to keep key press display showing.")

(defvar keyboard-display-last-key "")
(defvar keyboard-display-last-key-count 1)

(defun keyboard-display-key-press ()
  "Display current key press using gxmessage."
  (let ((key (concat (if current-prefix-arg
                         (concat current-prefix-arg " ")
                       "")
                     (symbol-name this-command))))
    (when keyboard-display-key-log
      (if (< (caar (reverse keyboard-display-key-log))
             (- (float-time (current-time)) keyboard-display-key-log-duration))
          (setq keyboard-display-key-log nil
                keyboard-display-key-log-width 0
                keyboard-display-last-key ""
                keyboard-display-last-key-count 1)
        (when (>= (length keyboard-display-key-log) keyboard-display-key-log-size)
          (pop keyboard-display-key-log))))
    (if (string= keyboard-display-last-key key)
        (setq key (concat key " x " (number-to-string (cl-incf keyboard-display-last-key-count))))
      (setq keyboard-display-last-key key
            keyboard-display-last-key-count 1))
    (when (> (length key) keyboard-display-key-log-width)
      (setq keyboard-display-key-log-width (length key)))
    (setq key (substring (concat key (make-string keyboard-display-key-log-width ? ))
                         0 keyboard-display-key-log-width))
    (if (> keyboard-display-last-key-count 1)
        (setcdr (car (last keyboard-display-key-log)) key)
      (setq keyboard-display-key-log (append keyboard-display-key-log
                                             (list (cons (float-time (current-time)) key)))))
    (let ((msg (mapconcat (lambda (x) (cdr x)) keyboard-display-key-log "\n")))
      (start-process keyboard-display-process-name nil
                     "gxmessage" "-borderless" "-nofocus" "-center"
                     "-timeout" (int-to-string keyboard-display-message-duration)
                     "-buttons" "" "-fn" "mono 32" msg))))

(defvar keyboard-display-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode keyboard-display-mode
  "Minor mode that displays every key press in a pop-up message."
  :init nil
  :keymap keyboard-display-mode-map
  (if keyboard-display-mode
      (add-hook 'pre-command-hook #'keyboard-display-key-press)
    (remove-hook 'pre-command-hook 'keyboard-display-key-press)))
;; Keyboard Display Mode:1 ends here

;; [[file:init-emacs.org::#functions-programs-star-wars-scroll][Star Wars Scroll:1]]
;;------------------------------------------------------------------------------
;; Star Wars Scroll (scroll current text buffer like Star Wars opening text)
;;
;; http://mbork.pl/2015-12-18_Star_Wars_crawl_in_Emacs
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Programs: Star Wars Scroll")

(defvar star-wars-scroll-substitution-list
  '(("." . 0)
    (" " . 0)
    ("[.,;:!?] " . 1)
    ("\\B[aeiou]\\B" . 0)
    ("\\B[bcdfghjklmnpqrstvwxyz]\\B" . 0)
    ("\\w\\b" . 0)
    ("[.,;:!?]" . 0)
    ("[^.,;:!?] " . 1)
    ("\\b\\w" . 0)
    (".$" . 0)
    ("^." . 0))
  "A list of dotted pairs with car equal to the regex matching
the character we want to delete and cdr equal to how many
characters we want to move the point forward before actually
deleting a character (useful in the case of space after a
punctuation). We begin with the substitutions we want to perform
first. If more than one regex matches, the last one is valid, so
it is probably a good idea to begin with \".\".")

(defun star-wars-scroll-center-line-no-tabs ()
  "A simplified version of center-line, using no tabs (and not
taking into account leading/trailing whitespace."
  (save-mark-and-excursion
    (let ((length (progn (end-of-line)
                         (current-column))))
      (forward-line 0)
      (insert (make-string (max 0 (/ (- fill-column length) 2)) ?\s)))))

(defun star-wars-scroll-scroll-prepare-marker-list ()
  "Prepare (and return) a list of markers pointing at characters
to delete from the current line, in the \"right\" order."
  (save-mark-and-excursion
    (save-match-data
      (let ((limit (progn
                     (star-wars-scroll-center-line-no-tabs) ; this may use tabs
                     (back-to-indentation)
                     (point)))
            (subst-list star-wars-scroll-substitution-list)
            (marker-list nil))
        (while subst-list
          (end-of-line)
          (while (search-backward-regexp (caar subst-list) limit :noerror)
            (forward-char (cdar subst-list))
            (push (point-marker) marker-list))
          (setq subst-list (cdr subst-list)))
        (delete-dups marker-list)
        (setq marker-list (nreverse marker-list))))))

(defvar star-wars-scroll-untouched-lines 3
  "Number of lines at the bottom of the window which should not
be touched by character deletion.")

(defvar star-wars-scroll-delay .5
  "Delay (in seconds) between frames of animation.")

(defun star-wars-scroll-scroll-current-buffer ()
  "Actually do SW-like scroll in the current buffer."
  (let (marker-list-list)
    (goto-char (point-min))
    (open-line (window-height))
    (goto-char (point-max))
    (move-beginning-of-line 0)
    (while (progn
             (push (star-wars-scroll-scroll-prepare-marker-list) marker-list-list)
             (> (point) (+ (point-min) (window-height))))
      (forward-line -1))
    (while (< (point-min) (point-max)) ; here the actual scroll begins
      (goto-char (point-min))
      (kill-line 1)
      (redisplay t)
      (sleep-for star-wars-scroll-delay)
      (let ((walker marker-list-list))
        (while (progn
                 (goto-char (or (caar walker) (point-min)))
                 (and walker (< (line-number-at-pos) (- (window-height) star-wars-scroll-untouched-lines))))
          (when (car walker)
            (goto-char (caar walker))
            (delete-char 1)
            (setf (car walker) (cdar walker)))
          (when (car walker)
            (goto-char (caar walker))
            (delete-char 1)
            (setf (car walker) (cdar walker))
            (forward-line 0)
            (insert " "))
          (setq walker (cdr walker)))))))

(defun star-wars-scroll ()
  "Do Star-Wars-like scroll of the region, or the whole buffer if
  no region is active, in a temporary buffer, and delete it
  afterwards. Special care is taken to make the lines more or
  less legible as long as possible, for example spaces after
  punctuation are deleted before vowels, vowels are deleted
  before consonants etc."
  (interactive)
  (save-mark-and-excursion
    (let ((begin (point-min)) (end (point-max)))
      (when (region-active-p)
        (setq beg (region-beginning))
        (setq end (region-end)))
      (copy-region-as-kill beg end)
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (rename-buffer "*Star Wars Scroll*")
        (buffer-disable-undo)
        (untabify (point-min) (point-max))
        (save-window-excursion
          (delete-other-windows)
          (yank)
          (star-wars-scroll-scroll-current-buffer))))))
;; Star Wars Scroll:1 ends here

;; [[file:init-emacs.org::#functions-games][Games:1]]
;;------------------------------------------------------------------------------
;;; Functions: Games
;;------------------------------------------------------------------------------

(init-message 2 "Functions: Games")
;; Games:1 ends here

;; [[file:init-emacs.org::#functions-games-towers-of-hanoi][Towers of Hanoi:1]]
;;------------------------------------------------------------------------------
;;;; Functions: Games: Towers of Hanoi
;;------------------------------------------------------------------------------

(init-message 3 "Functions: Games: Towers of Hanoi")

(defun towers (disks)
  "Solve the clasical Towers of Hanoi problem for given number of DISKS."
  (interactive "NNumber of disks: ")
  (let ((buffer "*Towers*"))
    ;; setup buffer
    (get-buffer-create buffer)
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; print header
    (insert (format "Towers of Hanoi puzzle with %d disks\n\n" disks))
    ;; make initial call to towers move
    ;; move from peg 1 to 3 using 2
    (towers-move disks 1 3 2)
    ;; set buffer to read-only
    (setq buffer-read-only t)
    ;; switch to buffer
    (switch-to-buffer buffer)
    (goto-char (point-min)))
  ;; no return value
  (values))

(defun towers-move (n from to using)
  "Make one Towers of Hanoi move.

N is the number of disks to move.

FROM is the source peg.

TO is the target peg.

USING is the remaining peg."
  (when (> n 0)
    ;; In order to move N disks FROM one peg TO another, we first move N-1
    ;; disks FROM one peg to the other USING peg.
    (towers-move (1- n) from using to)
    ;; Then we move the one remaining peg FROM the starting peg TO the
    ;; finishing peg.
    (insert (format "Move %d --> %d\n" from to))
    ;; Finally we move the N-1 disks now on the other USING peg to the TO peg
    ;; using FROM.
    (towers-move (1- n) using to from)))
;; Towers of Hanoi:1 ends here

;; [[file:init-emacs.org::#completions][Completions:1]]
;;==============================================================================
;;; Completions
;;==============================================================================

(init-message 1 "Completions")
;; Completions:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-corfu][vertico/consult/corfu:1]]
;;------------------------------------------------------------------------------
;;; Completions: vertico/consult/corfu
;;------------------------------------------------------------------------------

(init-message 2 "Completions: vertico/consult/corfu")
;; vertico/consult/corfu:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-company-vertico][vertico:1]]
;;------------------------------------------------------------------------------
;;;; Completions: vertico/consult/company: vertico
;;------------------------------------------------------------------------------

(init-message 3 "Completions: vertico/consult/company: vertico")

(use-package vertico
  :straight t
  :demand t
  :bind (:map vertico-map
              ("C-<return>" . vertico-exit-input) ; default: `vertico-exit'
              ("M-<return>" . vertico-exit-input) ; default: `vertico-exit'
              ("C-M-i" . vertico-scroll-down)     ; default: `completion-at-point' ("<prior>")
              ("C-M-k" . vertico-scroll-up))      ; default: `kill-whole-line' ("<next>")
  :init
  (vertico-mode))
;; vertico:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-company-orderless][orderless:1]]
;;------------------------------------------------------------------------------
;;;; Completions: vertico/consult/company: orderless
;;------------------------------------------------------------------------------

(init-message 3 "Completions: vertico/consult/company: orderless")

(use-package orderless
  :straight t
  :after (vertico)
  :custom
  (completion-styles '(orderless))
  ;;(completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
;; orderless:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-company-marginalia][marginalia:1]]
;;------------------------------------------------------------------------------
;;;; Completions: vertico/consult/company: marginalia
;;------------------------------------------------------------------------------

(init-message 3 "Completions: vertico/consult/company: marginalia")

(use-package marginalia
  :straight t
  :after (vertico)
  :init
  (marginalia-mode))
;; marginalia:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-company-consult][consult:1]]
;;------------------------------------------------------------------------------
;;;; Completions: vertico/consult/company: consult
;;------------------------------------------------------------------------------

(init-message 3 "Completions: vertico/consult/company: consult")

(use-package consult
  :straight (consult-custom
             :type git :host github :repo "minad/consult"
             :pre-build "git checkout 0.17")
  :after (vertico)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro))
  :bind* (
          ("C-'" . consult-line)                    ; default: `isearch-forward-regexp'
          ;; C-x bindings (ctl-x-map)
          ("C-x M-:" . consult-complex-command)     ; default: `repeat-complex-command'
          ("C-x b" . consult-buffer)                ; default: `switch-to-buffer'
          ("C-x 4 b" . consult-buffer-other-window) ; default: `switch-to-buffer-other-window'
          ("C-x 5 b" . consult-buffer-other-frame)  ; default: `switch-to-buffer-other-frame'
          ("C-x r b" . consult-bookmark)            ; default: `bookmark-jump'
          ("C-x p b" . consult-project-buffer)      ; default: `project-switch-to-buffer'
          ;; M-# bindings for fast register access
          ("M-#" . consult-register-load)           ; default: `calc-dispatch'
          ("M-'" . consult-register-store)          ; default: `abbrev-prefix-mark'
          ("C-M-#" . consult-register)
          ;; other bindings
          ("M-y" . consult-yank-pop)                ; default: `yank-pop'
          ("<help> a" . consult-apropos)            ; default: `apropos-command'
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flycheck)
          ("M-g F" . consult-flymake)
          ("M-g g" . consult-goto-line)             ; default: `goto-line'
          ("M-g M-g" . consult-goto-line)           ; default: `goto-line'
          ("M-g o" . consult-org-heading)           ; alternative: `consult-outline'
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s d" . consult-find)
          ("M-s D" . consult-locate)
          ("M-s g" . consult-grep)
          ("M-s G" . consult-git-grep)
          ("M-s r" . consult-ripgrep)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi)
          ("M-s m" . consult-multi-occur)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines)
          ;; isearch integration
          ("M-s e" . consult-isearch-history)
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)         ; default: `isearch-edit-string'
          ("M-s e" . consult-isearch-history)       ; default: `isearch-edit-string'
          ("M-s l" . consult-line)                  ; needed by `consult-line' to detect isearch
          ("M-s L" . consult-line-multi)            ; needed by `consult-line' to detect isearch
          ;; minibuffer bindings
          :map minibuffer-local-map
          ;;("C-<return>" . exit-minibuffer)        ; default: `vertico-exit'
          ;;("M-<return>" . exit-minibuffer)        ; default: `vertico-exit'
          ("M-s" . consult-history)                 ; default: `next-matching-history-element'
          ("M-r" . consult-history))                ; default: `previous-matching-history-element'
  ;; enable automatic preview at point in the *Completions* buffer
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; improve the register preview for `consult-register' and the emacs built-ins
  (setq register-preview-function #'consult-register-format
        register-preview-delay 0.5)
  ;; add thin lines, sorting, and hide the mode line of the register preview window
  (advice-add 'register-preview :override #'consult-register-window)
  ;; ;; replace `completing-read-multiple' with an enhanced version
  ;; (advice-add 'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; configure preview key
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))   ; delay preview
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;; my/command-wrapping-consult    ;; disable auto previews inside my command
  ;; :preview-key '(:debounce 0.4 any) ;; Option 1: Delay preview
  ;; :preview-key (kbd "M-."))      ;; Option 2: Manual preview
  ;; configure narrowing key
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; make narrowing help available in the minibuffer
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  ;; turn off consult mode in incompatable modes
  (defun force-completing-read-default (orig-fun &rest args)
    "Force a function to use `completing-read-default'."
    (let ((completing-read-function 'completing-read-default))
      (apply orig-fun args)))
  ;; advise `tmm-prompt'
  (advice-add 'tmm-prompt :around #'force-completing-read-default)
  ;; advise `yas-expand-snippet'
  (advice-add 'yas-expand-snippet :around #'force-completing-read-default))
;; consult:1 ends here

;; [[file:init-emacs.org::#completions-vertico-consult-company-corfu][corfu:1]]
;;------------------------------------------------------------------------------
;;;; Completions: vertico/consult/company: corfu
;;------------------------------------------------------------------------------

(init-message 3 "Completions: vertico/consult/company: corfu")

(use-package corfu
  :straight t
  ;; ;; enable only for certain modes
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("M-SPC" . corfu-insert-separator)
              ("RET" . nil)             ; enter does not complete
              ;; ("S-RET" . corfu-insert)
              ("C-n" . corfu-next)
              ("M-k" . corfu-next)
              ("C-p" . corfu-previous)
              ("M-i" . corfu-previous)
              ("TAB" . corfu-complete)
              ("<tab>" . corfu-complete))
  :custom
  (corfu-cycle nil)                 ; disable cycling for `corfu-next/previous'
  ;;(corfu-auto t)                    ; enable auto completion
  ;;(corfu-auto-prefix 3)             ; auto complete after a number of typed characters
  ;;(corfu-auto-delay 1.0)            ; auto complete after a delay
  (corfu-auto nil)                    ; disable auto completion
  (corfu-separator ?\s)             ; whitespace is orderless field separator
  (corfu-quit-at-boundary 'separator) ; quit at completion boundary
  (corfu-quit-no-match nil)         ; do not quit if there is no match
  (corfu-preview-current 'insert)   ; insert current candidate preview
  (corfu-preselect-first nil)       ; disable candidate preselection
  (corfu-on-exact-match nil)        ; configure handling of exact matches
  (corfu-echo-documentation 0.25)   ; enable documentation in the echo area
  (corfu-scroll-margin 5)           ; use scroll margin
  :init
  (global-corfu-mode 1)
  ;;(corfu-history-mode 1)
  :config
  (defun custom-corfu-eshell-mode-hook ()
    (setq-local corfu-auto nil
                corfu-quit-at-boundary t
                corfu-quit-no-match t)
    (corfu-mode 1))
  (add-hook 'eshell-mode-hook #'custom-corfu-eshell-mode-hook))

;; ;;------------------------------------------------------------------------------
;; ;;;; corfu-doc
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "corfu-doc")

;; ;; popup documentation for completion candidates
;; (use-package corfu-doc
;;   :straight t
;;   :after (corfu)
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :bind (:map corfu-map
;;               ("M-p" . corfu-doc-scroll-down)
;;               ("M-n" . corfu-doc-scroll-up)
;;               ("M-d" . corfu-doc-toggle)))

;; (init-message 3 "corfu-doc-terminal")

;; ;; make `corfu-doc' work in terminal
;; (use-package corfu-doc-terminal
;;   :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
;;   :init
;;   (unless (display-graphic-p)
;;     (corfu-doc-terminal-mode 1)))
;; corfu:1 ends here

;; [[file:init-emacs.org::#cape][cape:1]]
;;==============================================================================
;;; cape
;;==============================================================================

(init-message 1 "cape")

(use-package cape
  :straight t
  ;; bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ; capf
         ("C-c p t" . complete-tag)        ; etags
         ("C-c p d" . cape-dabbrev)        ; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ;;("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; sanitize `pcomplete-completions-at-point' capf
  ;; capf has undesired side effects on Emacs 28 and earlier
  (when (version<= "29.0" emacs-version)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))
;; cape:1 ends here

;; [[file:init-emacs.org::#packages][Packages:1]]
;;==============================================================================
;;; Packages
;;==============================================================================

(init-message 1 "Packages")
;; Packages:1 ends here

;; [[file:init-emacs.org::#modules-abbrev-mode][abbrev-mode:1]]
;;------------------------------------------------------------------------------
;;; Packages: abbrev-mode
;;------------------------------------------------------------------------------

(init-message 2 "Modules: abbrev-mode")

(use-package abbrev
  :straight (:type built-in)
  :diminish abbrev-mode
  :custom
  (save-abbrevs 'silently)
  :init
  (defconst abbrev-file
    (file-truename (expand-file-name "~/.abbrev_defs"))
    "Abbreviations file used by `abbrev-mode'.")
  (unless (file-exists-p abbrev-file)
    (with-temp-buffer (write-file abbrev-file)))
  (abbrev-mode 1)
  :config
  (setq dabbrev-case-replace nil) ; preserve case when replacing abbreviations
  (quietly-read-abbrev-file abbrev-file)
  (defun custom-kill-emacs-hook-write-abbrev-file ()
    (write-abbrev-file abbrev-file))
  (add-hook 'kill-emacs-hook #'custom-kill-emacs-hook-write-abbrev-file))
;; abbrev-mode:1 ends here

;; [[file:init-emacs.org::#modules-ag][ag:1]]
;;------------------------------------------------------------------------------
;;; Packages: ag
;;------------------------------------------------------------------------------

(init-message 2 "Modules: ag")

(use-package ag
  :straight t
  :commands (ag)
  :custom
  (ag-arguments (list "--smart-case" "--stats")))
;; ag:1 ends here

;; [[file:init-emacs.org::#modules-alert][alert:1]]
;;------------------------------------------------------------------------------
;;; Packages: alert
;;------------------------------------------------------------------------------

(init-message 2 "Modules: alert")

(use-package alert
  :straight t
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))
;; alert:1 ends here

;; [[file:init-emacs.org::#modules-analog-clock][analog-clock:1]]
;;------------------------------------------------------------------------------
;;; Packages: analog-clock
;;------------------------------------------------------------------------------

(init-message 2 "Modules: analog-clock")

(use-package analog-clock
  :load-path (lambda () (file-truename (expand-file-name "analog-clock.el" emacs-modules-dir)))
  :commands (analog-clock analog-clock-draw-analog)
  :custom
  ;; draw an analog clock
  (analog-clock-draw-function #'analog-clock-draw-analog)
  ;; draw a digital clock
  ;;(analog-clock-draw-function #'analog-clock-draw-ssd)
  :init
  ;; screen saver mode (sort of)
  ;;(cancel-function-timers #'analog-clock)
  ;;(run-with-idle-timer 600 :refresh #'analog-clock)
  )
;; analog-clock:1 ends here

;; [[file:init-emacs.org::#modules-any-ini-mode][any-ini-mode:1]]
;;------------------------------------------------------------------------------
;;; Packages: any-ini-mode
;;------------------------------------------------------------------------------

(init-message 2 "Modules: any-ini-mode")

(use-package any-ini-mode
  :load-path (lambda () (file-truename (expand-file-name "any-ini-mode.el" emacs-modules-dir))))
;; any-ini-mode:1 ends here

;; [[file:init-emacs.org::#modules-async][async:1]]
;;------------------------------------------------------------------------------
;;; Packages: async
;;------------------------------------------------------------------------------

(init-message 2 "Modules: async")

(use-package async
  :straight t)
  ;; :config
  ;; ;; lock-file wrapper macro to evaluate code blocks synchronously using an async task
  ;; (defmacro async-start-with-lock-file (lock-file start-func &optional finish-func)
  ;;   "Evaluate BODY using LOCK-FILE to prevent simultaneous calls.

  ;;   LOCK-FILE is a file name to be used as a lock. If another
  ;;   process is currently using the lock, this process will wait
  ;;   until it is released before running."
  ;;   (declare (indent 1))
  ;;   (eval
  ;;    `(async-start
  ;;      (lambda ()
  ;;        (while (file-exists-p ,lock-file)
  ;;          (message "Sleeping")
  ;;          (sleep-for 1))
  ;;        (make-empty-file ,lock-file)
  ;;        (condition-case nil
  ;;            (funcall ,start-func)
  ;;          (t
  ;;           (delete-file ,lock-file))))
  ;;      ,finish-func))))
;; async:1 ends here

;; [[file:init-emacs.org::#modules-auto-compile][auto-compile:1]]
;;------------------------------------------------------------------------------
;;; Packages: auto-compile
;;------------------------------------------------------------------------------

(init-message 2 "Modules: auto-compile")

(use-package auto-compile
  :straight t
  :custom
  (load-prefer-newer t)
  :init
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))
;; auto-compile:1 ends here

;; [[file:init-emacs.org::#modules-avy][avy:1]]
;;------------------------------------------------------------------------------
;;; Packages: avy
;;------------------------------------------------------------------------------

(init-message 2 "Modules: avy")

(use-package avy
  :straight t
  :bind* (("C-;" . avy-goto-char)
          ("C-:" . avy-goto-word-or-subword-1)
          ("C-M-;" . pop-to-mark-command)))
;; avy:1 ends here

;; [[file:init-emacs.org::#modules-bash-completion][bash-completion:1]]
;;------------------------------------------------------------------------------
;;; Packages: bash-completion
;;------------------------------------------------------------------------------

(init-message 2 "Modules: bash-completion")

(use-package bash-completion
  :straight t
  :init (bash-completion-setup))
;; bash-completion:1 ends here

;; [[file:init-emacs.org::#modules-bbdb][bbdb:1]]
;;------------------------------------------------------------------------------
;;; Packages: bbdb
;;------------------------------------------------------------------------------

(init-message 2 "Modules: bbdb")

(use-package bbdb
  :straight t
  :init
  ;;(bbdb-initialize)
  ;;(bbdb-initialize 'gnus 'message 'sc 'w3)
  (bbdb-initialize 'gnus 'message 'sc)
  :config
  ;; the following settings were copied from here:
  ;; http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html
  ;; TODO: look at these settings
  (setq bbdb-offer-save 1 ;; 1 means save-without-asking

        bbdb-use-pop-up t      ;; allow popups for addresses
        bbdb-electric-p t      ;; be disposable with SPC
        bbdb-popup-target-lines 1 ;; very small

        bbdb-dwim-net-address-allow-redundancy t ;; always use full name
        bbdb-quiet-about-name-mismatches 2 ;; show name-mismatches 2 secs

        bbdb-always-add-address t ;; add new addresses to existing contacts automatically
        bbdb-canonicalize-redundant-nets-p t ;; x@foo.bar.cx => x@bar.cx

        bbdb-completion-type nil ;; complete on anything

        bbdb-complete-name-allow-cycling t ;; cycle through matches (only partially works)

        bbbd-message-caching-enabled t ;; be fast
        bbdb-use-alternate-names t     ;; use AKA

        bbdb-elided-display t ;; single-line addresses

        ;; auto-create addresses from mail
        bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
        bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
        ;; NOTE: there can be only one entry per header (such as To, From)
        ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

        '(("From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))))

;; ;;------------------------------------------------------------------------------
;; ;;;; lookout
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "lookout")

;; ;; setup lookout for CSV import/export
;; (use-package lookout
;;   :straight (:type built-in)
;;   :after (bbdb)
;;   :config
;;   (progn
;;     ;; Thunderbird CSV fields:
;;     ;;
;;     ;; First Name, Last Name, Display Name, Nickname, Primary Email, Secondary
;;     ;; Email, Work Phone, Home Phone, Fax Number, Pager Number, Mobile Number,
;;     ;; Home Address, Home Address 2, Home City, Home State, Home ZipCode, Home
;;     ;; Country, Work Address, Work Address 2, Work City, Work State, Work
;;     ;; ZipCode, Work Country, Job Title, Department, Organization, Web Page 1,
;;     ;; Web Page 2, Birth Year, Birth Month, Birth Day, Custom 1, Custom 2,
;;     ;; Custom 3, Custom 4, Notes

;;     (defconst lookout-bbdb-mapping-table-thunderbird-english
;;       '(("lastname" "Last Name")
;;         ("firstname" "First Name")
;;         ("company" "Organization" "Department" "Job Title")
;;         ("net" "Primary Email" "Secondary Email")
;;         ("phones" "Mobile Number" "Work Phone" "Home Phone")
;;         ("addr1" "Home Address" "Home Address 2" "Home City" "Home State" "Home ZipCode" "Home Country")
;;         ("addr2" "Work Address" "Work Address 2" "Work City" "Work State" "Work ZipCode" "Work Country")
;;         ("addr3" "addr3")
;;         ("notes" "Notes")
;;         ("otherfields" "" ""))
;;       ;; ("otherfields"
;;       ;;  "group" "group"))
;;       "Mapping table, usable for input from Thunderbird CSV export.")
;;     (setq lookout-bbdb-mapping-table 'lookout-bbdb-mapping-table-thunderbird-english)

;;     ;; Outlook CSV fields:
;;     ;;
;;     ;; Title, First Name, Middle Name, Last Name, Suffix, Company, Department,
;;     ;; Job Title, Business Street, Business Street 2, Business Street 3,
;;     ;; Business City, Business State, Business Postal Code, Business Country,
;;     ;; Home Street, Home Street 2, Home Street 3, Home City, Home State, Home
;;     ;; Postal Code, Home Country, Other Street, Other Street 2, Other Street
;;     ;; 3, Other City, Other State, Other Postal Code, Other Country,
;;     ;; Assistant's Phone, Business Fax, Business Phone, Business Phone 2,
;;     ;; Callback, Car Phone, Company Main Phone, Home Fax, Home Phone, Home
;;     ;; Phone 2, ISDN, Mobile Phone, Other Fax, Other Phone, Pager, Primary
;;     ;; Phone, Radio Phone, TTY/TDD Phone, Telex, Account, Anniversary,
;;     ;; Assistant's Name, Billing Information, Birthday, Business Address PO
;;     ;; Box, Categories, Children, Directory Server, E-mail Address, E-mail
;;     ;; Type, E-mail Display Name, E-mail 2 Address, E-mail 2 Type, E-mail 2
;;     ;; Display Name, E-mail 3 Address, E-mail 3 Type, E-mail 3 Display Name,
;;     ;; Gender, Government ID Number, Hobby, Home Address PO Box, Initials,
;;     ;; Internet Free Busy, Keywords, Language, Location, Manager's Name,
;;     ;; Mileage, Notes, Office Location, Organizational ID Number, Other
;;     ;; Address PO Box, Priority, Private, Profession, Referred By,
;;     ;; Sensitivity, Spouse, User 1, User 2, User 3, User 4, Web Page

;;     (defconst lookout-bbdb-mapping-table-outlook-english
;;       '(("lastname" "Last Name" "Suffix")
;;         ("firstname" "Title" "First Name" "Middle Name")
;;         ("company" "Company" "Department" "Job Title")
;;         ("net" "E-mail Address" "E-mail 2 Address" "E-mail 3 Address")
;;         ("phones" "Business Phone" "Business Phone 2"
;;          "Home Phone" "Home Phone 2" "Mobile Phone" "Other Phone"
;;          "Assistant's Phone" "Business Fax" "Car Phone" "Callback"
;;          "Home Fax" "Other Fax" "Pager" "Primary Phone" "Radio Phone"
;;          "TTY/TDD Phone" "Telex")
;;         ("addr1" "Business Street" "Business Address PO Box" "Business Street 2" "Business Street 3"
;;          "Business City" "Business State" "Business Postal Code" "Business Country")
;;         ("addr2" "Home Street" "Home Address PO Box" "Home Street 2" "Home Street 3"
;;          "Home City" "Home State" "Home Postal Code" "Home Country")
;;         ("addr3" "Other Street" "Other Address PO Box" "Other Street 2" "Other Street 3"
;;          "Other City" "Other State" "Other Postal Code" "Other Country")
;;         ("notes" "Notes")
;;         ("otherfields" "" ""))
;;       ;; ("otherfields"
;;       ;;  "Account" "Anniversary" "Assistant's Name" "Billing Information"
;;       ;;  "Birthday" "Categories" "Children" "Directory Server" "Gender"
;;       ;;  "Government ID Number" "Hobby" "Initials" "Internet Free Busy"
;;       ;;  "Keywords" "Language" "Location" "Manager's Name" "Mileage"
;;       ;;  "Office Location" "Organizational ID Number" "Priority" "Private"
;;       ;;  "Profession" "Referred By" "Sensitivity" "Spouse"
;;       ;;  "User 1" "User 2" "User 3" "User 4" "Web Page"))
;;       "Mapping table, usable for input from Outlook CSV export.")
;;     (setq lookout-bbdb-mapping-table 'lookout-bbdb-mapping-table-outlook-english)

;;     ;; (defconst lookout-bbdb-mapping-table-outlook-german
;;     ;;   '(("lastname" "Nachname" "Suffix")
;;     ;;     ("firstname" "Vorname" "Weitere Vornamen")
;;     ;;     ("company" "Firma" "Abteilung" "Position")
;;     ;;     ("net" "E-Mail-Adresse" "E-Mail 2: Adresse" "E-Mail 3: Adresse")
;;     ;;     ("phones" "Telefon geschäftlich" "Telefon geschäftlich 2"
;;     ;;      "Telefon privat" "Telefon privat 2" "Mobiltelefon" "Weiteres Telefon"
;;     ;;      "Telefon Assistent" "Fax geschäftlich" "Autotelefon" "Telefon Firma"
;;     ;;      "Fax privat" "Weiteres Fax" "Pager" "Haupttelefon" "Mobiltelefon 2"
;;     ;;      "Telefon für Hörbehinderte" "Telex")
;;     ;;     ("addr1" "Straße geschäftlich" "Straße geschäftlich 2"
;;     ;;      "Straße geschäftlich 3" "Postleitzahl geschäftlich" "Ort geschäftlich"
;;     ;;      "Region geschäftlich" "Land geschäftlich")
;;     ;;     ("addr2" "Straße privat" "Straße privat 2" "Straße privat 3"
;;     ;;      "Postleitzahl privat" "Ort privat" "Region privat" "Land privat")
;;     ;;     ("addr3" "Weitere Straße" "Weitere Straße 2" "Weitere Straße 3"
;;     ;;      "Weitere Postleitzahl" "Weiterer Ort" "Weitere Region" "Weiteres Land")
;;     ;;     ("aka" "FIXME")
;;     ;;     ("notes" "Notizen")
;;     ;;     ("otherfields"
;;     ;;      "Rückmeldung" "Abrechnungsinformation" "Benutzer 1"
;;     ;;      "Benutzer 2" "Benutzer 3" "Benutzer 4" "Beruf" "Büro"
;;     ;;      "Empfohlen von" "Geburtstag" "Geschlecht" "Hobby" "Initialen"
;;     ;;      "Internet-Frei/Gebucht" "Jahrestag" "Kategorien" "Kinder" "Konto"
;;     ;;      "Name Assistent" "Name des/der Vorgesetzten"
;;     ;;      "Organisations-Nr." "Ort" "Partner" "Postfach" "Priorität" "Privat"
;;     ;;      "Regierungs-Nr." "Reisekilometer" "Sprache" "Stichwörter"
;;     ;;      "Vertraulichkeit" "Verzeichnisserver" "Webseite"))
;;     ;;   "Sample mapping table, usable for input from German M$ Outlook.")

;;     ;; alias for compatibility
;;     ;;(defalias 'bbdb-add-new-field 'bbdb-insert-new-field)
;;     ))
;; bbdb:1 ends here

;; [[file:init-emacs.org::#modules-beacon][beacon:1]]
;;------------------------------------------------------------------------------
;;; Packages: beacon
;;------------------------------------------------------------------------------

(init-message 2 "Modules: beacon")

(use-package beacon
  :straight t
  :demand t
  :custom
  ;; speed up duration (defaults to 0.3)
  (beacon-blink-duration 0.1)
  ;; speed up delay (defaults to 0.3)
  (beacon-blink-delay 0.1)
  :init (beacon-mode 1))
;; beacon:1 ends here

;; [[file:init-emacs.org::#modules-boxquote][boxquote:1]]
;;------------------------------------------------------------------------------
;;; Packages: boxquote
;;------------------------------------------------------------------------------

(init-message 2 "Modules: boxquote")

(use-package boxquote
  :straight t
  :init (unbind-key "C-c b")
  :bind (("C-c by" . boxquote-yank)
         ("C-c br" . boxquote-region)
         ("C-c bu" . boxquote-unbox-region)
         ("C-c bt" . boxquote-title)
         ("C-c bi" . boxquote-insert-file)
         ("C-c bk" . boxquote-kill)
         ("C-c bs" . boxquote-shell-command)
         ("C-c bb" . boxquote-buffer)
         ("C-c bp" . boxquote-paragraph)
         ("C-c bn" . boxquote-narrow-to-boxquote)
         ("C-c bw" . boxquote-where-is)
         ("C-c bdf" . boxquote-describe-function)
         ("C-c bdk" . boxquote-describe-key)
         ("C-c bdv" . boxquote-describe-variable)))
;; boxquote:1 ends here

;; [[file:init-emacs.org::#modules-browse-kill-ring][browse-kill-ring:1]]
;;------------------------------------------------------------------------------
;;; Packages: browse-kill-ring
;;------------------------------------------------------------------------------

(init-message 2 "Modules: browse-kill-ring")

(use-package browse-kill-ring
  :straight t
  :bind* (("C-M-y" . browse-kill-ring)
          ("C-M-_" . browse-kill-ring)))
;; browse-kill-ring:1 ends here

;; [[file:init-emacs.org::#modules-bs][bs:1]]
;;------------------------------------------------------------------------------
;;; Packages: bs
;;------------------------------------------------------------------------------

(init-message 2 "Modules: bs")

;; original code by Scott Frazer
(use-package bs
  :straight t
  :demand t
  :after (cycle-buffer)
  :commands (list-buffers bs-show)
  :bind* (([remap list-buffers] . bs-show) ; default: `list-buffers'
          ("C-x C-b" . bs-show))           ; default: `list-buffers'
  :bind (:map bs-mode-map
              ("C-n" . bs-down)
              ("C-p" . bs-up))
  :config
  (defvar custom-bs-always-show-regexps
    ;;'("\\*\\(scratch\\|info\\|grep\\)\\*")
    (list
     (rx (seq bos "*" (or "scratch" "info" "grep") "*" eos)))
    "*Buffer regexps to always show when buffer switching.")

  (defvar custom-bs-never-show-regexps
    ;;'("^\\s-" "^\\*" "TAGS$" "^Map_Sym.txt$" "^magit")
    (list
     (rx (or (seq bos space)
             (seq bos "*")
             (seq "TAGS" eos)
             (seq bos "Map_Sym.txt" eos)
             (seq bos "magit"))))
    "*Buffer regexps to never show when buffer switching.")

  (defvar custom-ido-ignore-dired-buffers nil
    "*If non-nil, buffer switching should ignore dired buffers.")

  (defun custom-bs-string-in-regexps (string regexps)
    "Return non-nil if STRING matches anything in REGEXPS list."
    (let ((case-fold-search nil))
      (catch 'done
        (dolist (regexp regexps)
          (when (string-match regexp string)
            (throw 'done t))))))

  (defun custom-bs-ignore-buffer (buffer)
    "Return non-nil if BUFFER should be ignored."
    (or (and (not (custom-bs-string-in-regexps buffer custom-bs-always-show-regexps))
             (custom-bs-string-in-regexps buffer custom-bs-never-show-regexps))
        (and custom-ido-ignore-dired-buffers
             (with-current-buffer buffer
               (equal major-mode 'dired-mode)))))

  (defun bs-toggle-recent ()
    "Toggle most recently visited buffers, ignoring certain ones."
    (interactive)
    (catch 'done
      (dolist (buffer (buffer-list))
        (unless (or (equal (current-buffer) buffer)
                    (and (fboundp 'my-bs-ignore-buffer)
                         (my-bs-ignore-buffer (buffer-name buffer))))
          (switch-to-buffer buffer)
          (throw 'done t)))))

  ;; config bs
  (setq bs-configurations
        '(("all" nil nil nil nil nil)
          ("files" nil nil nil (lambda (buffer) (custom-bs-ignore-buffer (buffer-name buffer))) nil))
        bs-cycle-configuration-name "files")

  ;; ;; add ignore rules to ido
  ;; (setq ido-ignore-buffers '(custom-bs-ignore-buffer))

  (defun custom-bs-cycle-buffer-filter-extra ()
    "Add ignore rules to `cycle-buffer'."
    (not (custom-bs-ignore-buffer (buffer-name))))
  (add-to-list 'cycle-buffer-filter-extra '(custom-bs-cycle-buffer-filter-extra) t)

  ;; customize `bs--up' so it does not wrap
  (defun bs--up ()
    "Move point vertically up one line."
    (when (> (count-lines 1 (point)) bs-header-lines-length)
      (forward-line -1)))

  ;; customize `bs--down' so it does not wrap
  (defun bs--down ()
    "Move point vertically down one line."
    (if (< (line-end-position) (point-max))
        (forward-line 1))))
;; bs:1 ends here

;; [[file:init-emacs.org::#modules-calc][calc:1]]
;;------------------------------------------------------------------------------
;;; Packages: calc
;;------------------------------------------------------------------------------

(init-message 2 "Modules: calc")

(use-package calc
  :straight (:type built-in)
  :commands (calc calc-dispatch)
  :bind* ("M-#" . calc-dispatch))
;; calc:1 ends here

;; [[file:init-emacs.org::#modules-cedet-semantic][cedet/semantic:1]]
;;------------------------------------------------------------------------------
;;; Packages: cedet/semantic
;;------------------------------------------------------------------------------

(init-message 2 "Modules: cedet/semantic")

(use-package cedet
  :straight (:type built-in))

(use-package semantic
  :straight (:type built-in)
  :after (cedet)
  :init (semantic-mode 1))
;; cedet/semantic:1 ends here

;; [[file:init-emacs.org::#packages-cheat-sh][cheat-sh:1]]
;;------------------------------------------------------------------------------
;;; Packages: cheat-sh
;;------------------------------------------------------------------------------

(init-message 2 "Packages: cheat-sh")

(use-package cheat-sh
  :straight t)
;; cheat-sh:1 ends here

;; [[file:init-emacs.org::#modules-command-log][command-log:1]]
;;------------------------------------------------------------------------------
;;; Packages: command-log
;;------------------------------------------------------------------------------

(init-message 2 "Modules: command-log")

(use-package command-log-mode
  :straight t
  :demand t
  :custom
  (command-log-mode-auto-show t)
  (command-log-mode-key-binding-open-log nil)
  (command-log-mode-open-log-turns-on-mode t)
  (command-log-mode-is-global t)
  :config
  (defun command-log-mode-on ()
    "Turn on `command-log-mode' and open the log buffer."
    (interactive)
    (global-command-log-mode 1))

  (defun command-log-mode-off ()
    "Turn off `command-log-mode' and close the log buffer."
    (interactive)
    (global-command-log-mode -1)))
;; command-log:1 ends here

;; [[file:init-emacs.org::#modules-compile][compile:1]]
;;------------------------------------------------------------------------------
;;; Packages: compile
;;------------------------------------------------------------------------------

(init-message 2 "Modules: compile")

(use-package compile
  :straight (:type built-in)
  :custom
  ;; auto save all modified buffers without asking
  (compilation-ask-about-save nil)
  ;; auto scroll compilation buffer
  (compilation-scroll-output 'next-error)
  ;; skip info and warnings
  (compilation-skip-threshold 2)
  :config
  ;; display compilation time in compile log
  ;; source: https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130

  (make-variable-buffer-local 'custom-compilation-start-time)

  (defun custom-compilation-start-hook (proc)
    (setq custom-compilation-start-time (current-time)))
  (add-hook 'compilation-start-hook #'custom-compilation-start-hook)

  (defun custom-compilation-finish-function (buf why)
    (let* ((elapsed (time-subtract nil custom-compilation-start-time))
           (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
      (save-excursion (goto-char (point-max)) (insert msg))
      (message "Compilation %s: %s" (string-trim-right why) msg)))
  (add-hook 'compilation-finish-functions #'custom-compilation-finish-function))
;; compile:1 ends here

;; [[file:init-emacs.org::#modules-cycle-buffer][cycle-buffer:1]]
;;------------------------------------------------------------------------------
;;; Packages: cycle-buffer
;;------------------------------------------------------------------------------

(init-message 2 "Modules: cycle-buffer")

(use-package cycle-buffer
  :load-path (lambda () (file-truename (expand-file-name "cycle-buffer.el" emacs-modules-dir)))
  :demand t
  :bind* (("C-x C-n" . cycle-buffer)          ; default: `set-goal-column'
          ("C-x C-p" . cycle-buffer-backward) ; default: `mark-page'
          ("<f9>" . cycle-buffer-backward)
          ("S-<f9>" . cycle-buffer-backward-permissive)
          ("<f10>" . cycle-buffer)      ; default: `tmm-menubar'
          ("S-<f10>" . cycle-buffer-permissive))
  :init
  ;; advise `cycle-buffer'
  (advice-add 'cycle-buffer :around #'advice--ignore-interactive-errors)
  ;; advise `cycle-buffer-permissive'
  (advice-add 'cycle-buffer-permissive :around #'advice--ignore-interactive-errors)
  ;; advise `cycle-buffer-backward'
  (advice-add 'cycle-buffer-backward :around #'advice--ignore-interactive-errors)
  ;; advise `cycle-buffer-backward-permissive'
  (advice-add 'cycle-buffer-backward-permissive :around #'advice--ignore-interactive-errors))
;; cycle-buffer:1 ends here

;; [[file:init-emacs.org::#modules-decide][decide:1]]
;;------------------------------------------------------------------------------
;;; Packages: decide
;;------------------------------------------------------------------------------

(init-message 2 "Modules: decide")

(use-package decide
  :straight t)
;; decide:1 ends here

;; [[file:init-emacs.org::#modules-demo-it][demo-it:1]]
;;------------------------------------------------------------------------------
;;; Packages: demo-it
;;------------------------------------------------------------------------------

(init-message 2 "Modules: demo-it")

(use-package demo-it
  :straight t)
;; demo-it:1 ends here

;; [[file:init-emacs.org::#modules-doom-modeline][doom-modeline:1]]
;;------------------------------------------------------------------------------
;;; Packages: doom-modeline
;;------------------------------------------------------------------------------

(init-message 2 "Modules: doom-modeline")

(use-package doom-modeline
  :straight t
  :after (all-the-icons)
  :demand t
  :custom
  ;; customizations
  (doom-modeline-height 30)
  :init (doom-modeline-mode 1)
  :config
  ;; show line number in modeline
  (line-number-mode 1)

  ;; show column number in modeline
  (column-number-mode 1))

;;------------------------------------------------------------------------------
;;;; all-the-icons
;;------------------------------------------------------------------------------

(init-message 3 "all-the-icons")

(use-package all-the-icons
  :straight t)
  ;; :config
  ;; ;; install fonts, if needed
  ;; (let ((font-dest (cl-case window-system
  ;;                    (x  (concat (or (getenv "XDG_DATA_HOME")
  ;;                                    (concat (getenv "HOME") "/.local/share"))
  ;;                                "/fonts/"))
  ;;                    (mac (concat (getenv "HOME") "/Library/Fonts/"))
  ;;                    (w32 (concat (getenv "WINDIR") "/Fonts/"))
  ;;                    (ns (concat (getenv "HOME") "/Library/Fonts/"))))
  ;;       (font-name (cl-case window-system
  ;;                    (w32 "AllTheIcons.ttf")
  ;;                    (t "all-the-icons.ttf"))))
  ;;   (or (file-exists-p (expand-file-name font-name font-dest))
  ;;       (all-the-icons-install-fonts :noconfirm))))
;; doom-modeline:1 ends here

;; [[file:init-emacs.org::#modules-easy-kill][easy-kill:1]]
;;------------------------------------------------------------------------------
;;; Packages: easy-kill
;;------------------------------------------------------------------------------

(use-package easy-kill
  :straight t
  :demand t
  :bind* (([remap kill-ring-save] . easy-kill)
          ([remap mark-sexp] . easy-mark)))
;; easy-kill:1 ends here

;; [[file:init-emacs.org::#packages-editorconfig][editorconfig:1]]
;;------------------------------------------------------------------------------
;;; Packages: editorconfig
;;------------------------------------------------------------------------------

(init-message 2 "Packages: editorconfig")

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))
;; editorconfig:1 ends here

;; [[file:init-emacs.org::#modules-eldoc][eldoc:1]]
;;------------------------------------------------------------------------------
;;; Packages: eldoc
;;------------------------------------------------------------------------------

(init-message 2 "Modules: eldoc")

(use-package eldoc
  :straight (:type built-in)
  :custom
  ;; no idle delay before showing contextual information
  (eldoc-idle-delay 0))
;; eldoc:1 ends here

;; [[file:init-emacs.org::#modules-elfeed][elfeed:1]]
;;------------------------------------------------------------------------------
;;; Packages: elfeed
;;------------------------------------------------------------------------------

(init-message 2 "Modules: elfeed")

(use-package elfeed
  :straight t
  :commands (elfeed-bookmarks-edit)
  :bind (:map elfeed-search-mode-map
              ("h" . elfeed-search-mode-help)
              ("?" . elfeed-search-mode-help))
  :bind (:map elfeed-show-mode-map
              ("h" . elfeed-show-mode-help)
              ("?" . elfeed-show-mode-help))
  :custom
  ;; use curl utility to fetch feeds
  (elfeed-use-curl nil)
  ;; reduce max connections for better performance
  (elfeed-curl-max-connections 6)
  ;; standard filters
  (elfeed-search-filter "-junk +unread")
  ;; custom feed list
  (elfeed-feeds
   (with-temp-buffer
     (insert-file-contents (locate-user-emacs-file "elfeed-bookmarks"))
     (goto-char (point-min))
     (mapcar
      (lambda (x) (list (plist-get x :rss) (intern (plist-get x :tag))))
      (read (current-buffer)))))
  :config
  ;; increase default text size in `elfeed-show' buffers
  (defun custom-elfeed-show-mode-hook ()
    ;; increase default text size
    (text-scale-set 2))
  (add-hook 'elfeed-show-mode-hook #'custom-elfeed-show-mode-hook)

  (defun elfeed-bookmarks-edit ()
    "Open `init-emacs.org' and move point to Elfeed Bookmarks File for easy editing."
    (interactive)
    (find-file (file-truename (expand-file-name "init-emacs.org" emacs-home-dir)))
    (goto-char (point-min))
    (search-forward ";; Elfeed Bookmarks File\n")
    (org-show-entry)
    (recenter-top-bottom scroll-margin))

  (defun elfeed-search-mode-help ()
    "Display `elfeed-search-mode' commands in mini-buffer."
    (interactive)
    (message (concat "RET show entry, "
                     "+ tag all, "
                     "- untag all, "
                     "G fetch, "
                     "S set filter, "
                     "b browse url, "
                     "g update, "
                     "n next line, "
                     "p prev line, "
                     "q quit, "
                     "r untag unread, "
                     "s live filter, "
                     "u tag unread, "
                     "y yank")))

  (defun elfeed-show-mode-help ()
    "Display `elfeed-show-mode' commands in mini-buffer."
    (interactive)
    (message (concat "TAB next link, "
                     "BACKTAB previous link, "
                     "SPC scroll down, "
                     "BACKSPACE scroll up,"
                     "+ tag, "
                     "- untag, "
                     "P play enclosure, "
                     "b visit, "
                     "d save enclosure, "
                     "g refresh, "
                     "n next, "
                     "p prev, "
                     "q kill buffer, "
                     "s new live search, "
                     "u copy url, "
                     "y yank")))

  (defun elfeed-bookmarks-to-opml ()
    "Export Elfeed Bookmarks File to OPML."
    (interactive)
    (let* ((buffer (generate-new-buffer-name "*elfeed-bookmarks-opml*"))
           (bookmarks
            (with-temp-buffer
              (insert-file-contents (locate-user-emacs-file "elfeed-bookmarks"))
              (goto-char (point-min))
              (read (current-buffer))))
           (tags
            (let (temp)
              (dolist (x (mapcar (lambda (x) (plist-get x :tag)) bookmarks))
                (cl-pushnew x temp :test #'string=))
              (nreverse temp))))
      (switch-to-buffer buffer)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<opml version=\"1.0\">\n")
      (insert "  <head>\n")
      (insert "    <title>Elfeed Bookmarks</title>\n")
      (insert "  </head>\n")
      (insert "  <body>\n")
      (dolist (tag tags)
        (let ((ctag (capitalize tag)))
          (insert "    <outline title=\"" ctag "\" text=\"" ctag "\">\n")
          (dolist (item
                   (remove-if
                    (lambda (x) (not (string= tag (plist-get x :tag))))
                    bookmarks))
            (let ((name (plist-get item :name))
                  (html (plist-get item :html))
                  (rss (plist-get item :rss)))
              (insert "      <outline title=\"" name "\" text=\"" name "\" type=\"rss\" xmlUrl=\"" rss "\" htmlUrl=\"" html "\"/>\n")))
          (insert "    </outline>\n")))
      (insert "  </body>\n")
      (insert "</opml>\n"))))
;; elfeed:1 ends here

;; [[file:init-emacs.org::#packages-elfeed-tube][elfeed-tube:1]]
;;------------------------------------------------------------------------------
;;; Packages: elfeed-tube
;;------------------------------------------------------------------------------

(init-message 2 "Packages: elfeed-tube")

(use-package elfeed-tube
  :straight t
  :after elfeed
  :bind (:map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save))
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save))
  :config
  (elfeed-tube-setup))

;; (use-package elfeed-tube-mpv
;;   :straight t
;;   :bind (:map elfeed-show-mode-map
;;               ("C-c C-f" . elfeed-tube-mpv-follow-mode)
;;               ("C-c C-w" . elfeed-tube-mpv-where)))
;; elfeed-tube:1 ends here

;; [[file:init-emacs.org::#modules-elnode][elnode:1]]
;;------------------------------------------------------------------------------
;;; Packages: elnode
;;------------------------------------------------------------------------------

(init-message 2 "Modules: elnode")

(use-package elnode
  :straight t
  :commands (elnode))
;; elnode:1 ends here

;; [[file:init-emacs.org::#modules-elpher][elpher:1]]
;;------------------------------------------------------------------------------
;;; Packages: elpher
;;------------------------------------------------------------------------------

(init-message 2 "Modules: elpher")

(use-package elpher
  :straight t
  :init
  (defun elpher-bookmarks-edit ()
    "Open `init-emacs.org' and move point to Elpher Bookmarks File for easy editing."
    (interactive)
    (find-file (file-truename (expand-file-name "init-emacs.org" emacs-home-dir)))
    (goto-char (point-min))
    (search-forward ";;;; Emacs Bookmark")
    (org-show-entry)
    (recenter-top-bottom scroll-margin)))
;; elpher:1 ends here

;; [[file:init-emacs.org::#packages-emacs-everywhere][emacs-everywhere:2]]
;;------------------------------------------------------------------------------
;;; Packages: emacs-everywhere
;;------------------------------------------------------------------------------

(init-message 2 "Packages: emacs-everywhere")

(use-package emacs-everywhere
  :straight t)
;; emacs-everywhere:2 ends here

;; [[file:init-emacs.org::#packages-epaint][epaint:1]]
;;------------------------------------------------------------------------------
;;; Packages: epaint
;;------------------------------------------------------------------------------

(init-message 2 "Packages: epaint")

(use-package epaint
  :straight (:type git :host github :repo "chuntaro/epaint"))
;; epaint:1 ends here

;; [[file:init-emacs.org::#modules-eperiodic][eperiodic:1]]
;;------------------------------------------------------------------------------
;;; Packages: eperiodic
;;------------------------------------------------------------------------------

(init-message 2 "Modules: eperiodic")

(use-package eperiodic
  :load-path (lambda () (file-truename (expand-file-name "eperiodic.el" emacs-modules-dir)))
  :commands (eperiodic))
;; eperiodic:1 ends here

;; [[file:init-emacs.org::#packages-epg][epg:1]]
;;------------------------------------------------------------------------------
;;; Packages: epg
;;------------------------------------------------------------------------------

(init-message 2 "Packages: epg")

(use-package epg
  :straight (:type built-in)
  :custom
  ;; do not prompt for password on every save
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  ;; redirect pinentry queries to caller
  ;; i.e. emacs will use its minibuffer to query passphrases
  (epg-pinentry-mode 'loopback))
;; epg:1 ends here

;; [[file:init-emacs.org::#modules-epoch][epoch:1]]
;;------------------------------------------------------------------------------
;;; Packages: epoch
;;------------------------------------------------------------------------------

(init-message 2 "Modules: epoch")

(use-package epoch
  :load-path (lambda () (file-truename (expand-file-name "epoch.el" local-modules-dir)))
  :commands (epoch time-to-epoch epoch-to-time))
;; epoch:1 ends here

;; [[file:init-emacs.org::#modules-ert][ert:1]]
;;------------------------------------------------------------------------------
;;; Packages: ert
;;------------------------------------------------------------------------------

(init-message 2 "Modules: ert")

(use-package ert
  :straight (:type built-in))
;; ert:1 ends here

;; [[file:init-emacs.org::#modules-exec-path-from-shell][exec-path-from-shell:1]]
;;------------------------------------------------------------------------------
;;; Packages: exec-path-from-shell
;;------------------------------------------------------------------------------

(init-message 2 "Modules: exec-path-from-shell")

(use-package exec-path-from-shell
  :when window-system-mac
  :straight t
  :init (exec-path-from-shell-initialize))
;; exec-path-from-shell:1 ends here

;; [[file:init-emacs.org::#modules-expand-region][expand-region:1]]
;;------------------------------------------------------------------------------
;;; Packages: expand-region
;;------------------------------------------------------------------------------

(init-message 2 "Modules: expand-region")

(use-package expand-region
  :straight t
  :bind* (("C-=" . er/expand-region)     ; default: `count-lines-region'
          ("C-+" . er/contract-region))) ; default: `negative-argument'
          ;;("C--" . er/contract-region))) ; default: `negative-argument'
;; expand-region:1 ends here

;; [[file:init-emacs.org::#modules-flycheck][flycheck:1]]
;;------------------------------------------------------------------------------
;;; Packages: flycheck
;;------------------------------------------------------------------------------

(init-message 2 "Modules: flycheck")

(use-package flycheck
  :straight t
  :hook (after-init . global-flycheck-mode))

;;------------------------------------------------------------------------------
;;;; flycheck-package
;;------------------------------------------------------------------------------

(init-message 3 "flycheck-package")

;; flycheck checker for elisp packages
(use-package flycheck-package
  :straight t
  :after (flycheck)
  :config
  (flycheck-package-setup))
;; flycheck:1 ends here

;; [[file:init-emacs.org::#modules-flymake-cursor][flymake-cursor:1]]
;;------------------------------------------------------------------------------
;;; Packages: flymake-cursor
;;------------------------------------------------------------------------------

(init-message 2 "Modules: flymake-cursor")

(use-package flymake-cursor
  :straight t)
;; flymake-cursor:1 ends here

;; [[file:init-emacs.org::#modules-flyspell][flyspell:1]]
;;------------------------------------------------------------------------------
;;; Packages: flyspell
;;------------------------------------------------------------------------------

(init-message 2 "Modules: flyspell")

(use-package flyspell
  :straight (:type built-in)
  :commands (flyspell-mode
             flyspell-mode-off
             flyspell-prog-mode)
  :custom
  ;; this fixes the "enabling flyspell mode gave an error" bug
  (flyspell-issue-welcome-flag nil)
  (flyspell-sort-corrections nil)
  (flyspell-use-meta-tab nil))
;; flyspell:1 ends here

;; [[file:init-emacs.org::#modules-fuzzy][fuzzy:1]]
;;------------------------------------------------------------------------------
;;; Packages: fuzzy
;;------------------------------------------------------------------------------

(init-message 2 "Modules: fuzzy")

(use-package fuzzy
  :straight t
  :commands (turn-on-fuzzy-isearch)
  :init (turn-on-fuzzy-isearch))
;; fuzzy:1 ends here

;; [[file:init-emacs.org::#packages-gcmh][gcmh:1]]
;;------------------------------------------------------------------------------
;;; Packages: gcmh
;;------------------------------------------------------------------------------

(init-message 2 "Packages: gcmh")

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))
;; gcmh:1 ends here

;; [[file:init-emacs.org::#packages-hide-mode-line][hide-mode-line:1]]
;;------------------------------------------------------------------------------
;;; Packages: hide-mode-line
;;------------------------------------------------------------------------------

(init-message 2 "Packages: hide-mode-line")

(use-package hide-mode-line
  :straight t)
;; hide-mode-line:1 ends here

;; [[file:init-emacs.org::#modules-hippie-exp][hippie-exp:1]]
;;------------------------------------------------------------------------------
;;; Packages: hippie-exp
;;------------------------------------------------------------------------------

(init-message 2 "Modules: hippie-exp")

(use-package hippie-exp
  :straight (:type built-in)
  :bind* (("M-/" . hippie-expand)         ; default: `dabbrev-expand'
          ("C-M-/" . completion-at-point))
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     yas-hippie-try-expand)))
;; hippie-exp:1 ends here

;; [[file:init-emacs.org::#modules-htmlize][htmlize:1]]
;;------------------------------------------------------------------------------
;;; Packages: htmlize
;;------------------------------------------------------------------------------

(init-message 2 "Modules: htmlize")

(use-package htmlize
  :straight t
  :commands (htmlize-buffer
             htmlize-region
             htmlize-file
             htmlize-many-file
             htmlize-many-files-dired
             htmlize-region-for-paste
             htmlize-region-for-paste-font-type)
  :custom
  (htmlize-output-type 'inline-css)
  :config
  ;; flatland theme
  (setq htmlize-face-overrides
        '(
          font-lock-comment-face (:foreground "#798188")
          font-lock-string-face (:foreground "#cfe2f2")
          font-lock-keyword-face (:foreground "#fa9a4b" :weight bold)
          font-lock-builtin-face (:foreground "#fa9a4b" :weight bold)
          font-lock-function-name-face (:foreground "#72aaca")
          font-lock-variable-name-face (:foreground "#f6f080")
          font-lock-type-face (:foreground "#72aaca")
          font-lock-constant-face (:foreground "#b9d977")
          font-lock-warning-face (:foreground "#f1e94b" :weight bold)
          default (:foreground "green" :background "black"))))
;; htmlize:1 ends here

;; [[file:init-emacs.org::#modules-hungry-delete][hungry-delete:1]]
;;------------------------------------------------------------------------------
;;; Packages: hungry-delete
;;------------------------------------------------------------------------------

(init-message 2 "Modules: hungry-delete")

(use-package hungry-delete
  :straight t
  :demand t
  :commands (global-hungry-delete-mode
             hungry-delete-skip-ws-forward
             hungry-delete-skip-ws-backward)
  :init
  ;; disable hungry delete mode globally
  (global-hungry-delete-mode -1)

  ;; custom versions of hungry delete functions that do not delete the space
  ;; at point unless it is the last one

  (defun hungry-delete-forward ()
    "Delete the following character or all following whitespace
up to the next non-whitespace character. See
\\[c-hungry-delete-backward]."
    (interactive)
    (let ((here (point)))
      (hungry-delete-skip-ws-forward)
      (when (> (point) here)
        (forward-char -1))
      (if (/= (point) here)
          (delete-region (point) here)
        (let ((hungry-delete-mode nil))
          (delete-char 1)))))

  (defun hungry-delete-backward ()
    "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character. See also
\\[c-hungry-delete-forward]."
    (interactive)
    (let ((here (point)))
      (hungry-delete-skip-ws-backward)
      (when (< (point) here)
        (forward-char 1))
      (if (/= (point) here)
          (delete-region (point) here)
        (let ((hungry-delete-mode nil))
          (delete-char -1))))))
;; hungry-delete:1 ends here

;; [[file:init-emacs.org::#modules-ibuffer][ibuffer:1]]
;;------------------------------------------------------------------------------
;;; Packages: ibuffer
;;------------------------------------------------------------------------------

(init-message 2 "Modules: ibuffer")

(use-package ibuffer
  :straight (:type built-in)
  :bind* ("C-x i" . ibuffer)            ; default: `insert-file'
  :commands (ibuffer)
  :config
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Apropos\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Compile-Log\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Completions\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*ftp .*\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*grep\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Help\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*IBuffer\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*inferior-lisp\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*JDEE bsh\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Messages\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Occur\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*RE-Builder\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*Shell Command Output\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*slime-events\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*tramp/.*\\*$" t)
  ;; (add-to-list 'ibuffer-never-show-regexps "^\\*WoMan-Log*\\*$" t)

  ;; default groups for ibuffer
  (setq ibuffer-saved-filters
        '(("default"
           ("c" (mode . c-mode))
           ("calendar" (or
                        (name . "^\\*Calendar\\*$")
                        (name . "^\\*Remind\\*$")
                        (name . "^diary$")))
           ("dired" (mode . dired-mode))
           ("elisp" (mode . emacs-lisp-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("erc" (mode . erc-mode))
           ("gnus" (or
                    (mode . message-mode)
                    (mode . bbdb-mode)
                    (mode . mail-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (name . "^\\.bbdb$")
                    (name . "^\\.newsrc-dribble")))
           ("java" (or
                    (mode . java-mode)
                    (mode . jde-mode)))
           ("kotlin" (mode . kotlin-mode))
           ("lisp" (mode . lisp-mode))
           ("org" (mode . org-mode))
           ("perl" (mode . perl-mode))
           ("python" (mode . python-mode))
           ("racket" (mode . racket-mode))
           ("ruby" (mode . ruby-mode)))))

  (defun custom-ibuffer-mode-hook ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook #'custom-ibuffer-mode-hook))
;; ibuffer:1 ends here

;; [[file:init-emacs.org::#packages-isearch][isearch:1]]
;;------------------------------------------------------------------------------
;;; Packages: isearch
;;------------------------------------------------------------------------------

(init-message 2 "Packages: isearch")

(use-package isearch
  :straight (:type built-in)
  :bind (:map isearch-mode-map
              ("C-M-<backspace>" . isearch-clear))
  :config
  (defun isearch-clear ()
    "Clear `isearch' search string."
    (interactive)
    (isearch-del-char most-positive-fixnum)))
;; isearch:1 ends here

;; [[file:init-emacs.org::#modules-iedit][iedit:1]]
;;------------------------------------------------------------------------------
;;; Packages: iedit
;;------------------------------------------------------------------------------

(init-message 2 "Modules: iedit")

(use-package iedit
  :straight t
  :commands (iedit-mode)
  :bind* ("C-x ;" . iedit-mode))
;; iedit:1 ends here

;; [[file:init-emacs.org::#modules-imdb][+imdb+:1]]
;; ;;------------------------------------------------------------------------------
;; ;;; Packages: imdb
;; ;;------------------------------------------------------------------------------

;; (init-message 2 "Modules: imdb")

;; (use-package imdb
;;   :straight (:type built-in))
;; +imdb+:1 ends here

;; [[file:init-emacs.org::#modules-ini][ini:1]]
;;------------------------------------------------------------------------------
;;; Packages: ini
;;------------------------------------------------------------------------------

(init-message 2 "Modules: ini")

(use-package ini
  :straight (ini :type git :host github :repo "daniel-ness/ini.el")
  :commands (ini-decode
             ini-encode))
;; ini:1 ends here

;; [[file:init-emacs.org::#modules-ispell][ispell:1]]
;;------------------------------------------------------------------------------
;;; Packages: ispell
;;------------------------------------------------------------------------------

(init-message 2 "Modules: ispell")

(use-package ispell
  :straight (:type built-in)
  :commands (ispell-buffer
             ispell-change-dictionary-hook
             ispell-complete-word-dict
             ispell-message
             ispell-region-end
             ispell-set-spellchecker-params
             ispell-word)
  :bind (("<f6>" . ispell-word)
         ("<S-f6>" . ispell))
  :config
  (setq ispell-enable-tex-parser t))
;; ispell:1 ends here

;; [[file:init-emacs.org::#modules-json][json:1]]
;;------------------------------------------------------------------------------
;;; Packages: json
;;------------------------------------------------------------------------------

(init-message 2 "Modules: json")

(use-package json
  :straight (:type built-in))
;; json:1 ends here

;; [[file:init-emacs.org::#modules-key-chord][key-chord:1]]
;;------------------------------------------------------------------------------
;;; Packages: key-chord
;;------------------------------------------------------------------------------

(init-message 2 "Modules: key-chord")

(use-package key-chord
  :straight t
  :demand t
  :init
  ;; turn on `key-chord-mode'
  (key-chord-mode 1)
  :config
  ;; key chords
  (key-chord-define-global ",." "<>\C-b")
  (key-chord-define-global "hj" 'undo)
  (key-chord-define-global "fg" 'undo-tree-redo)
  (key-chord-define-global "jk" 'dabbrev-expand)
  (key-chord-define-global "cv" 'reindent-then-newline-and-indent)
  (key-chord-define-global "1q" "!")
  (key-chord-define-global "2w" "@")
  (key-chord-define-global "3e" "#")
  (key-chord-define-global "4r" "$")
  (key-chord-define-global "5t" "%")
  (key-chord-define-global "6y" "^")
  (key-chord-define-global "7y" "&")
  (key-chord-define-global "8u" "*")
  (key-chord-define-global "9i" "(")
  (key-chord-define-global "0o" ")"))
;; key-chord:1 ends here

;; [[file:init-emacs.org::#modules-keyfreq][keyfreq:1]]
;;------------------------------------------------------------------------------
;;; Packages: keyfreq
;;------------------------------------------------------------------------------

(init-message 2 "Modules: keyfreq")

(use-package keyfreq
  :straight t
  :demand t
  :custom
  (keyfreq-file (expand-file-name ".emacs.keyfreq" user-emacs-directory))
  (keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" user-emacs-directory))
  :init
  ;; turn on `keyfreq-mode'
  (keyfreq-mode 1)
  :config
  ;; auto-save stats
  (keyfreq-autosave-mode 1))
;; keyfreq:1 ends here

;; [[file:init-emacs.org::#modules-langtool][langtool:2]]
;;------------------------------------------------------------------------------
;;; Packages: langtool
;;------------------------------------------------------------------------------

(init-message 2 "Modules: langtool")

(use-package langtool
  :when (executable-find "languagetool") ; only use if binary is available on system
  :straight t
  :bind* (("C-x 4 w" . langtool-check)
          ("C-x 4 W" . langtool-check-done)
          ("C-x 4 l" . langtool-switch-default-language)
          ("C-x 4 4" . langtool-show-message-at-point)
          ("C-x 4 c" . langtool-correct-buffer))
  :custom
  (langtool-java-classpath
   (concat "/usr/share/java/languagetool"
           ":/usr/share/java/languagetool/*"
           ":/usr/share/languagetool"
           ":/usr/share/languagetool/*")))
;; langtool:2 ends here

;; [[file:init-emacs.org::#packages-lorem-ipsum][lorem-ipsum:1]]
;;------------------------------------------------------------------------------
;;; Packages: lorem-ipsum
;;------------------------------------------------------------------------------

(init-message 2 "Packages: lorem-ipsum")

(use-package lorem-ipsum
  :straight t)
;; lorem-ipsum:1 ends here

;; [[file:init-emacs.org::#packages-lorem-ipsum-overlay][lorem-ipsum-overlay:1]]
;;------------------------------------------------------------------------------
;;; Packages: lorem-ipsum-overlay
;;------------------------------------------------------------------------------

(init-message 2 "Packages: lorem-ipsum-overlay")

(defcustom lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `lorem-ipsum-overlay'."
  :type '(repeat regexp))

(setq lorem-ipsum-overlay-exclude
      `(,(rx (or bol bos blank)
             "#+"
             (one-or-more alnum)
             ":"
             (or eol eos blank))))

(defun lorem-ipsum-overlay (&optional replace-p)
  "Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays. Useful for taking screenshots
without revealing buffer contents.

If REPLACE-P is non-nil (interactively, with prefix), replace
buffer contents rather than overlaying them. When a buffer is
very large and would have so many overlays that performance would
be prohibitively slow, you may replace the buffer contents
instead. (Of course, be careful about saving the buffer after
replacing its contents.)

Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid. Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid. For example, in an Org buffer, a line that starts
with:

#+TITLE: unpackaged.el

Could be matched against the exclude regexp (in `rx' syntax):

(rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))

And the line would be overlaid like:

#+TITLE: parturient.et"
  (interactive "P")
  (require 'lorem-ipsum)
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :lorem-ipsum-overlay))
        ;; remove overlays
        (dolist (ov ovs)
          (when (overlay-get ov :lorem-ipsum-overlay)
            (delete-overlay ov)))
      ;; add overlays
      (let ((lorem-ipsum-words (--> lorem-ipsum-text
                                    (-flatten it) (apply #'concat it)
                                    (split-string it (rx (or space punct)) 'omit-nulls)))
            (case-fold-search nil))
        (cl-labels
            ((overlay-group (group)
                            (let* ((beg (match-beginning group))
                                   (end (match-end group))
                                   (replacement-word (lorem-word (match-string group)))
                                   (ov (make-overlay beg end)))
                              (when replacement-word
                                (overlay-put ov :lorem-ipsum-overlay t)
                                (overlay-put ov 'display replacement-word))))
             (replace-group (group)
                            (let* ((beg (match-beginning group))
                                   (end (match-end group))
                                   (replacement-word (lorem-word (match-string group))))
                              (when replacement-word
                                (setf (buffer-substring beg end) replacement-word))))
             (lorem-word (word)
                         (if-let* ((matches (lorem-matches (length word))))
                             (apply-case word (downcase (seq-random-elt matches)))
                           ;; word too long; compose one
                           (apply-case word (downcase (compose-word (length word))))))
             (lorem-matches (length &optional (comparator #'=))
                            (cl-loop for liw in lorem-ipsum-words
                                     when (funcall comparator (length liw) length)
                                     collect liw))
             (apply-case (source target)
                         (cl-loop for sc across-ref source
                                  for tc across-ref target
                                  when (not (string-match-p (rx lower) (char-to-string sc)))
                                  do (setf tc (string-to-char (upcase (char-to-string tc)))))
                         target)
             (compose-word (length)
                           (cl-loop while (> length 0)
                                    for word = (seq-random-elt (lorem-matches length #'<=))
                                    concat word
                                    do (cl-decf length (length word)))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (rx (group (one-or-more (or bol bos blank (not alpha)))
                                                 (zero-or-more (not (any alpha blank)))
                                                 (group (one-ore-more alpha))
                                                 (zero-or-more (not (any alpha blank)))))
                                      nil t)
              (unless (cl-member (match-string 0) lorem-ipsum-overlay-exclude
                                 :test (lambda (string regexp)
                                         (string-match-p regexp string)))
                (if replace-p
                    (replace-group 2)
                  (overlay-group 2)))
              (goto-char (match-end 2)))))))))
;; lorem-ipsum-overlay:1 ends here

;; [[file:init-emacs.org::#modules-magit][magit:1]]
;;------------------------------------------------------------------------------
;;; Packages: magit
;;------------------------------------------------------------------------------

(init-message 2 "Modules: magit")

(use-package magit
  :straight t
  ;; :after (ivy)
  :diminish magit-auto-revert-mode
  :bind* (("C-x g" . magit-status)
          ("C-x M-g" . magit-dispatch))
  :bind (:map magit-status-mode-map
              ("W" . magit-toggle-whitespace)
              ("C-c C-a" . magit-commit-amend-without-prompt))
  :bind (:map magit-mode-map
              ("v" . magit-visit-pull-request-url))
  ;; :custom
  ;; ;; use ivy
  ;; (magit-completing-read-function #'ivy-completing-read)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defun magit-toggle-whitespace ()
    "Toggle whitespace ignoring."
    (interactive)
    (let ((ws "--ignore-all-space"))
      (if (member ws magit-diff-arguments)
          (setq magit-diff-arguments (remove ws magit-diff-arguments))
        (add-to-list 'magit-diff-arguments ws t))
      (magit-refresh)))

  (defun magit-commit-amend-without-prompt ()
    "Amend without any prompt."
    (interactive)
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))

  ;; Author: http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
  (defun magit-visit-pull-request-url ()
    "Visit the current branch's pull request on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "^.+github\\.com:\\(.+\\)\\.git$" "\\1"
              (magit-get "remote"
                         (magit-get-remote nil)
                         "url"))
             (cdr (magit-get-push-branch))))))

;; ;;------------------------------------------------------------------------------
;; ;;;; forge
;; ;;
;; ;; Access Git forges from Magit.
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "forge")

;; (use-package forge
;;   :straight t
;;   :after (magit))
;; magit:1 ends here

;; [[file:init-emacs.org::#packages-mastodon][mastodon:1]]
;;------------------------------------------------------------------------------
;;; Packages: mastodon
;;------------------------------------------------------------------------------

(init-message 2 "Packages: mastodon")

(use-package mastodon
  :straight t
  :config
  (let* ((env (expand-file-name "~/.mastodon"))
         (url (string-trim (shell-command-to-string (concat "sed -n 's/emacs.ch-url=//p' " env))))
         (username (string-trim (shell-command-to-string (concat "sed -n 's/emacs.ch-username=//p' " env)))))
    (setq mastodon-instance-url url
          mastodon-active-user username)))
;; mastodon:1 ends here

;; [[file:init-emacs.org::#modules-mingus][mingus:1]]
;;------------------------------------------------------------------------------
;;; Packages: mingus
;;------------------------------------------------------------------------------

(init-message 2 "Modules: mingus")

(use-package mingus
  :straight t
  :commands (mingus
             mingus-create-NP-mark
             mingus-get-details
             mingus-get-insertion-number
             mingus-get-new-playlist-version
             mingus-goto-line
             mingus-set-insertion-point
             mingus-move
             mingus-move-NP-mark
             mingus-playlist-length
             mingus-set-song-pos
             mingus-switch-to-playlist
             mingus-truncate-string
             mpd-execute-command
             mpd-get-status)
  :bind* ("C-x /" . mingus-switch-to-buffer)
  :bind (:map mingus-global-map
              ("<left>" . backward-char)
              ("<right>" . forward-char)
              ("<home>" . beginning-of-line)
              ("<end>" . end-of-line)
              ("C-c C-u" . mingus-mpc-update))
  :bind (:map mingus-playlist-map
              ("SPC" . mingus-pause)
              ("<left>" . backward-char)
              ("<right>" . forward-char)
              ("<home>" . beginning-of-line)
              ("<end>" . end-of-line)
              ("C-c C-e" . mingus-edit-id3tag)
              ("C-c C-l" . mingus-get-lyrics)
              ("C-c C-u" . mingus-mpc-update)
              ("<f1>" . mingus-set-song-rating-1)
              ("<f2>" . mingus-set-song-rating-2)
              ("<f3>" . mingus-set-song-rating-3)
              ("<f4>" . mingus-set-song-rating-4)
              ("<f5>" . mingus-set-song-rating-5)
              ("<f6>" . mingus-set-song-rating-0))
  :bind (:map mingus-browse-map
              ("C-c C-u" . mingus-mpc-update))
  :config
  ;; for some reason these are not being defined in libmpdee.el
  (defmacro _mpdgv () `(aref conn 0))
  (defmacro _mpdsv (val) `(aset conn 0 ,val))
  (defmacro _mpdgo () `(aref conn 1))
  (defmacro _mpdso (val) `(aset conn 1 ,val))
  (defmacro _mpdgb () `(aref conn 2))
  (defmacro _mpdsb (val) `(aset conn 2 ,val))
  (defmacro _mpdgl () `(aref conn 3))
  (defmacro _mpdsl (val) `(aset conn 3 ,val))
  (defmacro _mpdgs () `(aref conn 4))
  (defmacro _mpdss (val) `(aset conn 4 ,val))
  (defmacro _mpdgf () `(aref conn 5))
  (defmacro _mpdsf (val) `(aset conn 5 ,val))
  (defmacro _mpdgt () `(aref conn 6))
  (defmacro _mpdst (val) `(aset conn 6 ,val))
  (defmacro _mpdgh () `(aref conn 7))
  (defmacro _mpdsh (val) `(aset conn 7 ,val))
  (defmacro _mpdgp () `(aref conn 8))
  (defmacro _mpdsp (val) `(aset conn 8 ,val))
  (defmacro _mpdga () `(aref conn 9))
  (defmacro _mpdsa (val) `(aset conn 9 ,val))

  (defun custom-mingus-hook-custom-settings ()
    ;; disable undo
    (buffer-disable-undo)

    ;; turn off the display of trailing whitespace
    (setq show-trailing-whitespace nil)

    ;; ;; highlight current line
    ;; (hl-line-mode 1)
    )
  (add-hook 'mingus-playlist-hooks #'custom-mingus-hook-custom-settings)
  (add-hook 'mingus-browse-hook #'custom-mingus-hook-custom-settings)

  ;; set mpd config file location
  (setq mingus-mpd-config-file "~/.mpd/mpd.conf")

  ;; set mpd music directory location
  (defcustom mingus-mpd-music-dir nil
    "mpd music directory.")
  (setq mingus-mpd-music-dir "~/.mpd/music/")

  ;; sort case-insensitive
  (setq mingus-fold-case t)

  ;; use ido-mode for searching
  (setq mingus-use-ido-mode-p t)

  ;; do not use the mouse
  (setq mingus-use-mouse-p nil)

  ;; custom playlist song format
  (setq mingus-format-song-function 'mingus-format-song-custom-columns)

  (defun mingus-switch-to-buffer ()
    "Start mingus or switch to it if already running."
    (interactive)
    (if (get-buffer "*Mingus*")
        (switch-to-buffer "*Mingus*")
      (mingus)))

  (defun mingus-truncate-string (string length)
    (truncate-string-to-width string (max 1 length) nil 32 "..."))

  (defun mingus-format-song-custom-columns (plist)
    "Custom column playlist song format."
    (let* ((show-albums (>= (window-text-width) 140))
           (available-width (- (window-text-width) 19))
           (song-width (/ available-width (if show-albums 3 2)))
           (artist-width (/ available-width (if show-albums 3 2)))
           (album-width (/ available-width 3))
           (string
            (concat
             (format "% 4d.%.2d  "
                     (/ (or (plist-get plist 'Time) 0) 60)
                     (mod (or (plist-get plist 'Time) 0) 60))
             (mingus-truncate-string
              (or (plist-get plist 'Title)
                  (plist-get plist 'Name)
                  (plist-get plist 'file))
              song-width)
             (concat "  "
                     (mingus-truncate-string
                      (or (plist-get plist 'Artist)
                          (plist-get plist 'AlbumArtist)
                          "")
                      artist-width))
             (if show-albums
                 (concat "  "
                         (mingus-truncate-string
                          (or (plist-get plist 'Album) "")
                          album-width))
               ""))))
      string))

  (defun mingus-format-song-custom-simple (plist &optional separator)
    "Make a string from PLIST.

Concatenate the results for the values with SEPARATOR, where SEPARATOR
defaults to the string \" - \"."
    (let ((artist (plist-get plist 'Artist))
          (album (plist-get plist 'Album))
          (title (plist-get plist 'Title))
          (albumartist (plist-get plist 'Albumartist))
          (track (plist-get plist 'Track))
          (name (plist-get plist 'Name))
          (genre (plist-get plist 'Genre))
          (date (plist-get plist 'Date))
          (composer (plist-get plist 'Composer))
          (performer (plist-get plist 'Performer))
          (comment (plist-get plist 'Comment))
          (disc (plist-get plist 'Disc))
          (time (plist-get plist 'Time))
          (pos (plist-get plist 'Pos))
          (id (plist-get plist 'Id))
          (file (plist-get plist 'file))
          (separator (or separator " - ")))
      (or (and mingus-use-caching
               (gethash id mingus-song-strings))
          (let ((val
                 (let* ((file (and file (file-name-nondirectory file)))
                        (short (remove nil (list (or artist albumartist)
                                                 (or title file)))))
                   (mapconcat 'identity short separator))))
            (and mingus-use-caching
                 (puthash id val mingus-song-strings))
            val))))

  ;; recenter screen after moving to current song
  (defun mingus-goto-current-song--recenter ()
    "Recenter screen after moving to current song."
    (recenter-top-bottom))
  ;; advise `mingus-goto-current-song'
  (advice-add 'mingus-goto-current-song :after #'mingus-goto-current-song--recenter)

  (defun mingus-insert-song-rating (rating)
    "Insert song rating at current position."
    (insert (format "  [%s]" rating)))

  (defun mingus-display-song-rating (highlight)
    "Display song rating of currently selected mingus song.

If HIGHLIGHT is non-nil, highlight song."
    (let ((buffer "*Mingus*"))
      (unless (string= (buffer-name) buffer)
        (user-error "Current buffer is not: %s" buffer))
      (let (buffer-read-only)
        (save-mark-and-excursion
          (save-match-data
            (forward-line 0)
            (when (re-search-forward "  \\[[0-9]\\]$" (line-end-position) :noerror)
              (replace-match ""))
            (goto-char (line-end-position))
            (mingus-insert-song-rating (or (mingus-get-song-rating) 0))
            (when highlight
              (put-text-property (line-beginning-position) (line-end-position) 'face 'font-lock-keyword-face)))))))

  (defun mingus-set-NP-mark--highlight-current-song (orig-fun &rest args)
    "Mark entire line of currently playing song.

Use text properties to mark the line then call `mingus-set-NP-mark'."
    (when (string= (buffer-name) "*Mingus*")
      (ignore-errors
        (let ((pos (or (and (> (length args) 1) (cadr args))
                       (plist-get (mpd-get-status mpd-inter-conn) 'song)))
              buffer-read-only)
          (when pos
            (save-mark-and-excursion
              (save-window-excursion
                (remove-text-properties (point-min) (point-max) '(face nil))
                (mingus-goto-line (1+ pos))
                (mingus-display-song-rating t)))))))
    (apply orig-fun args))
  ;; advise `mingus-set-NP-mark'
  (advice-add 'mingus-set-NP-mark :around #'mingus-set-NP-mark--highlight-current-song)

  (defun mingus-get-all-song-ratings ()
    "Return all song ratings as a hash table of songs to ratings."
    (let ((ratings (make-hash-table :test 'equal)))
      (dotimes (x 5)
        (let* ((rating (1+ x))
               (file (file-truename
                      (expand-file-name
                       (concat mingus-ratings-prefix (number-to-string rating))
                       mingus-ratings-directory))))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (not (eobp))
              (puthash (buffer-substring-no-properties (line-beginning-position) (line-end-position)) rating ratings)
              (forward-line 1)))))
      ratings))

  (defun mingus-display-all-song-ratings ()
    "Display all song ratings in mingus playlist buffer."
    (when (string= (buffer-name) "*Mingus*")
      (save-mark-and-excursion
        (save-window-excursion
          (let ((ratings (mingus-get-all-song-ratings))
                buffer-read-only)
            (buffer-disable-undo)
            (goto-char (point-min))
            (while (not (eobp))
              (ignore-errors
                (let ((rating (get-byte (- (line-end-position) 2))))
                  (unless (and (= (get-byte (- (line-end-position) 1)) 93)
                               (= (get-byte (- (line-end-position) 3)) 91)
                               (>= rating 48)
                               (<= rating 53))
                    (let ((details (mingus-get-details)))
                      (when details
                        (let* ((file (plist-get details 'file))
                               (rating (gethash file ratings)))
                          (goto-char (line-end-position))
                          (mingus-insert-song-rating (number-to-string (or rating 0)))))))))
              (forward-line 1)))))))
  ;; advise `mingus-switch-to-playlist'
  (advice-add 'mingus-switch-to-playlist :after #'mingus-display-all-song-ratings)
  ;; advise `mingus-redraw-buffer'
  (advice-add 'mingus-redraw-buffer :after #'mingus-display-all-song-ratings)

  (defun mingus-mpc-update ()
    "Update (refresh) mpc (Music Player Client)"
    (interactive)
    (message "Updating mpc (Music Player Client)...")
    (shell-command "mpc --wait update > /dev/null 2>&1"))

  (defun mingus-edit-id3tag--finish-edit ()
    "Save any id3 tag changes and close the edit window."
    (interactive)
    (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
    (kill-buffer (current-buffer)))

  (defun mingus-edit-id3tag--abort-edit ()
    "Close the edit window without saving changes."
    (interactive)
    (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
    (kill-buffer (current-buffer)))

  (defun mingus-edit-id3tag ()
    "Edit id3 tag of the selected song."
    (interactive)
    (let ((id3tag "id3tag")
          (details (mingus-get-details)))
      (unless (executable-find id3tag)
        (user-error "Could not find system command: %s" id3tag))
      (when details
        (let ((buffer (get-buffer-create "*Mingus Edit ID3 Tags*"))
              (keymap (make-sparse-keymap))
              (artist (plist-get details 'Artist))
              (album (plist-get details 'Album))
              (title (plist-get details 'Title))
              (track (plist-get details 'Track))
              (genre (plist-get details 'Genre))
              (date (plist-get details 'Date))
              (file (plist-get details 'file)))
          (bind-keys :map keymap
                     ("<return>" . newline)
                     ("C-c C-c" . mingus-edit-id3tag--finish-edit)
                     ("C-c C-k" . mingus-edit-id3tag--abort-edit))
          (let* ((info (concat
                        "File:   " file "\n"
                        "Artist: " artist "\n"
                        "Album:  " album "\n"
                        "Title:  " title "\n"
                        "Track:  " track "\n"
                        "Year:   " date "\n"
                        "Genre:  " genre)))
            (with-current-buffer buffer
              (setq buffer-read-only nil)
              (buffer-disable-undo)
              (add-text-properties (point-min) (point-max) '(read-only nil))
              (kill-region (point-min) (point-max))
              (setq major-mode 'text-mode)
              (setq mode-name "Edit ID3 Tags")
              (use-local-map keymap)
              (setq header-line-format
                    (substitute-command-keys
                     (concat
                      "Edit id3 tags, then exit with `\\[mingus-edit-id3tag--finish-edit]'"
                      " or abort with `\\[mingus-edit-id3tag--abort-edit]'")))
              (insert info)
              (goto-char (point-min))
              (while (not (eobp))
                (add-text-properties
                 (point) (+ (point) 7)
                 '(face bold read-only t cursor-intangible t))
                (forward-line 1))
              (cursor-intangible-mode 1)
              (set-buffer-modified-p nil)
              (goto-char (point-min)))
            (switch-to-buffer buffer))))))

  ;;-----------------------------------------------------------------------
  ;;;; Mingus Fetch Lyrics Commands
  ;;-----------------------------------------------------------------------

  (init-message 3 "Mingus Fetch Lyrics Commands")

  ;; ;; TODO: add ability to query multiple lyric sites when a result is not found on one
  ;; (defun mingus-get-lyrics ()
  ;;   "Get lyrics (original non-api version)."
  ;;   (interactive)
  ;;   (let ((details (mingus-get-details)))
  ;;     (when details
  ;;       (let ((artist (plist-get details 'Artist))
  ;;             (title (plist-get details 'Title))
  ;;             (site "azlyrics.com")
  ;;             ;;(site "allthelyrics.com")
  ;;             ;;(site "lyricwiki.org")
  ;;             ;;(site "lyrics.wikia.com")
  ;;             (user-agent "Mozilla/4.0 (MSIE 6.0; Windows NT 5.0)")
  ;;             (referer "http://www.google.com/")
  ;;             (file (shell-command-to-string "echo -n /tmp/mingus-lyrics-$$.html")))
  ;;         ;; remove ", The" from artist
  ;;         (setq artist (replace-regexp-in-string artist ", The$" ""))
  ;;         ;; remove anything in parenthesis from title
  ;;         (setq title (replace-regexp-in-string title " ?([^)]*)" ""))
  ;;         (let ((query (concat "http://www.google.com/search?q="
  ;;                              "lyrics "
  ;;                              "\"" artist "\" "
  ;;                              "\"" title "\" "
  ;;                              "site:" site
  ;;                              "&btnI=Search")))
  ;;           (call-process "wget" nil nil nil
  ;;                         "--no-verbose"
  ;;                         "--convert-links"
  ;;                         (concat "--user-agent=" user-agent)
  ;;                         (concat "--referer=" referer)
  ;;                         "-O" file
  ;;                         query)
  ;;           (browse-url (concat "file://" file))
  ;;           ;; clean up response
  ;;           (fundamental-mode)
  ;;           (setq buffer-read-only nil)
  ;;           (buffer-disable-undo)
  ;;           (when (re-search-forward "You must enable javascript to view this page." nil :noerror)
  ;;             (goto-char (line-end-position))
  ;;             (forward-line 1)
  ;;             (delete-region (point-min) (point)))
  ;;           (when (re-search-forward "External links" nil :noerror)
  ;;             (forward-line 0)
  ;;             (delete-region (point) (point-max)))
  ;;           (goto-char (point-min))
  ;;           (while (re-search-forward "phone Send" nil :noerror)
  ;;             (delete-region (line-beginning-position) (1+ (line-end-position))))
  ;;           (setq buffer-read-only nil)
  ;;           (goto-char (point-min))
  ;;           )))))

  (defun mingus-get-lyrics-azlyrics (artist title)
    "Return the lyrics for a song matching ARTIST and TITLE
by scraping the azlyrics.com site."
    (interactive)
    (message "artist: %s, title: %s" artist title)
    (let* ((start-regexp "<!-- Usage of azlyrics.com")
           (end-regexp "<!-- MxM banner -->")
           (ret-regexp "")
           (bracket-regexp " *\\[[^\]].*\\]")
           (starting-spaces-regexp "^ +")
           (ending-spaces-regexp " +$")
           (feat-regexp "<span class=\"feat\">\\(.*\\)</span>")
           (site "azlyrics.com")
           ;;(user-agent "Mozilla/4.0 (MSIE 6.0; Windows NT 5.0)")
           (user-agent "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; WOW64; Trident/4.0; SLCC1)")
           (referer "http://www.google.com/")
           (search-file (shell-command-to-string "echo -n /tmp/mingus-search-$$.html"))
           (lyrics-file (shell-command-to-string "echo -n /tmp/mingus-lyrics-$$.html"))
           (parsed-title
            (replace-regexp-in-string ending-spaces-regexp ""
                                      (replace-regexp-in-string starting-spaces-regexp ""
                                                                (replace-regexp-in-string bracket-regexp "" title))))
           (query (concat "http://www.google.com/search?q="
                          "lyrics "
                          (if artist (concat "\"" artist "\" ") "")
                          "\"" parsed-title "\" "
                          "site:" site
                          "&btnI=Search"))
           url
           lyrics
           error)
      ;; TODO: make this call asynchronous (using `start-process')
      (call-process "wget" nil nil nil
                    "--no-verbose"
                    "--convert-links"
                    (concat "--user-agent=" user-agent)
                    (concat "--referer=" referer)
                    "-O" search-file
                    query)
      (message "query: %s" query)
      (message "wget call: wget --no-verbose --convert-links --user-agent=\"%s\" --referer=%s -O %s %s" user-agent referer search-file query)
      (with-temp-buffer
        (buffer-disable-undo)
        (condition-case err
            (progn
              (insert-file-contents search-file)
              (goto-char (point-min))
              (re-search-forward (concat "https://www." site "[^\"]*"))
              (setq url (match-string-no-properties 0)))
          ('error
           (message "Error trying to find lyrics url: %s" err)
           (setq error t))))
      (if (and error artist)
          ;; on error, try again without artist
          (mingus-get-lyrics-azlyrics nil title)
        (progn
          ;; no error, so continue
          (call-process "wget" nil nil nil
                        "--no-verbose"
                        "--convert-links"
                        (concat "--user-agent=" user-agent)
                        (concat "--referer=" referer)
                        "-O" lyrics-file
                        url)
          (with-temp-buffer
            (buffer-disable-undo)
            (condition-case err
                (progn
                  (insert-file-contents lyrics-file)
                  ;; clean up response
                  (fundamental-mode)
                  (goto-char (point-min))
                  (when (re-search-forward start-regexp nil :noerror)
                    (forward-line 0)
                    (forward-line 1)
                    (let ((pos (point)))
                      (forward-line -3)
                      (delete-region (point) pos))
                    (forward-line -2)
                    (delete-region (point-min) (point)))
                  (when (re-search-forward end-regexp nil :noerror)
                    (delete-region (line-beginning-position) (point-max)))
                  (goto-char (point-min))
                  (while (re-search-forward ret-regexp nil :noerror)
                    (replace-match ""))
                  (goto-char (point-min))
                  (while (re-search-forward feat-regexp nil :noerror)
                    (replace-match "\\1"))
                  (shr-render-region (point-min) (point-max))
                  (goto-char (point-max))
                  (delete-blank-lines)
                  (goto-char (point-min))
                  (insert (upcase artist) ": " (upcase title))
                  (newline)
                  (newline)
                  (setq lyrics (buffer-substring-no-properties (point-min) (point-max))))
              ('error
               (message "Error trying to format lyrics result: %s" err)))
            lyrics)))))

  ;; this site seems to be no longer working (2010-04-06)
  (defun mingus-get-lyrics-leoslyrics (artist title)
    "Return the lyrics for a song matching ARTIST and TITLE
using the api.leoslyrics.com site."
    (interactive)
    (let ((query (concat "wget -q \"http://api.leoslyrics.com/api_search.php?auth=emacs"
                         (if artist (concat "&artist=" (url-hexify-string artist)) "")
                         "&songtitle=" (url-hexify-string title)
                         "\" -O - | xmlstarlet sel -t -v \"/leoslyrics/searchResults/result/@hid\"")))
      ;;(message "hid query: %s" query)
      (let ((hid (shell-command-to-string query)))
        (when hid
          ;;(message "hid: %s" (url-hexify-string hid))
          (let ((query (concat "wget -q \"http://api.leoslyrics.com/api_lyrics.php?auth=emacs&hid="
                               (url-hexify-string hid)
                               "\" -O - | xmlstarlet sel -t -v \"/leoslyrics/lyric/text/text()\""
                               " | xmlstarlet unesc | tr -d '\r'")))
            ;;(message "lyrics query: %s" query)
            (let ((lyrics (shell-command-to-string query)))
              lyrics))))))

  (defun mingus-get-lyrics-metrolyrics (artist title)
    "Return the lyrics for a song matching ARTIST and TITLE
by scraping the metrolyrics.com site."
    (interactive)
    (let* ((start-regexp "<div id=\"lyrics-body-text\">")
           (end-regexp "</div>")
           (ret-regexp "")
           (bracket-regexp " *\\[[^\]].*\\]")
           (starting-spaces-regexp "^ +")
           (ending-spaces-regexp " +$")
           (site "metrolyrics.com")
           ;;(user-agent "Mozilla/4.0 (MSIE 6.0; Windows NT 5.0)")
           (user-agent "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; WOW64; Trident/4.0; SLCC1)")
           (referer "http://www.google.com/")
           (file (shell-command-to-string "echo -n /tmp/mingus-lyrics-$$.html"))
           (parsed-title
            (replace-regexp-in-string ending-spaces-regexp ""
                                      (replace-regexp-in-string starting-spaces-regexp ""
                                                                (replace-regexp-in-string bracket-regexp "" title))))
           (query (concat "http://www.google.com/search?q="
                          "lyrics "
                          (if artist "\"" artist "\" " "")
                          "\"" parsed-title "\" "
                          "site:" site
                          "&btnI=Search"))
           (lyrics))
      ;; TODO: make this call asynchronous (using `start-process')
      (call-process "wget" nil nil nil
                    "--no-verbose"
                    "--convert-links"
                    (concat "--user-agent=" user-agent)
                    (concat "--referer=" referer)
                    "-O" file
                    query)
      (message "query: %s" query)
      (message "wget call: wget --no-verbose --convert-links --user-agent=\"%s\" --referer=%s -O %s %s" user-agent referer file query)
      (with-temp-buffer
        (buffer-disable-undo)
        (condition-case err
            (progn
              (insert-file-contents file)
              ;; clean up response
              (fundamental-mode)
              (goto-char (point-min))
              (when (re-search-forward start-regexp nil :noerror)
                (forward-line 0)
                (forward-line 1)
                (let ((pos (point)))
                  (forward-line -3)
                  (delete-region (point) pos))
                (forward-line -2)
                (delete-region (point-min) (point)))
              (when (re-search-forward end-regexp nil :noerror)
                (delete-region (line-beginning-position) (point-max)))
              (goto-char (point-min))
              (while (re-search-forward ret-regexp nil :noerror)
                (replace-match ""))
              (shr-render-region (point-min) (point-max))
              (goto-char (point-max))
              (delete-blank-lines)
              (goto-char (point-min))
              (insert (upcase artist) ": " (upcase title))
              (newline)
              (newline)
              (setq lyrics (buffer-substring-no-properties (point-min) (point-max))))
          ('error
           (message "Error trying to format lyrics result: %s" err)))
        lyrics)))

  ;; TODO: add ability to query multiple lyric sites when a result is not found on one
  (defun mingus-get-lyrics ()
    "Display lyrics for the selected song."
    (interactive)
    (let ((details (mingus-get-details)))
      (when details
        (let ((artist (plist-get details 'Artist))
              (title (plist-get details 'Title))
              (funct 'mingus-get-lyrics-azlyrics))
          ;;(funct 'mingus-get-lyrics-leoslyrics))
          ;;(funct 'mingus-get-lyrics-metrolyrics))
          ;; remove ", The" from artist
          (setq artist (replace-regexp-in-string ", The$" "" artist))
          ;; remove anything in parenthesis from title
          (setq title (replace-regexp-in-string " ?([^)]*)" "" title))
          ;; call lyrics api
          (let ((buffer (concat "*" artist ": " title "*"))
                (lyrics (funcall funct artist title)))
            ;; if no lyrics returned, try again with song title only (helps with covers)
            (when (or (not lyrics)
                      (zerop (length lyrics)))
              (setq lyrics (funcall funct nil title)))
            (if (and lyrics
                     (> (length lyrics) 0))
                (progn
                  (get-buffer-create buffer)
                  (set-buffer buffer)
                  (let ((buffer-read-only nil))
                    (erase-buffer)
                    (insert lyrics)
                    (delete-trailing-whitespace)
                    (switch-to-buffer buffer)
                    (goto-char (point-min))
                    (view-mode)))
              (message "No lyrics found for Artist: %s, Title: %s" artist title)))))))

  ;;-----------------------------------------------------------------------
  ;;;; Mingus Song Rating System
  ;;-----------------------------------------------------------------------

  (init-message 3 "Mingus Song Rating System")

  ;; directory to store rating play lists
  (defcustom mingus-ratings-directory `,(file-truename (expand-file-name "~/.song-ratings"))
    "Directory to store rating play lists."
    :type 'string
    :group 'mingus)

  ;; rating play list prefix
  (defcustom mingus-ratings-prefix "songs-rated-"
    "Ratings play list files prefix."
    :type 'string
    :group 'mingus)

  (defun mingus-get-song-rating ()
    "Return song rating of the selected song."
    (interactive)
    (let ((details (mingus-get-details)))
      (when details
        (let* ((file (plist-get details 'file))
               (song-name (propertize
                           (replace-regexp-in-string "^.*/\\([^/]+\\)\\..*$" "\\1" file)
                           'face
                           'font-lock-keyword-face))
               (get-cmd (concat
                         "cd " mingus-ratings-directory " && "
                         "(for x in $(ls " mingus-ratings-prefix "*) ; do grep -q -F \""
                         (replace-regexp-in-string "\"" "\"" file)
                         "\" \"${x}\" && echo ${x: -1}; done)")))
          ;;(message "get-cmd: %s" get-cmd)
          ;; make sure ratings directory exists
          (unless (file-exists-p mingus-ratings-directory)
            (make-directory mingus-ratings-directory))
          ;; get rating, if there is one
          (let ((rating (shell-command-to-string get-cmd)))
            (when (string-match "\r?\n$" rating)
              (setq rating (replace-match "" t nil rating)))
            (when (zerop (length rating))
              (setq rating "0"))
            (let ((rating-name (propertize rating 'face 'font-lock-keyword-face)))
              ;; print or return rating
              (if (called-interactively-p 'any)
                  (message "Rating for %s is %s" song-name rating-name)
                (string-to-number rating))))))))

  (defun mingus-set-song-rating (rating)
    "Set song rating of the selected song.

RATING may be a number from 0 to 5, where 1 is least favorite and
5 is most favorite. 0 will unset the rating."
    (interactive)
    (unless (and (>= rating 0) (<= rating 5))
      (user-error "Rating must be a number from 0 through 5"))
    (let ((details (mingus-get-details)))
      (when details
        (let* ((file (plist-get details 'file))
               (song-name (propertize
                           (replace-regexp-in-string "^.*/\\([^/]+\\)\\..*$" "\\1" file)
                           'face
                           'font-lock-keyword-face))
               (rating-name (propertize (number-to-string rating) 'face 'font-lock-keyword-face))
               (playlist (concat mingus-ratings-prefix (number-to-string rating)))
               (clear-cmd (concat
                           "cd " mingus-ratings-directory " && "
                           "(for x in $(find . -name '" mingus-ratings-prefix "*') ; do grep -v -F \""
                           (replace-regexp-in-string "\"" "\"" file)
                           "\" \"${x}\" > tmp ; mv tmp \"${x}\" ; done)"))
               (set-cmd (concat
                         "cd " mingus-ratings-directory " && "
                         "(echo \""
                         (replace-regexp-in-string "\"" "\"" file)
                         "\" >> " playlist ") && "
                         "(cat " playlist " | sort > tmp ; mv tmp " playlist ")")))
          ;;(message "clear-cmd: %s" clear-cmd)
          ;;(message "set-cmd: %s" set-cmd)
          ;; make sure ratings directory exists
          (unless (file-exists-p mingus-ratings-directory)
            (make-directory mingus-ratings-directory))
          ;; clear song from any play lists
          (shell-command clear-cmd)
          ;; if rating > 0, add song to corresponding ratings play list
          (if (> rating 0)
              ;; set rating
              (if (> (shell-command set-cmd) 0)
                  (user-error "Could not set rating for \"%s\"" song-name)
                (message "\"%s\" rating was set to %s" song-name rating-name))
            ;; rating was cleared
            (message "%s rating was cleared" song-name))
          (let (buffer-read-only)
            (buffer-disable-undo)
            (mingus-display-song-rating nil))))))

  (defun mingus-set-song-rating-1 ()
    "Call `mingus-set-song-rating' with a rating of 1."
    (interactive)
    (mingus-set-song-rating 1))

  (defun mingus-set-song-rating-2 ()
    "Call `mingus-set-song-rating' with a rating of 2."
    (interactive)
    (mingus-set-song-rating 2))

  (defun mingus-set-song-rating-3 ()
    "Call `mingus-set-song-rating' with a rating of 3."
    (interactive)
    (mingus-set-song-rating 3))

  (defun mingus-set-song-rating-4 ()
    "Call `mingus-set-song-rating' with a rating of 4."
    (interactive)
    (mingus-set-song-rating 4))

  (defun mingus-set-song-rating-5 ()
    "Call `mingus-set-song-rating' with a rating of 5."
    (interactive)
    (mingus-set-song-rating 5))

  (defun mingus-set-song-rating-0 ()
    "Call `mingus-set-song-rating' with a rating of 0."
    (interactive)
    (mingus-set-song-rating 0)))
;; mingus:1 ends here

;; [[file:init-emacs.org::#modules-minions][minions:1]]
;;------------------------------------------------------------------------------
;;; Packages: minions
;;------------------------------------------------------------------------------

(init-message 2 "Modules: minions")

(use-package minions
  :straight t
  :config
  (minions-mode 1))
;; minions:1 ends here

;; [[file:init-emacs.org::#modules-multiple-cursors][multiple-cursors:1]]
;;------------------------------------------------------------------------------
;;; Packages: multiple-cursors
;;------------------------------------------------------------------------------

(init-message 2 "Modules: multiple-cursors")

(use-package multiple-cursors
  :straight t
  :bind* (("C-c C-." . mc/edit-lines)
          ("C-c C-," . mc/mark-all-like-this)
          ("C-M-." . mc/mark-next-like-this)     ; default: `xref-find-apropos'
          ("C-M-," . mc/mark-previous-like-this) ; default: `xref-go-forward'
          ("C-M->" . mc/unmark-next-like-this)
          ("C-M-<" . mc/unmark-previous-like-this)))
;; multiple-cursors:1 ends here

;; [[file:init-emacs.org::#packages-mwim][mwim:1]]
;;------------------------------------------------------------------------------
;;; Packages: mwim
;;------------------------------------------------------------------------------

(init-message 2 "Packages: mwim")

(use-package mwim
  :straight t
  :bind* (([remap move-beginning-of-line] . mwim-beginning-of-line-or-code)
          ([remap move-end-of-line] . mwim-end-of-line-or-code)
          ("<C-tab>" . mwim)))
;; mwim:1 ends here

;; [[file:init-emacs.org::#modules-neotree][neotree:1]]
;;------------------------------------------------------------------------------
;;; Packages: neotree
;;------------------------------------------------------------------------------

(init-message 2 "Modules: neotree")

(use-package neotree
  :straight t
  :commands (neo-global--select-window
             neo-global--window-exists-p
             neotree-toggle)
  :bind ("<f8>" . neotree-select-or-toggle)
  :custom
  ;; open tree at current file node
  (neo-smart-open t)
  ;; work with projectile
  (projectile-switch-project-action #'neotree-projectile-action)
  :config
  (defun neotree-select-or-toggle ()
    "Make an existing neotree window active or toggle it on/off."
    (interactive)
    (if (and (neo-global--window-exists-p)
             (not (eq (current-buffer) (get-buffer neo-buffer-name))))
        (neo-global--select-window)
      (neotree-toggle))))
;; neotree:1 ends here

;; [[file:init-emacs.org::#packages-nov][nov:1]]
;;------------------------------------------------------------------------------
;;; Packages: nov
;;------------------------------------------------------------------------------

(init-message 2 "Packages: nov")

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))

;; ;;------------------------------------------------------------------------------
;; ;;;; Packages: nov-xwidget
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "Packages: nov-xwidget")

;; (use-package nov-xwidget
;;   :straight t
;;   :after (nov)
;;   :config
;;   ;; (map! :map nov-mode-map
;;   ;;       :n "gv" 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook #'nov-xwidget-inject-all-files))
;; nov:1 ends here

;; [[file:init-emacs.org::#modules-occur][occur:1]]
;;------------------------------------------------------------------------------
;;; Packages: occur
;;------------------------------------------------------------------------------

(init-message 2 "Modules: occur")

(use-package replace
  :straight (:type built-in)
  :config
  (when (fboundp 'occur-inverse)
      (bind-keys* :map occur-mode-map ("C-c C-i" . occur-inverse))))
;; occur:1 ends here

;; [[file:init-emacs.org::#packages-org-tree-slide][org-tree-slide:1]]
;;------------------------------------------------------------------------------
;;; Packages: org-tree-slide
;;------------------------------------------------------------------------------

(init-message 2 "Packages: org-tree-slide")

(use-package org-tree-slide
  :straight t
  :bind (("S-<f8>" . org-tree-slide-mode)
         ("C-<f8>" . org-tree-slide-skip-done-toggle))
  :bind (:map org-tree-slide-mode-map
              ("<f11>" . org-tree-slide-move-previous-tree)
              ("<f12>" . org-tree-slide-move-next-tree))
  :hook ((org-tree-slide-play . org-tree-slide-presentation-setup)
         (org-tree-slide-stop . org-tree-slide-presentation-reset))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation Mode: On")
  (org-tree-slide-deactivate-message "Presentation Mode: Off")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil)
  :config
  (use-package hide-mode-line
    :straight t)

  (defun org-tree-slide-presentation-setup ()
    (setq text-scale-mode-amount 3)
    (when (fboundp 'hide-mode-line-mode)
      (hide-mode-line-mode 1))
    (org-display-inline-images)
    (text-scale-mode 1))

  (defun org-tree-slide-presentation-reset ()
    (when (fboundp 'hide-mode-line-mode)
      (hide-mode-line-mode 0))
    (text-scale-mode 0)))
;; org-tree-slide:1 ends here

;; [[file:init-emacs.org::#packages-olivetti][olivetti:1]]
;;------------------------------------------------------------------------------
;;; Packages: olivetti
;;------------------------------------------------------------------------------

(init-message 2 "Packages: olivetti")

(use-package olivetti
  :straight t)
;; olivetti:1 ends here

;; [[file:init-emacs.org::#modules-package-lint][package-lint:1]]
;;------------------------------------------------------------------------------
;;; Packages: package-lint
;;------------------------------------------------------------------------------

(init-message 2 "Modules: package-lint")

(use-package package-lint
  :straight t)
;; package-lint:1 ends here

;; [[file:init-emacs.org::#modules-persistent-scratch][persistent-scratch:1]]
;;------------------------------------------------------------------------------
;;; Packages: persistent-scratch
;;------------------------------------------------------------------------------

(init-message 2 "Modules: persistent-scratch")

(use-package persistent-scratch
  :straight t
  :demand t
  :init
  ;; enable autosave and restore last saved state
  (persistent-scratch-setup-default))
;; persistent-scratch:1 ends here

;; [[file:init-emacs.org::#packages-pocket-reader][pocket-reader:1]]
;;------------------------------------------------------------------------------
;;; Packages: pocket-reader
;;------------------------------------------------------------------------------

(init-message 2 "Packages: pocket-reader")

(use-package pocket-reader
  :straight t
  :custom
  (pocket-reader-archive-on-open nil))
;; pocket-reader:1 ends here

;; [[file:init-emacs.org::#modules-proced][proced:1]]
;;------------------------------------------------------------------------------
;;; Packages: proced
;;------------------------------------------------------------------------------

(init-message 2 "Modules: proced")

(use-package proced
  :straight t
  :commands (proced))
;; proced:1 ends here

;; [[file:init-emacs.org::#modules-projectile][projectile:1]]
;;------------------------------------------------------------------------------
;;; projectile (project interaction library)
;;
;; Install `ack-grep' and `exuberant-ctags' system packages for more
;; projectile functionality.
;; ------------------------------------------------------------------------------

(init-message 2 "Modules: projectile")

(use-package projectile
  :straight t
  ;; :after (ivy)
  :diminish (projectile-mode . "Proj")
  :bind* ("C-x p" . projectile-command-map)
  ;;:bind-keymap ("C-c p" . projectile-command-map)
  :custom
  ;; open the root directory when switching projects
  (projectile-switch-project-action #'projectile-dired)
  ;; ;; use ivy
  ;; (projectile-completion-system 'ivy)
  :init
  ;; enable projectile globally
  (projectile-mode)
  :config
  (dolist (x '("~/dev" "~/code" "~/web"))
    (when (file-directory-p x)
      (add-to-list 'projectile-project-search-path x))))

;; ;;------------------------------------------------------------------------------
;; ;;;; counsel-projectile
;; ;;
;; ;; Ivy integration for Projectile.
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "counsel-projectile")

;; (use-package counsel-projectile
;;   :straight t
;;   :after (ivy counsel projectile)
;;   :after (counsel projectile)
;;   :init (counsel-projectile-mode))
;; projectile:1 ends here

;; [[file:init-emacs.org::#modules-rainbow-mode][rainbow-mode:1]]
;;------------------------------------------------------------------------------
;;; Packages: rainbow-mode
;;------------------------------------------------------------------------------

(init-message 2 "Modules: rainbow-mode")

(use-package rainbow-mode
  :straight t
  :init
  (rainbow-mode 1))
;; rainbow-mode:1 ends here

;; [[file:init-emacs.org::#modules-recentf][recentf:1]]
;;------------------------------------------------------------------------------
;;; Packages: recentf
;;------------------------------------------------------------------------------

(init-message 2 "Modules: recentf")

(use-package recentf
  :straight (:type built-in)
  :commands (recentf-mode)
  :custom
  (recentf-max-menu-items 25)
  :init
  (recentf-mode 1))
;; recentf:1 ends here

;; [[file:init-emacs.org::#modules-regex-tool][regex-tool:1]]
;;------------------------------------------------------------------------------
;;; Packages: regex-tool
;;------------------------------------------------------------------------------

(init-message 2 "Modules: regex-tool")

(use-package regex-tool
  :straight t
  :commands (regex-tool))
;; regex-tool:1 ends here

;; [[file:init-emacs.org::#modules-replacer][replacer:1]]
;;------------------------------------------------------------------------------
;;; Packages: replacer
;;------------------------------------------------------------------------------

(init-message 2 "Modules: replacer")

(use-package replacer
  :load-path (lambda () (file-truename (expand-file-name "replacer.el" local-modules-dir)))
  :after (company)
  :commands (replacer-mode company-replacer-backend)
  :custom
  ;; set trigger start
  (replacer-trigger-start ";")
  ;; set trigger end
  (replacer-trigger-end " ")
  ;; set trigger keys and replacements
  (replacer-replacements
   '(
     ;; help
     ("h" . replacer-help)
     ("help" . replacer-help)
     ;; insert
     ("id" . insert-date)
     ("idt" . insert-datetime)
     ("it" . insert-time)
     ("iuuid" . insert-uuid)
     ("iguid" . insert-guid)
     ("ipw" . insert-password-20)
     ("ipp" . insert-password-phrase-three-hyphen)
     ("i=" . append-equal-to-column-80)
     ("i-" . append-dash-to-column-80)
     ("i*" . append-asterisk-to-column-80)
     ("ilgpl" . insert-license-gpl)
     ("ilmit" . insert-license-mit)
     ("ilapache" . insert-license-apache)
     ;; org-mode
     ("on" . org-insert-literate-programming-name)
     ("os" . org-insert-literate-programming-src)
     ("ossh" . org-insert-literate-programming-src-sh)
     ("ossu" . org-insert-literate-programming-src-sh-sudo)
     ("osel" . org-insert-literate-programming-src-emacs-lisp)
     ("osr" . org-insert-literate-programming-src-racket)
     ("osk" . org-insert-literate-programming-src-kotlin)
     ("ospy" . org-insert-literate-programming-src-python)
     ("obie" . org-insert-literate-programming-init-emacs-block)
     ("obc" . org-insert-literate-programming-code-block)
     ("obpe" . org-insert-literate-programming-project-euler-problem-block)
     ;; ;; ironsworn
     ;; ("isfd" . "Face Danger")
     ;; ("issa" . "Secure an Advantage")
     ;; ("isgi" . "Gather Information")
     ;; ("ish" . "Heal")
     ;; ("isr" . "Resupply")
     ;; ("ismc" . "Make Camp")
     ;; ("isuj" . "Undertake a Journey")
     ;; ("isrd" . "Reach Your Destination")
     ;; ("isp" . "Compel")
     ;; ("isj" . "Sojourn")
     ;; ("isdc" . "Draw the Circle")
     ;; ("isfb" . "Forge a Bond")
     ;; ("istb" . "Test Your Bond")
     ;; ("isaa" . "Aid Your Ally")
     ;; ("iswe" . "Write Your Epilogue")
     ;; ("isef" . "Enter the Fray")
     ;; ("iss" . "Strike")
     ;; ("isc" . "Clash")
     ;; ("istt" . "Turn the Tide")
     ;; ("isef" . "End the Fight")
     ;; ("isb" . "Battle")
     ;; ("iseh" . "Endure Harm")
     ;; ("isfd" . "Face Death")
     ;; ("isceh" . "Companion Endure Harm")
     ;; ("ises" . "Endure Stress")
     ;; ("" . "")
     ))
  :init
  ;; turn replacer mode on
  (replacer-mode 1)
  :config
  (defun replacer-replacements-edit ()
    "Open `init-emacs.org' and move point to `replacer-replacements' variable for easy editing."
    (interactive)
    (find-file (expand-file-name "init-emacs.org" emacs-home-dir))
    (goto-char (point-min))
    (search-forward "replacer-replacements")
    (org-show-entry))

  (defun replacer-help ()
    "Show list of `replacer-replacements'."
    (describe-variable 'replacer-replacements)))
;; replacer:1 ends here

;; [[file:init-emacs.org::#modules-s][s:1]]
;;------------------------------------------------------------------------------
;;; Packages: s
;;------------------------------------------------------------------------------

(init-message 2 "Modules: s")

(use-package s
  :straight t)
;; s:1 ends here

;; [[file:init-emacs.org::#modules-saveplace][saveplace:1]]
;;------------------------------------------------------------------------------
;;; Packages: saveplace
;;------------------------------------------------------------------------------

(init-message 2 "Modules: saveplace")

(use-package saveplace
  :straight (:type built-in)
  :after (org-visibility)             ; restore visibility before moving point
  :custom
  (save-place-file (locate-user-emacs-file ".saveplace"))
  (save-place-limit 100)
  (save-place-ignore-files-regexp
   (rx (or
        ;; ignore org files so as to not conflict with `org-visibility'
        (seq ".org" eol)
        ;; ignore ~/.cddb files
        "/.cddb/")))
  :init (save-place-mode 1)
  :config
  (defun save-place-find-file-hook--ignore-hidden-point ()
    "Only restore position if point is visible."
    (when (invisible-p (point))
      (beginning-of-buffer)))
  ;; advise `save-place-find-file-hook'
  (advice-add 'save-place-find-file-hook :after #'save-place-find-file-hook--ignore-hidden-point))
;; saveplace:1 ends here

;; [[file:init-emacs.org::#modules-smerge][smerge:1]]
;;------------------------------------------------------------------------------
;;; Packages: smerge
;;------------------------------------------------------------------------------

(init-message 2 "Modules: smerge")

(use-package smerge-mode
  :straight (:type built-in)
  :init
  (defun smerge-mode-maybe ()
    "Auto turn on smerge mode when a file with merge conflicts is loaded.
Do not perform the search on very large files (to avoid a delay when loaded)."
    (when (<= (buffer-size) 10000)
      (save-mark-and-excursion
        (save-match-data
          (goto-char (point-min))
          (when (re-search-forward "^<<<<<<< " nil :noerror)
            (smerge-mode 1))))))
  (add-hook 'find-file-hook #'smerge-mode-maybe :append))
;; smerge:1 ends here

;; [[file:init-emacs.org::#modules-sokoban][sokoban:1]]
;;------------------------------------------------------------------------------
;;; Packages: sokoban
;;------------------------------------------------------------------------------

(init-message 2 "Modules: sokoban")

(use-package sokoban
  :load-path (lambda () (file-truename (expand-file-name "sokoban/sokoban.el" emacs-modules-dir)))
  :commands (sokoban sokoban-mode)
  :custom
  (sokoban-levels-dir (file-truename (expand-file-name "sokoban/sokoban-levels" emacs-modules-dir))))
;; sokoban:1 ends here

;; [[file:init-emacs.org::#modules-split-move][split-move:1]]
;;------------------------------------------------------------------------------
;;; Packages: split-move
;;------------------------------------------------------------------------------

(init-message 2 "Modules: split-move")

(use-package split-move
  :load-path (lambda () (file-truename (expand-file-name "split-move.el" local-modules-dir)))
  :commands (split-move-up split-move-down))
;; split-move:1 ends here

;; [[file:init-emacs.org::#modules-spinner][spinner:1]]
;;------------------------------------------------------------------------------
;;; Packages: spinner
;;------------------------------------------------------------------------------

(init-message 2 "Modules: spinner")

(use-package spinner
  :straight (spinner :type git :host github :repo "Malabarba/spinner.el"))
;; spinner:1 ends here

;; [[file:init-emacs.org::#modules-sudoku][sudoku:1]]
;;------------------------------------------------------------------------------
;;; Packages: sudoku
;;------------------------------------------------------------------------------

(init-message 2 "Modules: sudoku")

(use-package sudoku
  :straight (sudoku :type git :host github :repo "zevlg/sudoku.el")
  :commands (sudoku))
;; sudoku:1 ends here

;; [[file:init-emacs.org::#packages-svg-2048][svg-2048:1]]
;;------------------------------------------------------------------------------
;;; Packages: svg-2048
;;------------------------------------------------------------------------------

(init-message 2 "Packages: svg-2048")

(use-package svg-2048
  :straight (:type git :host github :repo "wasamasa/svg-2048")
  :bind (:map svg-2048-mode-map
              ("<left>" . svg-2048-move-left)
              ("<right>" . svg-2048-move-right)
              ("<up>" . svg-2048-move-up)
              ("<down>" . svg-2048-move-down)))
;; svg-2048:1 ends here

;; [[file:init-emacs.org::#packages-svg-clock][svg-clock:1]]
;;------------------------------------------------------------------------------
;;; Packages: svg-clock
;;------------------------------------------------------------------------------

(init-message 2 "Packages: svg-clock")

(use-package svg-clock
  :straight t)
;; svg-clock:1 ends here

;; [[file:init-emacs.org::#modules-switch-window][switch-window:1]]
;;------------------------------------------------------------------------------
;;; Packages: switch-window
;;------------------------------------------------------------------------------

(init-message 2 "Modules: switch-window")

(use-package switch-window
  :straight t
  :demand t
  :commands (switch-window switch-window-then-delete)
  :custom
  ;; use home keys to select windows
  (switch-window-shortcut-style 'qwerty))
;; switch-window:1 ends here

;; [[file:init-emacs.org::#modules-telnet][telnet:1]]
;;------------------------------------------------------------------------------
;;; Packages: telnet
;;------------------------------------------------------------------------------

(init-message 2 "Modules: telnet")

(use-package telnet
  :straight (:type built-in))
;; telnet:1 ends here

;; [[file:init-emacs.org::#modules-timeclock][timeclock:1]]
;;------------------------------------------------------------------------------
;;; Packages: timeclock
;;------------------------------------------------------------------------------

(init-message 2 "Modules: timeclock")

(use-package timeclock
  :straight (:type built-in)
  :bind (("C-c ti" . timeclock-in)
         ("C-c to" . timeclock-out)
         ("C-c tc" . timeclock-change)
         ("C-c tr" . timeclock-reread-log)
         ("C-c tu" . timeclock-update-mode-line)
         ("C-c tw" . timeclock-when-to-leave-string)
         ("C-c tv" . timeclock-visit-timelog)
         ("C-c ts" . timeclock-status-string)
         ("C-c td" . timeclock-mode-line-display)
         ("C-c tg" . timeclock-generate-report))
  ;;:init (timeclock-mode-line-display)
  )
;; timeclock:1 ends here

;; [[file:init-emacs.org::#modules-time-stamp][time-stamp:1]]
;;------------------------------------------------------------------------------
;;; Packages: time-stamp
;;
;; When there is a "Timestamp: <>" in the first 20 lines of a file,
;; emacs will write the time-stamp there when saving the file.
;; Timestamp is case insensitive and a dash is optional (e.g. Time-stamp).
;;------------------------------------------------------------------------------

(init-message 2 "Modules: time-stamp")

(use-package time-stamp
  :straight (:type built-in)
  :commands (time-stamp)
  :custom
  (time-stamp-active t)
  :init
  (setq time-stamp-line-limit 50
        time-stamp-start "[Tt][Ii][Mm][Ee][-]?[Ss][Tt][Aa][Mm][Pp]:[ \t]+\\\\?[\"<]+"
        time-stamp-format "%Y-%02m-%02d %02H:%02M (%u)")
  (add-hook 'before-save-hook #'time-stamp))
;; time-stamp:1 ends here

;; [[file:init-emacs.org::#modules-tramp][tramp:1]]
;;------------------------------------------------------------------------------
;;; Packages: tramp
;;------------------------------------------------------------------------------

(init-message 2 "Modules: tramp")

(use-package tramp
  :straight (:type built-in)
  :commands (tramp)
  :custom
  (tramp-default-method "ssh")
  :config
  (add-to-list 'tramp-default-method-alist '("^localhost$" "^root$" "su") t))

;;------------------------------------------------------------------------------
;;; Find File as Root
;;------------------------------------------------------------------------------

(init-message 3 "Find File as Root")

(use-package tramp
  :straight (:type built-in)
  :commands (find-alternative-file-as-root
             find-file-as-root
             find-file-as-root-or-find-alternative-file-as-root
             tramp
             tramp-dissect-file-name
             tramp-file-name-localname
             tramp-tramp-file-p)
  :bind ("C-c M-r" . find-alternative-file-as-root)
  :config
  (defvar find-file-as-root-prefix "/sudo:root@localhost:"
    "*The file name prefix used to open a file with `find-file-as-root'.")

  (defvar find-file-as-root-history nil
    "History list for files found using `find-file-as-root'.")

  (defvar find-file-as-root-hook nil
    "Normal hook for functions to run after finding files with `find-file-as-root'.")

  (defun find-file-as-root ()
    "*Open a file as the root user.

Prepends `find-file-as-root-prefix' to the selected file name so
that it maybe accessed via the corresponding tramp method."
    (interactive)
    (require 'tramp)
    ;; bind the variable `file-name-history' locally so there is a separate
    ;; history list for files found using `find-file-as-root'
    (let* ((file-name-history find-file-as-root-history)
           (name (or buffer-file-name default-directory))
           (tramp (and (tramp-tramp-file-p name) (tramp-dissect-file-name name)))
           path dir file)
      ;; when called from a file opened with tramp, fix up the path
      (when tramp
        (setq path (tramp-file-name-localname tramp)
              dir (file-name-directory path)))
      ;; ask for file name
      (when (setq file (read-file-name "Find file as root: " dir path))
        (find-file (concat find-file-as-root-prefix file))
        ;; if this all succeeded, save our new history list
        (setq find-file-as-root-history file-name-history)
        ;; allow some user customization
        (run-hooks 'find-file-as-root-hook))))

  (defun find-alternative-file-as-root ()
    "Find alternative file as root."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))))

  (defun find-file-as-root-or-find-alternative-file-as-root ()
    "If current buffer is read-only, run `file-alternative-file-as-root',
otherwise run `find-file-as-root'."
    (interactive)
    (if buffer-read-only
        (find-alternative-file-as-root)
      (find-file-as-root))))
;; tramp:1 ends here

;; [[file:init-emacs.org::#packages-tree-sitter][tree-sitter:1]]
;;------------------------------------------------------------------------------
;;; Packages: tree-sitter
;;------------------------------------------------------------------------------

(init-message 2 "Packages: tree-sitter")

(use-package tree-sitter
  :straight t
  ;;:hook (prog-mode . tree-sitter-hl-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode 1))

;;------------------------------------------------------------------------------
;;;; tree-sitter-langs
;;
;; Grammar bundle for tree-sitter.
;;------------------------------------------------------------------------------

(init-message 3 "tree-sitter-langs")

(use-package tree-sitter-langs
  :straight t
  :after (tree-sitter))
;; tree-sitter:1 ends here

;; [[file:init-emacs.org::#modules-undo-tree][undo-tree:1]]
;;------------------------------------------------------------------------------
;;; Packages: undo-tree
;;------------------------------------------------------------------------------

(init-message 2 "Modules: undo-tree")

(use-package undo-tree
  :straight (undo-tree :type git :host github :repo "apchamberlain/undo-tree.el")
  :demand t
  :diminish undo-tree-mode
  :bind* (("<M-mouse-5>" . undo-tree-redo)
          ("<M-mouse-4>" . undo-tree-undo))
  :init
  ;; turn on undo-tree globally
  (global-undo-tree-mode 1))
;; undo-tree:1 ends here

;; [[file:init-emacs.org::#modules-vimish-fold][vimish-fold:1]]
;;------------------------------------------------------------------------------
;;; Packages: vimish-fold
;;------------------------------------------------------------------------------

(init-message 2 "Modules: vimish-fold")

(use-package vimish-fold
  :straight t)
;; vimish-fold:1 ends here

;; [[file:init-emacs.org::#modules-w3m][w3m:2]]
;;------------------------------------------------------------------------------
;;; Packages: w3m
;;------------------------------------------------------------------------------

(init-message 2 "Modules: w3m")

(use-package w3m
  :when (executable-find "w3m") ; only use if binary is available on system
  :straight t
  :commands (w3m
             w3m-antenna
             w3m-browse-url
             w3m-encode-specials-string
             w3m-find-file
             w3m-namazu
             w3m-next-buffer
             w3m-previous-buffer
             w3m-region
             w3m-search
             w3m-weather)
  :defines (w3m-use-tab-line)
  :bind (:map w3m-mode-map
              ("," . w3m-previous-buffer)
              ("." . w3m-next-buffer))
  :custom
  ;; directory of icon files
  (w3m-icon-directory "/usr/share/emacs-w3m/icon")
  ;; turn on cookies
  (w3m-use-cookies t)
  :config
  ;; add new functionality not in this version
  (defun w3m-buffer (&optional buffer)
    "Render the current buffer or BUFFER if given."
    (interactive)
    (when buffer
      (switch-to-buffer buffer))
    (w3m-region (point-min) (point-max)))

  (defun custom-w3m-display-hook (url)
    "Hook to auto-rename buffers to page title or url."
    (rename-buffer
     (format "*w3m: %s*" (or w3m-current-title w3m-current-url)) t))
  (add-hook 'w3m-display-hook #'custom-w3m-display-hook))

;; ;;------------------------------------------------------------------------------
;; ;;;; Packages: w3m-session
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "w3m-session")

;; ;; persistent sessions
;; (use-package w3m-session
;;   :when (executable-find "w3m") ; only use if w3m command is available on system
;;   :straight t
;;   :after (w3m)
;;   :commands (w3m-session-load
;;              w3m-session-load-always
;;              w3m-session-save
;;              w3m-session-save-always)
;;   :bind (:map w3m-mode-map
;;               ("S" . w3m-session-save)
;;               ("L" . w3m-session-load))
;;   ;;:config
;;   ;;(setq w3m-session-file "~/.w3m-session")
;;   ;;(setq w3m-session-save-always nil)
;;   ;;(setq w3m-session-load-always nil)
;;   ;;(setq w3m-session-show-titles t)
;;   ;;(setq w3m-session-duplicate-tabs 'ask) ; 'never, 'always, 'ask
;;   )
;; w3m:2 ends here

;; [[file:init-emacs.org::#modules-web-query][web-query:1]]
;;------------------------------------------------------------------------------
;;; Packages: web-query
;;------------------------------------------------------------------------------

(init-message 2 "Modules: web-query")

(use-package web-query
  ;; :when (executable-find "w3m")
  ;; :after (w3m)
  :load-path (lambda () (file-truename (expand-file-name "web-query.el" local-modules-dir)))
  :commands (web-query
             web-query-word
             web-query-word-at-point
             web-query-symbol-by-mode
             web-query-symbol-by-mode-at-point)
  :bind (;;("S-<f5>" . web-query-word-at-point)
         ("S-<f6>" . web-query)
         ("S-<f7>" . web-query-symbol-by-mode-at-point)
         ("C-c w" . web-query)))
;; web-query:1 ends here

;; [[file:init-emacs.org::#modules-webjump][webjump:1]]
;;------------------------------------------------------------------------------
;;; Packages: webjump
;;------------------------------------------------------------------------------

(init-message 2 "Modules: webjump")

(use-package webjump
  :straight (:type built-in)
  ;;:bind* ("C-x j" . webjump)
  :config
  ;; add some sites
  (add-to-list 'webjump-sites '("Urban Dictionary" . [simple-query "www.urbandictionary.com" "http://www.urbandictionary.com/define.php?term=" ""]) t))
;; webjump:1 ends here

;; [[file:init-emacs.org::#modules-weblogger][weblogger:1]]
;;------------------------------------------------------------------------------
;;; Packages: weblogger
;;------------------------------------------------------------------------------

(init-message 2 "Modules: weblogger")

(use-package weblogger
  :straight t
  :commands (weblogger-select-configuration
             weblogger-setup-weblog
             weblogger-start-entry)
  :custom
  ;; add weblog sites
  (weblogger-config-alist
   `(("nullman" "http://www.blogger.com/api" ,user-mail-address "" "6007591")
     ("Nullman on Life" "http://www2.blogger.com/api" ,user-mail-address "" "6007591"))))
;; weblogger:1 ends here

;; [[file:init-emacs.org::#modules-wgrep][wgrep:1]]
;;------------------------------------------------------------------------------
;;; Packages: wgrep
;;------------------------------------------------------------------------------

(init-message 2 "Modules: wgrep")

(use-package wgrep
  :straight t
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode))) ; same keybinding as `wdired-mode'
;; wgrep:1 ends here

;; [[file:init-emacs.org::#modules-which-key][which-key:1]]
;;------------------------------------------------------------------------------
;;; Packages: which-key
;;------------------------------------------------------------------------------

(init-message 2 "Modules: which-key")

(use-package which-key
  :straight t
  :demand t
  :init (which-key-mode))
;; which-key:1 ends here

;; [[file:init-emacs.org::#modules-wtf][wtf:1]]
;;------------------------------------------------------------------------------
;;; Packages: wtf
;;------------------------------------------------------------------------------

(init-message 2 "Modules: wtf")

(use-package wtf
  :load-path (lambda () (file-truename (expand-file-name "wtf.el" emacs-modules-dir)))
  :commands (wtf-is wtf-get-term-at-point))
;; wtf:1 ends here

;; [[file:init-emacs.org::#modules-wttrin][wttrin:1]]
;;------------------------------------------------------------------------------
;;; Packages: wttrin
;;------------------------------------------------------------------------------

(init-message 2 "Modules: wttrin")

(use-package wttrin
  :straight t
  :custom
  ;; default cities
  (wttrin-default-cities '("Austin"
                           "London"
                           "Minneapolis"
                           "New York"
                           "San Diego"))
  ;; default language
  (wttrin-default-accept-language '("Accept-Language" . "en-US")))
;; wttrin:1 ends here

;; [[file:init-emacs.org::#lsp-mode][LSP Mode:1]]
;;==============================================================================
;;; LSP Mode
;;==============================================================================

(init-message 1 "LSP Mode")
;; LSP Mode:1 ends here

;; [[file:init-emacs.org::#lsp-mode-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; LSP Mode: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "LSP Mode: Configuration")

(defun custom-lsp-mode-hook ()
  ;; turn on breadcrumbs
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode 1))

(use-package lsp-mode
  :straight t
  ;;:after (company)
  :after (corfu)
  :commands (lsp lsp-mode lsp-defered)
  :bind (:map lsp-mode-map
              ("M-RET" . lsp-execute-code-action))
  ;;             ("TAB" . company-indent-or-complete-common)
  ;;             ("<tab>" . company-indent-or-complete-common))
  :hook ((lsp-mode . custom-lsp-mode-hook))
         ;; (lsp-mode . company-mode))
  :init
  (setq lsp-keymap-prefix "C-c C-l")    ; default: `downcase-region'
  :config
  ;; add `which-key-mode' descriptions
  (lsp-enable-which-key-integration t))

;;------------------------------------------------------------------------------
;;;; lsp-ui
;;
;; Minor mode that contains a series of useful UI integrations.
;;------------------------------------------------------------------------------

(init-message 3 "lsp-ui")

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;;(lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-code-actions t))

;; ;;------------------------------------------------------------------------------
;; ;;;; helm-lsp
;; ;;
;; ;; LSP helm integration.
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "helm-lsp")

;; (use-package helm-lsp
;;   :straight t
;;   :after (lsp-mode))

;; ;;------------------------------------------------------------------------------
;; ;;;; lsp-ivy
;; ;;
;; ;; LSP ivy integration.
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "lsp-ivy")

;; (use-package lsp-ivy
;;   :straight t
;;   :after (lsp-mode))

;;------------------------------------------------------------------------------
;;;; lsp-treemacs
;;
;; LSP treemacs integration.
;;------------------------------------------------------------------------------

;; (init-message 3 "lsp-treemacs")

;; (use-package lsp-treemacs
;;   :straight t
;;   :after (lsp-mode)
;;   :init
;;   (lsp-treemacs-sync-mode 1))
;; Configuration:1 ends here

;; [[file:init-emacs.org::#eglot][eglot:1]]
;;==============================================================================
;;; eglot
;;==============================================================================

(init-message 1 "eglot")

(use-package eglot
  :straight t)
;; eglot:1 ends here

;; [[file:init-emacs.org::#modes][Modes:1]]
;;==============================================================================
;;; Modes
;;==============================================================================

(init-message 1 "Modes")
;; Modes:1 ends here

;; [[file:init-emacs.org::#modes-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Modes: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Configuration")

;; turn off electric indent for all modes
(setq electric-indent-inhibit t)
(setq-default electric-indent-inhibit electric-indent-inhibit)

;; turn off electric mode for all cc modes
(setq c-electric-flag nil)
(setq-default c-electric-flag c-electric-flag)
;; Configuration:1 ends here

;; [[file:init-emacs.org::#modes-asm][ASM:1]]
;;------------------------------------------------------------------------------
;;; Modes: ASM
;;------------------------------------------------------------------------------

(init-message 2 "Modes: ASM")

(use-package asm-mode
  :straight (:type built-in)
  :mode ("\\.asm\\'" . asm-mode)
  :config
  (defun custom-asm-mode-hook ()
    "Customizations for asm-mode."
    ;; remove using ';' in place of M-; to insert a comment
    (local-unset-key [asm-comment-char])

    ;; ;; enable tabs
    ;; (add-hook 'asm-mode-hook #'enable-tabs-8)

    ;; ;; set tab indentation, width, and do not convert tabs to spaces
    ;; (setq indent-tabs-mode t          ; insert tab characters
    ;;       tab-width 8                 ; default tab width is four spaces
    ;;       standard-indent 8           ; default margin-changing functions indent
    ;;       tab-always-indent nil       ; tab key will insert a tab
    ;;       tab-stop-list (number-sequence 8 180 8)) ; tab stops set to every 8 spaces

    ;; ;; set tab indentation, width, and convert tabs to spaces
    ;; (setq indent-tabs-mode nil        ; do not insert tab characters
    ;;       tab-width 4                 ; default tab width is four spaces
    ;;       standard-indent 4           ; default margin-changing functions indent
    ;;       tab-always-indent complete  ; tab key will insert a tab
    ;;       tab-stop-list (number-sequence 4 180 4)) ; tab stops set to every 4 spaces

    ;; disable tabs
    ;;(disable-tabs)

    ;; (defun asm-calculate-indentation ()
    ;;   (or
    ;;    ;; Flush labels to the left margin.
    ;;    (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
    ;;    ;; Same thing for `;;;' comments.
    ;;    (and (looking-at "\\s<\\s<\\s<") 0)
    ;;    ;; Simple `;' comments go to the comment-column.
    ;;    (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
    ;;    ;; The rest goes at the first tab stop.
    ;;    (indent-next-tab-stop 0)))
    )
  (add-hook 'asm-mode-hook #'custom-asm-mode-hook))
;; ASM:1 ends here

;; [[file:init-emacs.org::#modes-brainfuck][Brainfuck:1]]
;;------------------------------------------------------------------------------
;;; Modes: Brainfuck
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Brainfuck")

(use-package brainfuck
  :load-path (lambda () (file-truename (expand-file-name "brainfuck.el" local-modules-dir)))
  :mode ("\\.bf\\'" . brainfuck-mode))
;; Brainfuck:1 ends here

;; [[file:init-emacs.org::#modes-basic][BASIC:1]]
;;------------------------------------------------------------------------------
;;; Modes: BASIC
;;------------------------------------------------------------------------------

(init-message 2 "Modes: BASIC")

(use-package basic
  :load-path (lambda () (file-truename (expand-file-name "basic.el" local-modules-dir)))
  :mode ("\\.bas\\'" . basic-mode))
;; BASIC:1 ends here

;; [[file:init-emacs.org::#modes-c-mode][C Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: C Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: C Mode")

(use-package cc-mode
  :straight (:type built-in)
  :demand t
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.ice\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.c++\\'" . c++-mode)
         ("\\.h++\\'" . c++-mode))
  :commands (c-skip-comments-and-strings)
  :config
  ;; c style
  (defvar custom-c-style
    '((c-tab-always-indent . 'complete)
      (c-basic-offset . 4)
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . ((substatement-open after)
                                 (brace-list-open)))
      (c-hanging-colons-alist . ((member-init-intro before)
                                 (inher-intro)
                                 (case-label after)
                                 (label after)
                                 (access-label after)))
      (c-cleanup-list . (scope-operator
                         empty-defun-braces
                         defun-close-semi))
      (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                          (substatement-open . 0)
                          (substatement-label . 0)
                          (label . 0)
                          (case-label . +)
                          (block-open . 0)
                          (defun-block-intro . +)
                          (statement-block-intro . +)
                          (substatement . +)
                          (knr-argdecl-intro . -)
                          (inline-open . 0)
                          (defun-block-intro . 4)))
      (c-echo-syntactic-information-p . nil)))

  (defun custom-c-mode-common-hook ()
    "Customizations for `c-mode', `c++-mode', `objc-mode', `java-mode', and `idl-mode'."
    ;; add my personal style and set it for the current buffer
    (c-add-style "local" custom-c-style t)
    ;;(c-set-style 'stroustrup)

    ;; electric indention turned on
    (when (fboundp 'c-toggle-electric-state)
      (c-toggle-electric-state 1))

    ;; auto-newline and hungry-delete turned off
    (when (fboundp 'c-toggle-auto-hungry-state)
      (c-toggle-auto-hungry-state -1))

    ;; disable tabs
    (disable-tabs)

    ;; ;; key bindings for all supported languages
    ;; ;; can put these in c-mode-base-map because c-mode-map, c++-mode-map,
    ;; ;; objc-mode-map, java-mode-map, and idl-mode-map inherit from it
    ;; (define-key c-mode-base-map (kbd "C-<return>") 'newline-and-indent)
    ;; (define-key c-mode-base-map (kbd "C-c c") 'mode-compile)
    ;; (define-key c-mode-base-map (kbd "C-c k") 'mode-compile-kill)
    ;; (define-key c-mode-base-map (kbd "<f7>") 'mode-compile)

    ;; ;; hide/show mode and keys
    ;; (define-key c-mode-base-map (kbd "C-c <right>") 'hs-show-block)
    ;; (define-key c-mode-base-map (kbd "C-c <left>") 'hs-hide-block)
    ;; (define-key c-mode-base-map (kbd "C-c <up>") 'hs-hide-all)
    ;; (define-key c-mode-base-map (kbd "C-c <down>") 'hs-show-all)
    ;; (hs-minor-mode 1)

    ;; ;; toggle between header files and code files
    ;; (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)

    ;; set default fill column for auto-fill mode and fill-paragraph
    (setq fill-column custom-fill-column)

    ;; turn on auto-fill
    ;;(turn-on-auto-fill)

    ;; ;; turn off auto newlines
    ;; (setq c-auto-newline nil)

    ;; turn on flyspell
    (when (boundp 'flyspell-prog-mode)
      (flyspell-prog-mode))

    ;; initialize eldoc
    (when (boundp 'eldoc-mode)
      (eldoc-mode 1))

    ;; compilation settings
    (setq compile-command "make -k"
          compilation-window-height 10
          compilation-ask-about-save nil
          ;;compilation-scroll-output t
          )

    ;; (defun compile-internal--scroll ()
    ;;   "Forces compile buffer to scroll."
    ;;   (let* ((ob (current-buffer))
    ;;          (obw (get-buffer-window ob t))
    ;;          win)
    ;;     (save-mark-and-excursion
    ;;       (unless (and (setq win (get-buffer-window ad-return-value t))
    ;;                    obw)
    ;;         (select-window win)
    ;;         (goto-char (point-max))
    ;;         (select-window obw)))))
    ;; ;; advise `compile-internal'
    ;; (advice-add 'compile-internal :after #'compile-internal--scroll)

    ;; turn on else minor mode
    ;;(else-mode)
    )
  (add-hook 'c-mode-common-hook #'custom-c-mode-common-hook)

  ;; remove trailing blanks
  ;;(add-hook 'c-mode-hook #'install-remove-trailing-blanks)
  ;;(add-hook 'c++-mode-hook #'install-remove-trailing-blanks)
  ;;(add-hook 'c-mode-common-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'c-mode-hook #'install-remove-tabs)
  ;;(add-hook 'c++-mode-hook #'install-remove-tabs)
  ;;(add-hook 'c-mode-common-hook #'install-remove-tabs)
  )
;; C Mode:1 ends here

;; [[file:init-emacs.org::#modes-calendar][Calendar:1]]
;;------------------------------------------------------------------------------
;;; Modes: Calendar
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Calendar")

(use-package calendar
  :straight (:type built-in)
  :bind* ("C-x c" . calendar)
  :bind (:map calendar-mode-map
              ;; scrolling keys
              (">" . calendar-scroll-left)
              ("<" . calendar-scroll-right)
              ("C-x >" . calendar-scroll-left)
              ("C-x <" . calendar-scroll-right))
  :config
  ;; turn off diary entries view when calendar is run
  (setq calendar-view-diary-initially-flag nil)
  ;; set number of diary days to show
  (setq diary-number-of-entries 10)
  ;; turn on calendar marks for diary entries
  (setq mark-diary-entries-in-calendar t)
  ;; turn on diary appointments
  ;;(add-hook 'diary-hook #'appt-make-list)
  ;; star current date
  (setq calendar-today-visible-hook 'calendar-star-date)
  ;; star today's date
  (setq calendar-today-visible-hook 'calendar-mark-today)
  ;; mark holidays
  (setq calendar-mark-holidays-flag t))

;;------------------------------------------------------------------------------
;;;; calendar-remind
;;------------------------------------------------------------------------------

(init-message 3 "calendar-remind")

(use-package calendar-remind
  :load-path (lambda () (file-truename (expand-file-name "calendar-remind.el" local-modules-dir)))
  :after (calendar)
  :commands (calendar-remind-lookup
             calendar-remind-visit
             calendar-remind-visit-insert)
  :bind (:map calendar-mode-map
              ;; remind lookup
              ("<return>" . calendar-remind-lookup)
              ("r" . calendar-remind-lookup)
              ;;("SPC" . calendar-remind-lookup)
              ;; remind visit
              ("v" . calendar-remind-visit)
              ("V" . calendar-remind-visit-insert)))
;; Calendar:1 ends here

;; [[file:init-emacs.org::#modes-css-mode][CSS Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: CSS Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: CSS Mode")

(use-package css-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . css-mode))
  :custom
  (cssm-indent-function #'cssm-c-style-indenter))
;; CSS Mode:1 ends here

;; [[file:init-emacs.org::#modes-dired][Dired:1]]
;;------------------------------------------------------------------------------
;;; Modes: Dired
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Dired")

(defun custom-dired-mode-hook ())

(use-package dired
  :straight (:type built-in)
  ;;:after (dired-single)
  :commands (dired dired-jump)
  ;;:hook (dired-mode . custom-dired-mode-hook)
  ;;:bind* ("C-x j" . dired-jump)
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("C-c C-z f" . browse-url-of-dired-file))
  :custom
  ;; only prompt once for recursive deletes
  (dired-recursive-deletes 'top)
  ;; auto-revert buffer upon revisiting
  (dired-auto-revert-buffer t)
  ;; list directories first and remove unwanted fields
  (dired-listing-switches "-alhGv1 --time-style=+%F --group-directories-first")
  ;; supplemental rules for suggested commands based on file extensions
  ;; see `dired-guess-shell-alist-default' for default list
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "evince")
     ("\\.htm?l\\'" "firefox")
     ("\\.doc?x\\'" "libreoffice --writer")
     ("\\.odt\\'" "libreoffice --writer")
     ("\\.p[bgpn]m\\'" "gpicview")
     ("\\.bmp\\'" "gpicview")
     ("\\.gif\\'" "gpicview")
     ("\\.tif?f\\'" "gpicview")
     ("\\.png\\'" "gpicview")
     ("\\.jpe?g\\'" "gpicview")
     ("\\.mpe?g\\'" "vlc")
     ("\\.avi\\'" "vlc")
     ("\\.mkv\\'" "vlc")
     ("\\.mp4\\'" "vlc")
     ("\\.wmv\\'" "vlc")
     ("\\.mp3\\'" "audacious")
     ("\\.ogg\\'" "audacious")
     ("\\.wav\\'" "audacious")))
  :config
  ;; mac support
  (when window-system-mac
    (setq insert-directory-program "gls"
          dired-use-ls-dired t))

  ;; add extra file compression support
  (setq dired-compress-files-alist
        '(("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
          ("\\.bz2\\'" . "bzip2 -c9 %i > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
          ("\\.xz\\'" . "xz -c9 %i > %o")
          ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
          ("\\.zst\\'" . "zstd -19 %i -o %o")
          ("\\.zip\\'" . "zip %o -r --filesync %i")))

  (defun dired-move-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))
  (define-key dired-mode-map [remap beginning-of-buffer] 'dired-move-to-top)

  (defun dired-move-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (define-key dired-mode-map [remap end-of-buffer] 'dired-move-to-bottom))

;;------------------------------------------------------------------------------
;;;; dired-single
;;------------------------------------------------------------------------------

(init-message 3 "dired-single")

;; make dired use a single buffer
(use-package dired-single
  :straight t
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("<return>" . dired-single-buffer)
              ("f" . dired-single-buffer)
              ("^" . dired-single-buffer-up)
              ("b" . dired-single-buffer-up)
              ("<mouse-1>" . dired-single-buffer-mouse))
  :init
  (defun dired-single-buffer-up ()
    (interactive)
    (dired-single-buffer ".."))
  :config
  ;; advise `dired-single-buffer-mouse' to suppress errors
  (advice-add 'dired-single-buffer-mouse :around #'advice--ignore-interactive-errors))

;;------------------------------------------------------------------------------
;;;; dired-open
;;------------------------------------------------------------------------------

(init-message 3 "dired-open")

;; open files with external programs
(use-package dired-open
  :straight t
  :custom
  (dired-open-extensions
   '(("png" . "display")
     ("gif" . "display")
     ("jpg" . "display")
     ("jepg" . "display")
     ("mpg" . "vlc")
     ("mpeg" . "vlc")
     ("avi" . "vlc")
     ("mov" . "vlc"))))

;;------------------------------------------------------------------------------
;;;; dired-hide-dotfiles
;;------------------------------------------------------------------------------

(init-message 3 "dired-hide-dotfiles")

(use-package dired-hide-dotfiles
  :straight t
  ;;:hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;; ;;------------------------------------------------------------------------------
;; ;;;; all-the-icons-dired
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "all-the-icons-dired")

;; ;; add icons to file listings
;; (use-package all-the-icons-dired
;;   :straight t
;;   :commands (dired dired-jump)
;;   :hook (dired-mode . all-the-icons-dired-mode))
;; Dired:1 ends here

;; [[file:init-emacs.org::#modes-ediff][Ediff:1]]
;;------------------------------------------------------------------------------
;;; Modes: Ediff
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Ediff")

(use-package ediff
  :straight (:type built-in)
  :custom
  ;; split windows horizontally
  (ediff-split-window-function 'split-window-horizontally)
  ;; only highlight current diff
  (ediff-highlight-all-diffs nil)
  ;; turn off whitespace checking
  (ediff-diff-options "-w")
  ;; place control window in same frame
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; place control window in separate frame
  ;;(ediff-window-setup-function 'ediff-setup-windows-multiframe)
  ;; highlight changes to characters rather than words
  ;;(ediff-forward-word-function 'forward-char)
  )
;; Ediff:1 ends here

;; [[file:init-emacs.org::#modes-erlang-mode][Erlang Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Erlang Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Erlang Mode")

(use-package erlang
  :straight t
  :after (flyspell)
  :mode ("\\.erl\\'" . erlang-mode)
  :interpreter ("erlang" . erlang-mode)
  :commands (erlang-start)
  :init
  (add-to-list 'exec-path "/usr/lib/erlang/bin" t)
  (setq erlang-root-dir "/usr/lib/erlang"
        erlang-electric-commands nil)

  :config
  (defun custom-erlang-hook ()
    ;; turn on flyspell
    (flyspell-prog-mode))
  (add-hook 'erlang-hook #'custom-erlang-hook)

  ;; remove trailing blanks
  ;;(add-hook 'erlang-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'erlang-mode-hook #'install-remove-tabs)
  )
;; Erlang Mode:1 ends here

;; [[file:init-emacs.org::#modes-fundamental-mode][Fundamental Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Fundamental Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Fundamental Mode")

;; fundamental-mode

;; make sure tabs are not inserted
(setq indent-tabs-mode nil)

;; ;; turn off auto-fill
;; (turn-off-auto-fill)

;; turn off auto-save
(auto-save-mode nil)

;; remove trailing blanks
;;(add-hook 'fundamental-mode-hook #'install-remove-trailing-blanks)

;; remove tabs
;;(add-hook 'fundamental-mode-hook #'install-remove-tabs)
;; Fundamental Mode:1 ends here

;; [[file:init-emacs.org::#modes-geiser-racket-scheme-repl][Geiser (Racket Scheme REPL):1]]
;;------------------------------------------------------------------------------
;;; Modes: Geiser (Racket Scheme REPL)
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Geiser (Racket Scheme REPL)")

(use-package geiser
  :straight t
  :commands (geiser-mode
             run-geiser)
  :init
  ;; set default scheme program to racket
  ;;(setq scheme-program-name "racket")
  ;; set default scheme mode to geiser-mode
  ;;(add-hook 'scheme-mode-hook #'geiser-mode)

  :config
  ;; ;; set default geiser implementation to racket
  ;; (setq geiser-default-implementation 'racket)
  ;; ;; set active implementations list to just racket
  ;; (setq geiser-active-implementations '(racket))

  ;; define `insert-char' functions to insert unicode chars
  (defun geiser-insert-sigma ()
    "Insert ∑ character."
    (interactive "*")
    ;;(insert-char ?Σ))
    (insert-char ?∑))

  (defun custom-geiser-mode-hook ()
    ;; key bindings
    (local-set-key (kbd "C-c \\") 'geiser-insert-lambda)
    (local-set-key (kbd "C-c C-\\") 'geiser-insert-lambda)
    (local-set-key (kbd "C-c s") 'geiser-insert-sigma)
    (local-set-key (kbd "C-c C-s") 'geiser-insert-sigma))
  (add-hook 'geiser-mode-hook #'custom-geiser-mode-hook)
  (add-hook 'geiser-repl-mode-hook #'custom-geiser-mode-hook))

(use-package geiser-racket
  :straight t
  :after (geiser)
  :commands (run-racket)
  :init
  ;; set default scheme program to racket
  ;;(setq scheme-program-name "racket")
  ;; set default scheme mode to geiser-mode
  ;;(add-hook 'scheme-mode-hook #'geiser-mode)

  :config
  ;; set default geiser implementation to racket
  (setq geiser-default-implementation 'racket)
  ;; set active implementations list to just racket
  (setq geiser-active-implementations '(racket)))
;; Geiser (Racket Scheme REPL):1 ends here

;; [[file:init-emacs.org::#modes-gnu-plot][GNU Plot:1]]
;;------------------------------------------------------------------------------
;;; Modes: GNU Plot
;;------------------------------------------------------------------------------

(init-message 2 "Modes: GNU Plot")

(use-package gnuplot
  :straight t
  :mode ("\\.gp\\'" . gnuplot-mode)
  :commands (gnuplot-mode gnuplot-make-buffer gnuplot-send-string-to-gnuplot))
;; GNU Plot:1 ends here

;; [[file:init-emacs.org::#modes-go-mode][+Go Mode+:1]]
;; ;;------------------------------------------------------------------------------
;; ;;; Modes: Go Mode
;; ;;------------------------------------------------------------------------------

;; (init-message 2 "Modes: Go Mode")

;; (use-package go-mode
;;   :straight (:type built-in)
;;   :mode (("\\.go\\'" . go-mode))
;;   :config (progn
;;             (defun custom-go-mode-hook ()
;;               ;; use goimports instead of gofmt
;;               (setq gofmt-command "goimports")

;;               ;; call gofmt before saving
;;               (add-hook 'before-save-hook #'gofmt-before-save)

;;               ;; customize compile command to run go build
;;               (when (not (string-match "go" compile-command))
;;                 (setq-local compile-command "go build -v && go test -v && go vet"))

;;               ;; oracle
;;               (load-file "${GOPATH}/src/golang.org/x/tools/cmd/oracle/oracle.el")

;;               ;; key bindings
;;               (local-set-key (kbd "M-.") 'godef-jump))
;;             (add-hook 'go-mode-hook #'custom-go-mode-hook)))
;; +Go Mode+:1 ends here

;; [[file:init-emacs.org::#modes-graphviz-dot-mode][Graphviz Dot Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Graphviz Dot Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Graphviz Dot Mode")

(use-package graphviz-dot-mode
  :straight t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :commands (graphviz-dot-mode))
;; Graphviz Dot Mode:1 ends here

;; [[file:init-emacs.org::#modes-ini-mode][INI Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: INI Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: INI Mode")

(use-package ini-mode
  :straight t
  :mode ("\\.ini\\'" . ini-mode))
;; INI Mode:1 ends here

;; [[file:init-emacs.org::#modes-javascript-js2-mode][Javascript: js2 Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Javascript: js2 Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Javascript: js2 Mode")

(use-package js2-mode
  :straight t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.gradle\\'" . js-mode))    ; use js-mode for gradle files
  :interpreter ("node" . js2-mode)
  :config
  ;; set indent offset
  (setq-local py-indent-offset custom-short-tab-width)

  ;; turn on auto indent
  (setq js2-auto-indent-p t
        js2-cleanup-whitespace t
        js2-enter-indents-newline t
        js2-indent-on-enter-key t
        js2-bounce-indent-p nil
        js2-mirror-mode nil
        js2-mode-escape-quotes nil)
  ;;js2-electric-keys (quote nil))

  ;; better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  )

;;------------------------------------------------------------------------------
;;;; js2-refactor
;;------------------------------------------------------------------------------

(init-message 3 "js2-refactor")

;; javascript refactoring library
(use-package js2-refactor
  :straight t
  :after (js2-mode)
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)
              ("M-." . nil))            ; unbind conflicting key
  :config
  ;; set prefix key
  (js2r-add-keybindings-with-prefix "C-c C-r")

  ;; load when `js2-mode' is active
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

;;------------------------------------------------------------------------------
;;;; xref-js2
;;------------------------------------------------------------------------------

(init-message 3 "xref-js2")

;; jump to references and definitions
(use-package xref-js2
  :straight t
  :after (js2-mode)
  :config
  ;; load when `js2-mode' is active
  (defun js2-mode-hook--js2-refactor ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
    (add-hook 'js2-mode-hook #'js2-mode-hook--js2-refactor)))

;;------------------------------------------------------------------------------
;;;; js-comint
;;------------------------------------------------------------------------------

(init-message 3 "js-comint")

;; javascript interpreter repl
(use-package js-comint
  :straight t
  :after (js2-mode)
  :commands (js-send-buffer
             js-send-buffer-and-go
             js-send-last-sexp
             js-send-last-sexp-and-go
             js-load-file-and-go)
  :bind (:map js2-mode-map
              ("C-c C-c" . js-eval-sexp-and-go)
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-eval-sexp-and-go)
              ("C-c b" . js-send-buffer)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c C-k" . js-send-buffer-and-go)
              ("C-c l" . js-load-file-and-go))
  :config
  ;;(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
  ;;(setq inferior-js-program-command "/usr/bin/rhino")

  ;; ;; key bindings
  ;; (bind-keys :map js2-mode-map
  ;;            ("C-c C-c" . js-eval-sexp-and-go)
  ;;            ("C-x C-e" . js-send-last-sexp)
  ;;            ("C-M-x" . js-eval-sexp-and-go)
  ;;            ("C-c b" . js-send-buffer)
  ;;            ("C-c C-b" . js-send-buffer-and-go)
  ;;            ("C-c C-k" . js-send-buffer-and-go)
  ;;            ("C-c l" . js-load-file-and-go))

  (defun js-eval-sexp ()
    "js-comint evaluate current sexp."
    (interactive)
    (save-mark-and-excursion
      (end-of-defun)
      (js-send-last-sexp)))

  (defun js-eval-sexp-and-go ()
    "js-comint evaluate current sexp and switch to js buffer."
    (interactive)
    (save-mark-and-excursion
      (end-of-defun)
      (js-send-last-sexp-and-go)))

  (defun js2-mode-hook--js-comint ()
    ;; (local-set-key (kbd "C-c C-c") 'js-eval-sexp-and-go)
    ;; (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
    ;; (local-set-key (kbd "C-M-x") 'js-eval-sexp-and-go)
    ;; (local-set-key (kbd "C-c b") 'js-send-buffer)
    ;; (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
    ;; (local-set-key (kbd "C-c C-k") 'js-send-buffer-and-go)
    ;; (local-set-key (kbd "C-c l") 'js-load-file-and-go)
    ;; disable skewer-mode as it uses similar key bindings
    (when (fboundp 'skewer-mode)
      (skewer-mode -1)))
  (add-hook 'js2-mode-hook #'js-mode-hook--js-comint))
;; Javascript: js2 Mode:1 ends here

;; [[file:init-emacs.org::#modes-json-mode][JSON Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: JSON Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: JSON Mode")

(use-package json-mode
  :straight t
  :mode (("\\.json\\'" . json-mode))
  :config
  ;; set indentation to four spaces
  (setq json-encoding-default-indentation "    "
        json-encoding-pretty-print t))
;; JSON Mode:1 ends here

;; [[file:init-emacs.org::#modes-latex][LaTeX:1]]
;;------------------------------------------------------------------------------
;;; Modes: LaTeX
;;------------------------------------------------------------------------------

(init-message 2 "Modes: LaTeX")

;;------------------------------------------------------------------------------
;;; Packages: tex
;;------------------------------------------------------------------------------

(init-message 3 "Packages: tex")

(use-package tex
  :straight (:type built-in)
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (latex-noindent-commands '("footnote"))
  :config
  ;;(setq TeX-master "paper.tex")
  ;;(setq-default TeX-master TeX-master)
  ;; update PDF buffers after successful LaTeX run
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;;------------------------------------------------------------------------------
;;; Packages: pdf-tools
;;------------------------------------------------------------------------------

(init-message 3 "Packages: pdf-tools")

(use-package pdf-tools
  :straight t
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward))
  ;; :hook (pdf-view-mode . bms/pdf-midnite-amber) ; turn on midnight-mode for pdfs
  :config
  (pdf-tools-install))

;; ;;------------------------------------------------------------------------------
;; ;;; Packages: auctex-latexmk
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "Packages: auctex-latexmk")

;; (use-package auctex-latexmk
;;   :straight t
;;   :custom
;;   (auctex-latexmk-inherit-TeX-PDF-mode t)
;;   :config
;;   ;;(setq TeX-master nil)
;;   ;;(setq-default TeX-master TeX-master)
;;   (auctex-latexmk-setup))

;; ;;------------------------------------------------------------------------------
;; ;;; Packages: company-acutex
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "Packages: company-acutex")

;; (use-package company-auctex
;;   :straight t
;;   :after (company)
;;   :init (company-auctex-init))
;; LaTeX:1 ends here

;; [[file:init-emacs.org::#modes-ledger-mode][Ledger Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Ledger Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Ledger Mode")

(use-package ledger-mode
  :straight t
  :functions (ledger-align-amounts)
  :config
  (defun custom-ledger-align-amounts ()
    "Return `ledger-align-amounts' for entire buffer."
    (save-mark-and-excursion
      (goto-char (point-min))
      (ledger-align-amounts 52)))

  (defun custom-ledger-mode-hook ()
    ;; align amounts on save
    (add-hook 'before-save-hook #'custom-ledger-align-amounts 0 :local))
  (add-hook 'ledger-mode-hook #'custom-ledger-mode-hook))
;; Ledger Mode:1 ends here

;; [[file:init-emacs.org::#modes-lisp-mode][Lisp Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Lisp Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Lisp Mode")

(defun custom-lisp-mode-hook ()
  ;; disable tabs
  (disable-tabs)

  ;; clear input method
  ;;(set-input-method nil)

  ;; set indent function for lisp-mode to `lisp-indent-function'
  ;; (it defaults to `clisp-indent-function')
  (when (equal major-mode 'lisp-mode)
    (setq-local lisp-indent-function 'lisp-indent-function))

  ;; add underscore and dash to word boundaries
  (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
  (modify-syntax-entry ?- "w" lisp-mode-syntax-table)

  ;; define keys
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (local-set-key (kbd "S-<tab>") 'lisp-complete-symbol)

  ;; turn on flyspell
  (flyspell-prog-mode)

  ;; initialize eldoc
  (eldoc-mode 1)

  ;; ;; turn on abbreviation mode
  ;; (abbrev-mode 1)

  ;; turn on else minor mode
  ;;(else-mode)

  ;; ;; initialize elisp slime nav mode
  ;; (when (fboundp 'elisp-slime-nav-mode)
  ;;   (elisp-slime-nav-mode)
  ;;   (when (fboundp 'diminish)
  ;;     (with-eval-after-load "elisp-slime-nav"
  ;;       (diminish 'elisp-slime-nav-mode))))

  ;; ;; check parenthesis after file save
  ;; (add-hook 'after-save-hook #'check-parens nil t)

  ;; (use-package aggressive-indent
  ;;   :straight (:type built-in)
  ;;   :config (aggressive-indent-mode 1))

  ;; set outline header regexp
  (setq-local outline-regexp "\\(;; [*]\\{1,8\\} \\|;;[;]\\{1,8\\} \\)")
  (setq-local outline-level 'lisp-outline-level))

(use-package lisp-mode
  :straight (:type built-in)
  :after (flyspell eldoc info-look)
  :commands (emacs-lisp-mode)
  :functions (custom-lisp-mode-hook)
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.clisp\\'" . lisp-mode))
  :hook ((emacs-lisp-mode . custom-lisp-mode-hook)
         (lisp-mode . custom-lisp-mode-hook)
         (common-lisp-mode . custom-lisp-mode-hook))
  :config
  ;; remove trailing blanks
  ;;(add-hook 'emacs-lisp-mode-hook #'install-remove-trailing-blanks)
  ;;(add-hook 'lisp-mode-hook #'install-remove-trailing-blanks)
  ;;(add-hook 'common-lisp-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'emacs-lisp-mode-hook #'install-remove-tabs)
  ;;(add-hook 'lisp-mode-hook #'install-remove-tabs)
  ;;(add-hook 'common-lisp-mode-hook #'install-remove-tabs)
  )
;; Lisp Mode:1 ends here

;; [[file:init-emacs.org::#modes-lua-mode][LUA Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: LUA Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: LUA Mode")

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode)))
;; LUA Mode:1 ends here

;; [[file:init-emacs.org::#modes-makefile-mode][Makefile Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Makefile Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Makefile Mode")

(use-package make-mode
  :straight (:type built-in)
  :mode ("Makefile" . makefile-mode)
  :config
  (defun custom-makefile-mode-hook ()
    ;; enable tabs
    (enable-tabs))
  (add-hook 'makefile-mode-hook #'custom-makefile-mode-hook))
;; Makefile Mode:1 ends here

;; [[file:init-emacs.org::#modes-markdown-mode][Markdown Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Markdown Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Markdown Mode")

(use-package markdown-mode
  :straight t
  :after (org-table)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :functions (markdown-mode-fix-org-tables)
  ;; use org table mode for markdown tables
  :hook (markdown-mode . orgtbl-mode)
  :config
  (defun markdown-mode-fix-org-tables ()
    "Hook to fix org table format on save."
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward "-+-" nil :noerror)
          (replace-match "-|-")))))
  (defun before-save-hook--markdown-mode-fix-org-tables ()
    "Add hook to run `markdown-mode-fix-org-tables' before saving
Markdown files."
    (add-hook 'before-save-hook #'markdown-mode-fix-org-tables nil 'make-it-local))
  (add-hook 'markdown-mode-hook #'before-save-hook--markdown-mode-fix-org-tables))
;; Markdown Mode:1 ends here

;; [[file:init-emacs.org::#modes-nix][Nix:1]]
;;------------------------------------------------------------------------------
;;; Modes: Nix
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Nix")

(use-package nix-mode
  :straight t
  :mode ("\\.nix\\'" . nix-mode))
;; Nix:1 ends here

;; [[file:init-emacs.org::#modes-perl-mode][Perl Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Perl Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Perl Mode")

(use-package perl-mode
  :straight (:type built-in)
  :after (flyspell)
  :mode (("\\.\\([pP][Llm]\\|al\\|t\\)\\'" . perl-mode)
         ("perl" . perl-mode)
         ("perl5" . perl-mode))
  :interpreter ("miniperl" . perl-mode)
  :config
  (defun custom-perl-mode-hook ()
    ;; configure some options
    (setq perl-indent-level 4)
    (setq perl-indent-parens-as-block t)
    (setq perl-indent-continued-arguments t)
    (setq perl-continued-statement-offset 4)

    ;; turn on flyspell
    (flyspell-prog-mode))
  (add-hook 'perl-mode-hook #'custom-perl-mode-hook)

  (defun perl-mode-maybe ()
    "Determine if file is a perl script and switch to perl-mode if it is."
    (interactive)
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (when (or
               (search-forward "#!/usr/bin/perl" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env perl" (line-end-position) :noerror))
          (perl-mode)))))

  ;; run when a file is loaded
  (add-hook 'find-file-hook #'perl-mode-maybe)

  ;; remove trailing blanks
  ;;(add-hook 'perl-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'perl-mode-hook #'install-remove-tabs)
  )
;; Perl Mode:1 ends here

;; [[file:init-emacs.org::#modes-plantuml-mode][PlantUML Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: PlantUML Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: PlantUML Mode")

(use-package plantuml-mode
  :straight t)
;; PlantUML Mode:1 ends here

;; [[file:init-emacs.org::#modes-python-mode][Python Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Python Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Python Mode")

(use-package python-mode
  :straight t
  :after (company elpy)
  :mode (("\\.py\\'" . python-mode)
         ("\\.python\\'" . python-mode))
  :commands (py--buffer-filename-remote-maybe)
  :functions (custom-python-mode-hook)
  :config
  ;; turn off auto-indent so code pasting works
  (setq py-python-command-args '("--no-autoindent" "--colors=Linux"))

  ;; switch to interpreter after executing code
  ;;(setq py-switch-buffers-on-execute-p t)

  ;; split windows
  ;;(setq py-split-windows-on-execute t)
  (setq py-keep-windows-configuration 'force)

  ;; try to automatically figure out indentation
  (setq py-smart-indentation t)

  ;; execute python in source code blocks on 'C-c C-c'
  ;;(add-to-list 'org-ctrl-c-ctrl-c-hook #'org-babel-async-execute:python)

  ;; increase recursion depth of auto-complete to prevent errors
  (setq py-max-specpdl-size 999)

  (defun custom-python-mode-hook ()
    ;; override some default keybindings
    (when (fboundp 'backward-delete-word)
      (bind-keys* ("C-<backspace>" . backward-delete-word))) ; default: `py-hungry-delete-backwards'

    ;; set indent offset
    (setq-local py-indent-offset custom-short-tab-width)

    ;; set outline header regexp
    (setq-local outline-regexp " *\\(def \\|clas\\|#hea\\)")
    ;;(hide-sublevels 1)

    ;; remove python-shell-completion-at-point from completion-at-point-functions,
    ;; if it is not defined
    (when (and (not (fboundp 'python-shell-completion-at-point))
               (memq 'python-shell-completion-at-point completion-at-point-functions))
      (setq completion-at-point-functions
            (remove 'python-shell-completion-at-point completion-at-point-functions)))
    )
  (add-hook 'python-mode-hook #'custom-python-mode-hook))

;;------------------------------------------------------------------------------
;;;; elpy
;;------------------------------------------------------------------------------

(use-package elpy
  :straight t
  :commands (elpy-enable
             elpy-shell-switch-to-shell)
  :config
  ;; turn on elpy mode
  (elpy-enable)

  ;; hack to fix quote error issue
  (setq elpy-eldoc-show-current-function nil)

  ;; custom version of `elpy-shell-switch-to-shell' that opens shell in a split window
  (defun elpy-shell-switch-to-shell ()
    "Switch to inferior Python process buffer."
    (interactive)
    (setq elpy--shell-last-py-buffer (buffer-name))
    (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process)) t))

  ;; enable flycheck
  (when (fboundp 'flycheck-mode)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook #'flycheck-mode)))

;;------------------------------------------------------------------------------
;;;; jedi
;;------------------------------------------------------------------------------

;; (init-message 3 "jedi")

;; ;; python auto-completion
;; (use-package jedi
;;   :straight t
;;   :after (python-mode)
;;   :config
;;   ;; ;; add jedi completions to auto-complete sources
;;   ;; (add-to-list 'ac-sources 'ac-source-jedi-direct t)
;;   ;; enable with python mode
;;   (add-hook 'python-mode-hook #'jedi:setup))
;; Python Mode:1 ends here

;; [[file:init-emacs.org::#modes-racket-mode][Racket Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Racket Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Racket Mode")

;; (use-package racket-mode
;;   :straight t
;;   :mode ("\\.rkt\\'" . racket-mode)
;;   :interpreter ("racket" . racket-mode)
;;   :commands (racket-mode
;;              racket-repl)
;;   :config
;;   (defun custom-racket-mode-hook ()
;;     ;; do not auto-complete on tab
;;     (setq tab-always-indent t))
;;   (add-hook 'racket-mode-hook #'custom-racket-mode-hook)

;;   ;; turn on support for unicode input
;;   (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
;;   (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(use-package scheme
  :straight (:type built-in)
  :after (geiser)
  :mode ("\\.rkt\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :commands (racket-mode
             scheme-mode)
  :config
  ;; create racket mode based on scheme mode
  (define-derived-mode racket-mode scheme-mode "Scheme"
    "Major mode for editing Racket Scheme code. Editing commands
are similar to those of `lisp-mode'.

In addition, if an inferior Racket Scheme process is running,
some additional commands will be defined, for evaluating
expressions and controlling the interpreter, and the state of the
process will be displayed in the mode line of all Scheme buffers.
The names of commands that interact with the Scheme process start
with \"xscheme-\" if you use the MIT Scheme-specific `xscheme'
package; for more information see the documentation for
`xscheme-interaction-mode'. Use \\[run-scheme] to start an
inferior Scheme using the more general `cmuscheme' package.

Commands:

  - Delete converts tabs to spaces as it moves back.
  - Blank lines separate paragraphs.
  - Semicolons start comments.

\\{scheme-mode-map}"
    ;; turn on geiser-mode
    (when (fboundp 'geiser-mode)
      (geiser-mode t)))

  ;; ;; racket files should use racket-mode
  ;; (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  )
;; Racket Mode:1 ends here

;; [[file:init-emacs.org::#modes-ruby-mode][Ruby Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Ruby Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Ruby Mode")

(use-package ruby-mode
  :straight (:type built-in)
  :after (flyspell)
  :mode ("\\.rb\\'" . ruby-mode)
  :config
  (defun custom-ruby-mode-hook ()
    ;; set indent level
    (setq ruby-indent-level 2)
    ;;(setq ruby-indent-level 4)

    ;; define keys
    (define-key ruby-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
    ;; undefine electric keys
    (define-key ruby-mode-map (kbd "{") 'self-insert-command)
    (define-key ruby-mode-map (kbd "}") 'self-insert-command)

    ;; turn on flyspell
    (when (boundp 'flyspell-prog-mode)
      (flyspell-prog-mode)))
  (add-hook 'ruby-mode-hook #'custom-ruby-mode-hook :append)

  ;; turn on flyspell
  (flyspell-prog-mode)

  ;; FIXME: No longer works
  ;; (use-package flymake
  ;;   :straight (:type built-in)
  ;;   :config (progn
  ;;             (defun flymake-ruby-init ()
  ;;               (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;                      (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;                 (list "ruby" (list "-c" local-file))))

  ;;             (defun flymake-ruby-enable ()
  ;;               (when (and buffer-file-name
  ;;                          (file-writable-p (file-name-directory buffer-file-name))
  ;;                          (file-writable-p buffer-file-name)
  ;;                          (if (fboundp 'tramp-list-remote-buffers)
  ;;                              (not (cl-subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
  ;;                            t))
  ;;                 (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  ;;                 (flymake-mode t)))

  ;;             (add-to-list 'flymake-allowed-file-name-masks '(".+\\.rb\\'" flymake-ruby-init) t)
  ;;             (add-to-list 'flymake-allowed-file-name-masks '("Rakefile\\'" flymake-ruby-init) t)
  ;;             (add-to-list 'flymake-err-line-patterns '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) t)
  ;;             (add-hook 'ruby-mode-hook #'flymake-ruby-enable)))

  ;; ;; turn on syntax highlighting (actually turns off syntax highlighting)
  ;; (add-hook 'ruby-mode-hook #'turn-on-font-lock)

  ;; ;; turn on abbreviation mode
  ;; (abbrev-mode 1)

  (defun ruby-mode-maybe ()
    "Determine if file is a ruby script and switch to `ruby-mode' if it is."
    (interactive)
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (when (or
               (search-forward "#!/usr/bin/ruby" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env ruby" (line-end-position) :noerror))
          (ruby-mode)))))

  ;; run when a file is loaded
  (add-hook 'find-file-hook #'ruby-mode-maybe)

  ;; remove trailing blanks
  ;;(add-hook 'ruby-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'ruby-mode-hook #'install-remove-tabs)
  )

;;------------------------------------------------------------------------------
;;;; robe
;;------------------------------------------------------------------------------

(init-message 3 "robe")

;; code navigation, documentation lookup, and completion for ruby
(use-package robe
  :straight t
  :after (ruby-mode)
  :commands (robe-mode)
  :config
  (add-hook 'ruby-mode-hook #'robe-mode))

;; ;;------------------------------------------------------------------------------
;; ;;;; inf-ruby
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "inf-ruby")

;; (use-package inf-ruby
;;   :straight t
;;   :after (ruby-mode)
;;   :interpreter ("ruby" . ruby-mode)
;;   :commands (run-ruby inf-ruby-keys)
;;   :config
;;   (defun custom-ruby-mode-hook-inf-ruby-keys ()
;;     (inf-ruby-keys))
;;   (add-hook 'ruby-mode-hook #'custom-ruby-mode-hook-inf-ruby-keys)

;;   ;; use ruby-robe with inf-ruby
;;   (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;     (rvm-activate-corresponding-ruby)))

;; ;;------------------------------------------------------------------------------
;; ;;;; ac-inf-ruby
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "ac-inf-ruby")

;; ;; auto-complete source for interactive ruby
;; (use-package ac-inf-ruby
;;   :straight t
;;   :config
;;   (add-to-list 'ac-modes 'inf-ruby-mode t)
;;   (add-hook 'inf-ruby-mode-hook #'ac-inf-ruby-enable)
;;   ;; ;; make TAB auto-complete
;;   ;; (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete)
;;   )
;; Ruby Mode:1 ends here

;; [[file:init-emacs.org::#modes-rust-mode][Rust Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Rust Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Rust Mode")

(use-package rust-mode
  :straight t
  :after (flyspell)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run)
              ("C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c C-f" . rust-format-buffer)
              ("C-c C-n" . rust-goto-format-problem))
  :custom
  (rustic-lsp-client nil)
  :config
  (defun custom-rust-mode-hook ()
    ;; use spaces for tabs
    (setq indent-tabs-mode nil)

    ;; set indent level
    (setq rust-indent-level 4)

    ;; format code on save
    (setq rust-format-on-save t)

    ;; ;; define keys
    ;; (define-key rust-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
    ;; ;; undefine electric keys
    ;; (define-key rust-mode-map (kbd "{") 'self-insert-command)
    ;; (define-key rust-mode-map (kbd "}") 'self-insert-command)

    ;; turn on flyspell
    (when (boundp 'flyspell-prog-mode)
      (flyspell-prog-mode)))
  (add-hook 'rust-mode-hook #'custom-rust-mode-hook :append)

  ;; turn on flyspell
  (flyspell-prog-mode)

  ;; FIXME: No longer works
  ;; (use-package flymake
  ;;   :straight (:type built-in)
  ;;   :config (progn
  ;;             (defun flymake-rust-init ()
  ;;               (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;                      (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;                 (list "rust" (list "-c" local-file))))

  ;;             (defun flymake-rust-enable ()
  ;;               (when (and buffer-file-name
  ;;                          (file-writable-p (file-name-directory buffer-file-name))
  ;;                          (file-writable-p buffer-file-name)
  ;;                          (if (fboundp 'tramp-list-remote-buffers)
  ;;                              (not (cl-subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
  ;;                            t))
  ;;                 (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  ;;                 (flymake-mode t)))

  ;;             (add-to-list 'flymake-allowed-file-name-masks '(".+\\.rb\\'" flymake-rust-init) t)
  ;;             (add-to-list 'flymake-allowed-file-name-masks '("Rakefile\\'" flymake-rust-init) t)
  ;;             (add-to-list 'flymake-err-line-patterns '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) t)
  ;;             (add-hook 'rust-mode-hook #'flymake-rust-enable)))

  ;; ;; turn on syntax highlighting (actually turns off syntax highlighting)
  ;; (add-hook 'rust-mode-hook #'turn-on-font-lock)

  ;; ;; turn on abbreviation mode
  ;; (abbrev-mode 1)

  (defun rust-mode-maybe ()
    "Determine if file is a rust script and switch to `rust-mode' if it is."
    (interactive)
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (when (or
               (search-forward "#!/usr/bin/rust" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env rust" (line-end-position) :noerror))
          (rust-mode)))))

  ;; run when a file is loaded
  (add-hook 'find-file-hook #'rust-mode-maybe)

  ;; remove trailing blanks
  ;;(add-hook 'rust-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'rust-mode-hook #'install-remove-tabs)
  )

;;------------------------------------------------------------------------------
;;;; Rustic
;;------------------------------------------------------------------------------

(init-message 3 "Rustic")

(use-package rustic
  :straight t
  :after (rust-mode))
;; Rust Mode:1 ends here

;; [[file:init-emacs.org::#modes-sh-script][SH Script:1]]
;;------------------------------------------------------------------------------
;;; Modes: SH Script
;;------------------------------------------------------------------------------

(init-message 2 "Modes: SH Script")

(use-package sh-script
  :straight (:type built-in)
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.shell\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ;; use sh-mode for ldif and schema files
         ("\\.ldif\\'" . sh-mode)
         ("\\.schema\\'" . sh-mode)
         ;; use sh-mode for remind files
         ("\\.reminders\\'" . sh-mode)
         ("^reminders_" . sh-mode))
  :config
  ;; disable tabs
  (add-hook 'sh-mode-hook #'disable-tabs)

  ;; make comment lines indent
  (setq sh-indent-comment t)

  (defun sh-mode-maybe ()
    "Determine if file is a shell script and switch to sh-mode if it is."
    (interactive)
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (when (or
               (search-forward "#!/bin/sh" (line-end-position) :noerror)
               (search-forward "#!/bin/bash" (line-end-position) :noerror)
               (search-forward "#!/bin/csh" (line-end-position) :noerror)
               (search-forward "#!/bin/tsh" (line-end-position) :noerror)
               (search-forward "#!/bin/zsh" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env sh" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env bash" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env csh" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env tsh" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env zsh" (line-end-position) :noerror)
               (search-forward "#=========" (line-end-position) :noerror)
               (search-forward "#---------" (line-end-position) :noerror))
          (sh-mode)))))

  ;; run when a file is loaded
  (add-hook 'find-file-hook #'sh-mode-maybe)

  ;; remove trailing blanks
  ;;(add-hook 'sh-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'sh-mode-hook #'install-remove-tabs)
  )
;; SH Script:1 ends here

;; [[file:init-emacs.org::#modes-shell-mode][Shell Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Shell Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Shell Mode")

(use-package shell
  :straight (:type built-in)
  :commands (shell-mode)
  :config
  ;; disable tabs
  (add-hook 'shell-mode-hook #'disable-tabs)

  ;; set prompt to read only
  (setq comint-prompt-read-only t)

  ;; start a shell
  ;;(shell)
  )

;;------------------------------------------------------------------------------
;;;; ansi-color
;;------------------------------------------------------------------------------

(init-message 3 "ansi-color")

(use-package ansi-color
  :straight (:type built-in)
  :after (shell)
  :commands (ansi-color-for-comint-mode-on)
  :config
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))
;; Shell Mode:1 ends here

;; [[file:init-emacs.org::#modes-slime-mode-common-lisp][Slime Mode (Common Lisp):1]]
;;------------------------------------------------------------------------------
;;; Modes: Slime Mode (Common Lisp)
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Slime Mode (Common Lisp)")

;; superior lisp interaction mode for emacs
;; if loading slime errors out because swank-loader.lisp is not found, do:
;;   # cd /usr/share/emacs22/site-lisp/slime
;;   # for i in $(ls -1 /usr/share/common-lisp/source/slime/) ; do ln -s /usr/share/common-lisp/source/slime/$i ; done
(use-package slime
  :straight t
  ;; :load-path (;;(lambda () (file-truename (expand-file-name "slime" emacs-modules-dir)))
  ;;             ;;(lambda () (file-truename (expand-file-name "slime/contrib" emacs-modules-dir)))
  ;;             (lambda () (file-truename (expand-file-name "swank-clojure" emacs-modules-dir))))
  :commands (slime-autoloads
             slime
             slime-eval-buffer
             slime-eval-last-expression
             slime-interactive-eval
             slime-last-expression
             slime-mode
             slime-setup
             clisp
             clojure
             swank-clojure-init
             swank-clojure-slime-mode-hook
             swank-clojure-cmd
             swank-clojure-project)
  :config
  ;; clojure swank paths
  (setq swank-clojure-jar-path (file-truename (expand-file-name "~/.clojure/clojure.jar"))
        swank-clojure-binary (file-truename (expand-file-name "~/bin/clojure"))
        swank-clojure-extra-classpaths
        (list (file-truename (expand-file-name "~/.clojure/clojure.jar"))
              (file-truename (expand-file-name "~/.clojure/jline.jar"))
              (file-truename (expand-file-name "~/.clojure/clojure-contrib.jar"))))

  ;; slime setup
  (slime-setup)

  ;; set lisp program to clisp
  ;;(setq inferior-lisp-program "clisp -K full")

  (defun slime-eval-sexp ()
    "Slime evaluate current sexp."
    (interactive)
    (save-mark-and-excursion
      (end-of-defun)
      (slime-interactive-eval (slime-last-expression))))

  ;; run slime setup (this does too much)
  ;;(slime-setup)

  ;; redefine `slime-lisp-mode-hook'
  ;; set indent function for lisp-mode to `lisp-indent-function'
  ;; (it defaults to `common-lisp-indent-function')
  (defun slime-lisp-mode-hook ()
    (slime-mode 1)
    (setq-local lisp-indent-function 'lisp-indent-function))

  ;; run custom version of slime setup to preserve lisp-indent-function
  (defun custom-slime-lisp-mode-hook ()
    ;; ;; set lisp program
    ;; (setq inferior-lisp-program "clisp -K full")
    ;; (setq inferior-lisp-program "clisp -K base")

    ;; turn on slime mode
    (slime-mode 1)

    ;; typeout frame
    ;;(add-hook 'slime-connected-hook #'slime-ensure-typeout-frame)
    ;; highlight edits
    ;;(add-hook 'slime-mode-hook #'slime-highlight-edits-mode)

    ;; key bindings
    (bind-keys :map slime-mode-map
               ("C-c C-c" . slime-eval-sexp)
               ("C-x C-e" . slime-eval-last-expression)
               ("C-M-x" . slime-eval-sexp)
               ("C-c C-k" . slime-eval-buffer))

    ;; start inferior lisp job
    ;;(unless (comint-check-proc "*inferior-lisp*")
    ;;  (let ((buffer (current-buffer)))
    ;;    (slime)
    ;;    (switch-to-buffer buffer)))

    ;; ;; auto connect to slime
    ;; (unless (slime-connected-p)
    ;;   (save-mark-and-excursion (slime)))
    )
  (add-hook 'lisp-mode-hook #'custom-slime-lisp-mode-hook)
  (add-hook 'common-lisp-mode-hook #'custom-slime-lisp-mode-hook)
  (add-hook 'clojure-mode-hook #'custom-slime-lisp-mode-hook)

  ;; cldoc (common lisp info in minibuffer)
  (autoload 'turn-on-cldoc-mode "cldoc" nil t)
  (add-hook 'common-lisp-mode-hook #'turn-on-cldoc-mode)
  ;; wait 3 seconds before showing minibuffer docs
  (setq cldoc-idle-delay 3)

  ;; ;; slime motd warnings
  ;; (use-package slime-cl-pitfalls
  ;;   :straight (:type built-in))

  ;; add clisp to slime implementations
  (add-to-list 'slime-lisp-implementations '(clisp ("/usr/bin/clisp" "-K" "base")) t)

  (defun clisp ()
    "Start Common Lisp in Slime."
    (interactive)
    (slime 'clisp))

  ;; initialize clisp: quicklisp
  ;; $ clisp --load quicklisp.lisp
  ;; (quicklisp-quickstart:install)
  ;; (ql:add-to-init-file)
  ;; (ql:quickload 'restas)

  ;; add sbcl to slime implementations
  (add-to-list 'slime-lisp-implementations '(sbcl ("/usr/bin/sbcl")) t)

  (defun sbcl ()
    "Start Steel Bank Common Lisp in Slime."
    (interactive)
    (slime 'sbcl))

  ;; initialize sbcl: quicklisp
  ;; $ sbcl --load quicklisp.lisp
  ;; (quicklisp-quickstart:install)
  ;; (ql:add-to-init-file)
  ;; (ql:quickload 'restas)

  ;; add clojure to slime implementation
  ;; (add-to-list 'slime-lisp-implementations
  ;;              `(clojure (,(file-truename (expand-file-name "~/bin/clojure")))
  ;;                        :init swank-clojure-init
  ;;                        :coding-system utf-8-unix) t)
  (add-to-list 'slime-lisp-implementations `(clojure ("/usr/bin/clojure")) t)

  (defun clojure ()
    "Start Clojure in Slime."
    (interactive)
    (slime 'clojure))

  (defun custom-lisp-mode-hook-slime-mode ()
    "Hook to load slime-mode when lisp-mode is loaded."
    (slime-mode 1))
  (add-hook 'lisp-mode-hook #'custom-lisp-mode-hook-slime-mode)
  ;; (defun custom-inferior-lisp-mode-hook-inferior-slime-mode ()
  ;;   (inferior-slime-mode 1))
  ;; (add-hook 'inferior-lisp-mode-hook #'custom-inferior-lisp-mode-hook-inferior-slime-mode)
  )

;;------------------------------------------------------------------------------
;;;; ac-slime
;;------------------------------------------------------------------------------

;; (init-message 3 "ac-slime")

;; ;; auto-complete source for slime
;; (use-package ac-slime
;;   :straight t
;;   :after (slime)
;;   :config
;;   (add-hook 'slime-mode-hook #'set-up-slime-ac)
;;   (add-hook 'slime-repl-mode-hook #'set-up-slime-ac)
;;   (add-to-list 'ac-modes 'slime-repl-mode t))

;;------------------------------------------------------------------------------
;;;; elisp-slime-nav-mode
;;------------------------------------------------------------------------------

(init-message 3 "elisp-slime-nav-mode")

(use-package elisp-slime-nav
  :straight t
  :after (slime)
  :diminish elisp-slime-nav-mode
  :commands (elisp-slime-nav-mode)
  :config (elisp-slime-nav-mode))
;; Slime Mode (Common Lisp):1 ends here

;; [[file:init-emacs.org::#modes-sql-mode][SQL Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: SQL Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: SQL Mode")

(use-package sql
  :straight (:type built-in)
  :mode (("\\.sql\\'" . sql-mode)
         ("\\.tbl\\'" . sql-mode)
         ("\\.sp\\'" . sql-mode))
  :hook (sql-interactive-mode . custom-sql-interactive-hook)
  :config
  ;; ;; turn on abbreviation mode
  ;; (abbrev-mode 1)

  ;; remove trailing blanks
  ;;(add-hook 'sql-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'sql-mode-hook #'install-remove-tabs)

  ;; remove prompt
  (defun sql-remove-prompt (output)
    "Remove prompt so output lines up correctly."
    (save-mark-and-excursion
      (let ((inhibit-read-only t))
        (forward-line 0)
        (while (and (not (bobp))
                    (not (looking-at "^GO$")))
          (while (re-search-forward (concat sql-prompt-regexp "[ \t]*") (point-at-eol) :noerror)
            (replace-match ""))
          (forward-line -1))))
    output)

  ;; custom hook
  (defun custom-sql-interactive-hook ()
    ;; turn off line wrapping
    (toggle-truncate-lines 1)

    ;; turn on sql history
    (set (make-local-variable 'sql-input-ring-file-name)
         (expand-file-name "history-sql" user-emacs-directory))

    ;; remove prompt on output
    (add-hook 'comint-preoutput-filter-functions #'sql-remove-prompt)))

;;------------------------------------------------------------------------------
;;;; sqlup-mode
;;------------------------------------------------------------------------------

(use-package sqlup-mode
  :straight t
  :after (sql)
  :bind (:map sql-mode-map
              ("C-c b" . sqlup-capitalize-keywords-in-buffer)
              ("C-c r" . sqlup-capitalize-keywords-in-region))
  :bind (:map sql-interactive-mode-map
              ("C-c b" . sqlup-capitalize-keywords-in-buffer)
              ("C-c r" . sqlup-capitalize-keywords-in-region))
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

;;------------------------------------------------------------------------------
;;;; sql-transform
;;------------------------------------------------------------------------------

(use-package sql-transform
  :straight t
  :after (sql)
  :bind (:map sql-mode-map
              ("C-c s" . sql-to-select)
              ("C-c i" . sql-to-insert)
              ("C-c u" . sql-to-update)
              ("C-c d" . sql-to-delete))
  :bind (:map sql-interactive-mode-map
              ("C-c s" . sql-to-select)
              ("C-c i" . sql-to-insert)
              ("C-c u" . sql-to-update)
              ("C-c d" . sql-to-delete)))

;; ;;------------------------------------------------------------------------------
;; ;;;; mysql
;; ;;------------------------------------------------------------------------------

;; (init-message 3 "mysql")

;; (use-package mysql
;;   :straight t
;;   :after (sql)
;;   :config (setq sql-product 'mysql))
;; SQL Mode:1 ends here

;; [[file:init-emacs.org::#modes-text-mode][Text Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Text Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Text Mode")

(use-package text-mode
  :straight (:type built-in)
  :after (flyspell)
  :mode (("\\.txt\\'" . text-mode)
         ("\\.text\\'" . text-mode)
         ("\\'README\\'" . text-mode)
         ("\\'INSTALL\\'" . text-mode)
         ("\\'CHANGELOG\\'" . text-mode))
  :config
  (defun custom-text-mode-hook ()
    ;; set tab
    ;;(setq tab-width 4)
    ;;(setq tab-stop-list (number-sequence 4 76 4))
    (setq tab-width 8)
    (setq tab-stop-list (number-sequence 8 76 8))
    ;;(setq indent-tabs-mode t)           ; can insert TAB characters
    ;;(bind-key "<tab>" 'indent-relative text-mode-map)

    ;; set default fill column for auto-fill mode
    (setq fill-column custom-fill-column)

    ;; turn on word wrap
    (turn-on-auto-fill)

    ;; add underscore and dash to word boundaries
    (modify-syntax-entry ?_ "w" text-mode-syntax-table)
    (modify-syntax-entry ?- "w" text-mode-syntax-table)

    ;; turn on flyspell
    (flyspell-mode 1)

    ;; ;; turn on abbreviation mode
    ;; (abbrev-mode 1)

    ;; turn on pabbrev mode
    ;;(pabbrev-mode)

    ;; insert one space after a sentence
    (setq sentence-end-double-space nil)

    ;; insert one space after a colon
    (setq colon-double-space nil)

    ;; ;; insert two spaces after a sentence
    ;; (setq sentence-end-double-space t)

    ;; ;; insert two spaces after a colon
    ;; (setq colon-double-space t)
    )
  (add-hook 'text-mode-hook #'custom-text-mode-hook)

  ;; remove trailing blanks
  ;;(add-hook 'text-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'text-mode-hook #'install-remove-tabs)
  )
;; Text Mode:1 ends here

;; [[file:init-emacs.org::#modes-typescript-mode][TypeScript Mode:2]]
;;------------------------------------------------------------------------------
;;; Modes: TypeScript Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: TypeScript Mode")

(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; tide for typescript
(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
   :config
   (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

;; Add node_modules to PATH
(use-package add-node-modules-path
  :straight t
  :hook (typescript-mode . add-node-modules-path))
;; TypeScript Mode:2 ends here

;; [[file:init-emacs.org::#modes-v-mode][V Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: V Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: V Mode")

(use-package v-mode
  :straight (v-mode
             :type git :host github :repo "damon-kwok/v-mode"
             :files ("tokens" "v-mode.el"))
  :after (flyspell)
  :mode ("\\.v?v\\.vsh\\'" . v-mode)
  :bind (:map v-mode-map
              ("C-c C-b" . v-project-build)
              ("C-c C-c" . v-project-run)
              ("C-c C-f" . v-format-buffer)
              ("C-c C-f" . v-menu))
  :config
  (defun custom-v-mode-hook ()
    ;; use spaces for tabs
    (setq indent-tabs-mode nil)

    ;; set indent level
    ;;(setq v-indent-level 4)

    ;; format code on save
    ;;(setq v-format-on-save t)

    ;; ;; define keys
    ;; (define-key v-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
    ;; ;; undefine electric keys
    ;; (define-key v-mode-map (kbd "{") 'self-insert-command)
    ;; (define-key v-mode-map (kbd "}") 'self-insert-command)

    ;; turn on flyspell
    (when (boundp 'flyspell-prog-mode)
      (flyspell-prog-mode)))
  (add-hook 'v-mode-hook #'custom-v-mode-hook :append)

  ;; turn on flyspell
  (flyspell-prog-mode)

  ;; FIXME: No longer works
  ;; (use-package flymake
  ;;   :straight (:type built-in)
  ;;   :config (progn
  ;;             (defun flymake-v-init ()
  ;;               (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;                      (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;                 (list "v" (list "-c" local-file))))

  ;;             (defun flymake-v-enable ()
  ;;               (when (and buffer-file-name
  ;;                          (file-writable-p (file-name-directory buffer-file-name))
  ;;                          (file-writable-p buffer-file-name)
  ;;                          (if (fboundp 'tramp-list-remote-buffers)
  ;;                              (not (cl-subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
  ;;                            t))
  ;;                 (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  ;;                 (flymake-mode t)))

  ;;             (add-to-list 'flymake-allowed-file-name-masks '(".+\\.rb\\'" flymake-v-init) t)
  ;;             (add-to-list 'flymake-allowed-file-name-masks '("Rakefile\\'" flymake-v-init) t)
  ;;             (add-to-list 'flymake-err-line-patterns '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) t)
  ;;             (add-hook 'v-mode-hook #'flymake-v-enable)))

  ;; ;; turn on syntax highlighting (actually turns off syntax highlighting)
  ;; (add-hook 'v-mode-hook #'turn-on-font-lock)

  ;; ;; turn on abbreviation mode
  ;; (abbrev-mode 1)

  (defun v-mode-maybe ()
    "Determine if file is a v script and switch to `v-mode' if it is."
    (interactive)
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (when (or
               (search-forward "#!/usr/bin/v" (line-end-position) :noerror)
               (search-forward "#!/usr/bin/env v" (line-end-position) :noerror))
          (v-mode)))))

  ;; run when a file is loaded
  (add-hook 'find-file-hook #'v-mode-maybe)

  ;; remove trailing blanks
  ;;(add-hook 'v-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'v-mode-hook #'install-remove-tabs)
  )
;; V Mode:1 ends here

;; [[file:init-emacs.org::#modes-vimrc-mode][Vimrc Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: Vimrc Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: Vimrc Mode")

(use-package vimrc-mode
  :straight t
  :mode (("\\.vimrc\\'" . vimrc-mode)
         ("\\.vim\\'" . vimrc-mode)
         ("\\.exrc\\'" . vimrc-mode)))
;; Vimrc Mode:1 ends here

;; [[file:init-emacs.org::#modes-xml-mode][XML Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: XML Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: XML Mode")

(use-package nxml-mode
  :straight (:type built-in)
  :after (flyspell)
  :mode (("\\.dtd\\'" . nxml-mode)
         ("\\.htm\\'" . nxml-mode)
         ("\\.html\\'" . nxml-mode)
         ("\\.rdf\\'" . nxml-mode)
         ("\\.rhtml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode)
         ("\\.sgml\\'" . nxml-mode)
         ("\\.svg\\'" . nxml-mode)
         ("\\.xhtml\\'" . nxml-mode)
         ("\\.xml\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode)
         ("\\.xsl\\'" . nxml-mode)
         ("\\.tt\\'" . nxml-mode))
  :config
  ;; set magic modes
  ;;(load "rng-auto")
  ;;(unify-8859-on-decoding-mode)
  (add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode) t)

  (defun custom-nxml-mode-hook ()
    ;; do not use `indent-relative' for tab indenting
    (bind-key "<tab>" 'indent-for-tab-command nxml-mode-map)

    ;; turn off auto-fill mode
    (turn-off-auto-fill)

    ;; turn on flyspell
    (flyspell-prog-mode)

    ;; turn on auto-completion
    (setq nxml-slash-auto-complete-flag t)

    ;; turn on org minor mode
    ;; (when (string-match "\\.\\(x?html\\|php[34]?\\)\\'"
    ;;                     (file-name-sans-versions buffer-file-name))
    ;;   (custom-nxml-mode-org))
    ;;(custom-nxml-mode-org)

    ;; set outline header regexp
    ;;(setq-local outline-regexp " *<[^/]")
    (setq-local outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
    ;;(hide-sublevels 1)
    )
  (add-hook 'nxml-mode-hook #'custom-nxml-mode-hook)

  ;; (defun custom-nxml-mode-org ()
  ;;   ;;(setq-local outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
  ;;   (setq-local outline-regexp "\\s *<")
  ;;   (setq-local outline-level 'custom-nxml-mode-outline-level)
  ;;   (outline-minor-mode 1)
  ;;   (hs-minor-mode 1))

  ;; (defun custom-nxml-mode-outline-level ()
  ;;   (save-mark-and-excursion
  ;;     (save-match-data
  ;;       (re-search-forward html-outline-level)
  ;;       (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
  ;;         (if (eq (length tag) 2)
  ;;             (- (aref tag 1) ?0)
  ;;           0)))))

  ;; (add-to-list 'hs-special-modes-alist
  ;;              '(nxml-mode
  ;;                "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
  ;;                ""
  ;;                "<!--" ;; won't work on its own; uses syntax table
  ;;                (lambda (arg) (custom-nxml-mode-forward-element))
  ;;                nil) t)

  ;; (defun custom-nxml-mode-forward-element ()
  ;;   (let ((nxml-sexp-element-flag))
  ;;     (setq nxml-sexp-element-flag (not (looking-at "<!--")))
  ;;     (unless (looking-at outline-regexp)
  ;;       (ignore-errors
  ;;         (nxml-forward-balanced-item 1)))))

  ;; remove trailing blanks
  ;;(add-hook 'nxml-mode-hook #'install-remove-trailing-blanks)

  ;; remove tabs
  ;;(add-hook 'nxml-mode-hook #'install-remove-tabs)

  ;; (use-package flymake
  ;;   :straight (:type built-in)
  ;;   :config (progn
  ;;             (defun flymake-html-init ()
  ;;               (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;                      (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;                 (list "tidy" (list local-file))))

  ;;             (add-to-list 'flymake-allowed-file-name-masks '("\\.html$\\|\\.ctp" flymake-html-init) t)
  ;;             (add-to-list 'flymake-err-line-patterns '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)" nil 1 2 4) t)))

  ;; derive xml-mode and html-mode from nxml-mode
  (fset 'xml-mode 'nxml-mode)
  (fset 'html-mode 'nxml-mode))
;; XML Mode:1 ends here

;; [[file:init-emacs.org::#modes-yaml-mode][YAML Mode:1]]
;;------------------------------------------------------------------------------
;;; Modes: YAML Mode
;;------------------------------------------------------------------------------

(init-message 2 "Modes: YAML Mode")

(use-package yaml-mode
  :straight t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))
;; YAML Mode:1 ends here

;; [[file:init-emacs.org::#menus][Menus:1]]
;;==============================================================================
;;; Menus
;;==============================================================================

(init-message 1 "Menus")
;; Menus:1 ends here

;; [[file:init-emacs.org::#menus-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Menus: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Configuration")
;; Configuration:1 ends here

;; [[file:init-emacs.org::#menus-setup-easy-menu][Easy Menu:1]]
;;------------------------------------------------------------------------------
;;;; Menus: Setup: Easy Menu
;;------------------------------------------------------------------------------

(init-message 3 "Menus: Setup: Easy Menu")

;; easymenu
(use-package easymenu
  :straight (:type built-in))
;; Easy Menu:1 ends here

;; [[file:init-emacs.org::#menus-setup-auto-menu][Auto-Menu:1]]
;;------------------------------------------------------------------------------
;;;; Menus: Setup: Auto-Menu
;;------------------------------------------------------------------------------

(init-message 3 "Menus: Setup: Auto-Menu")

;; auto-menu
(use-package auto-menu
  :load-path (lambda () (file-truename (expand-file-name "auto-menu.el" local-modules-dir)))
  :commands (auto-menu
             auto-menu-dired
             auto-menu-file
             auto-menu-file-dir))
;; Auto-Menu:1 ends here

;; [[file:init-emacs.org::#menus-setup-find-or-browse-file][Find or Browse File:1]]
;;------------------------------------------------------------------------------
;;;; Menus: Setup: Find or Browse File
;;------------------------------------------------------------------------------

(init-message 3 "Menus: Setup: Find or Browse File")

(defun find-or-browse-file (file)
  "Based on file type either open FILE or browse FILE."
  (let ((file (file-truename (expand-file-name file))))
    (if (string= (file-name-extension file) "html")
        (browse-url (concat "file://" file))
      (find-file file))))
;; Find or Browse File:1 ends here

;; [[file:init-emacs.org::#menus-buffer-switch-menu][Buffer-Switch Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Buffer-Switch Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Buffer-Switch Menu")

;; buffer-switch menu
(auto-menu
 "Buffer-Switch"
 `(("*scratch*" "(switch-to-buffer \"*scratch*\")" "Switch to '*scratch*' buffer.")
   ("New *scratch*" "(switch-to-buffer (generate-new-buffer-name \"*scratch*\"))" "Create and switch to a '*scratch*' buffer.")
   ("Current Mode *scratch*" "(switch-to-scratch-for-current-mode)" "Switch to '*scratch-MODE*' buffer.")
   ("Emacs Lisp Mode *scratch*" "(new-emacs-lisp-scratch :use-existing)" "Switch to '*scratch-emacs-lisp-mode*' buffer.")
   ("New Emacs Lisp Mode *scratch*" "(new-emacs-lisp-scratch)" "Create and switch to '*scratch-emacs-lisp-mode*' buffer.")
   ("*messages*" "(switch-to-buffer \"*Messages*\")" "Switch to '*Messages*' buffer.")))
;; Buffer-Switch Menu:1 ends here

;; [[file:init-emacs.org::#menus-dired-menu][Dired Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Dired Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Dired Menu")

;; dired menu
(auto-menu
 "Dired"
 ;;(append '(("recent" "context-dired" "Run `cdired' to list recent files.")) ; no longer installed
 (auto-menu-dired `(("home" . "~/")
                    ,(cons "emacs" emacs-home-dir)
                    ;;,(cons "emacs-modules" (concat emacs-home-dir "/modules"))
                    ;;,(cons "emacs-mode-abbrevs" (concat emacs-home-dir "/mode-abbrevs"))
                    ;;,(cons "emacs-init" local-init-dir)
                    ;;,(cons "emacs-modules" local-modules-dir)
                    ;;,(cons "emacs-work-modules" local-work-modules-dir)
                    ("config" . "~/config")
                    ("config-private" . "~/config-private")
                    ("bin" . "~/bin")
                    ("org" . "~/org")
                    ("web" . "~/web")
                    ("web/org" . "~/web/org")
                    ("public_html" . "~/public_html")
                    ;;("plans" . "~/plans")
                    ("reminders" . "~/reminders")
                    ;;("wiki" . "~/wiki")
                    ("doc" . "~/doc")
                    ("bbs" . "~/doc/bbs")
                    ("dev" . "~/dev")
                    ("prj" . "~/prj")
                    ("code" . "~/code")
                    ("github" . "~/code/github-nullman")
                    ;;("gitlab" . "~/code/gitlab-kylesherman")
                    ("media" . "/home/data/media")
                    ("music" . "/home/data/media/audio/Music")
                    ("text" . "/home/data/media/text")
                    ("softwre" . "/home/data/media/software")
                    ("repos" . "/home/data/media/repos")
                    ("Downloads" . "~/Downloads")
                    ("Documents" . "~/Documents")
                    ("clisp" . "~/dev/clisp")
                    ;;("clojure" . "~/dev/clojure")
                    ("racket" . "~/dev/racket")
                    ("erlang" . "~/dev/erlang")
                    ("basic" . "~/dev/basic")
                    ("java" . "~/dev/java")
                    ("javascript" . "~/dev/javascript")
                    ("kotlin" . "~/dev/kotlin")
                    ,(cons "emacs-help" (concat emacs-home-dir "/help")))))
;;)
;; Dired Menu:1 ends here

;; [[file:init-emacs.org::#menus-load-menu][Load Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Load Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Load Menu")

;; load menu
(auto-menu
 "Load"
 `(
   ;;("Restore Context" "context-restore" "Restore previous context save.")
   ("Home Files..."
    ,(auto-menu-file '((".alias" . "~/.alias")
                       (".alias-local" . "~/.alias-local")
                       (".alias-work" . "~/.alias-work")
                       (".aspell.en.pws" . "~/.aspell.en.pws")
                       (".bashrc" . "~/.bashrc")
                       (".funct" . "~/.funct")
                       (".funct-local" . "~/.funct-local")
                       (".funct-work" . "~/.funct-work")
                       (".gopherus.bookmarks" . "~/.gopherus.bookmarks")
                       (".profile" . "~/.profile")
                       (".profile-local" . "~/.profile-local")
                       (".profile-work" . "~/.profile-work")
                       (".profile_run" . "~/.profile_run")
                       (".xbindkeysrc" . "~/.xbindkeysrc"))))
   ("Emacs Initialization..."
    ,(auto-menu-file `(("init-emacs.org" . ,(file-truename (expand-file-name "init-emacs.org" emacs-home-dir)))
                       ("customization.el" . ,(file-truename (expand-file-name "customization.el" emacs-home-dir))))))
   ;; ("Emacs Initialization..."
   ;;  ,(auto-menu-file-dir local-init-dir "\\.el\\'" "find-file"))
   ("Emacs Personal Modules..."
    ,(auto-menu-file-dir local-modules-dir "\\.el\\'" "find-file" t))
   ("Bin Files..."
    ;;,(auto-menu-file-dir "~/bin" nil "find-file" t))
    ,(auto-menu-file '(("get-emacs-modules" . "~/bin/get-emacs-modules"))))
   ("Web Org Files..."
    ,(auto-menu-file-dir "~/web/org" "\\.org\\'" "find-file" t))
   ("Org Files..."
    ,(cl-remove-if (lambda (x) (string-prefix-p "agenda-" (car x)))
                   (auto-menu-file-dir "~/org" "\\.\\(org\\|org\\.gpg\\)\\'" "find-file" t)))
   ("Agenda Files..."
    ,(cl-remove-if (lambda (x) (not (string-prefix-p "agenda-" (car x))))
                   (auto-menu-file-dir "~/org" "\\.\\(org\\|org\\.gpg\\)\\'" "find-file" t)))
   ("Bookmarks" "load-bookmarks" "Load `~/lynx_bookmarks.html' file.")
   ("Emacs Work Modules..."
    ,(auto-menu-file-dir local-work-modules-dir "\\.el\\'" "find-file" t))
   ("Clojure Files..."
    ,(auto-menu-file-dir "~/dev/clojure" "\\.clj\\'" "find-file" t))
   ("CLisp Files..."
    ,(auto-menu-file-dir "~/dev/clisp" "\\.lisp\\'" "find-file" t))
   ("Erlang Files..."
    ,(auto-menu-file-dir "~/dev/erlang" "\\.erl\\'" "find-file" t))
   ("BASIC Files..."
    ,(auto-menu-file-dir "~/dev/basic" "\\.bas\\'" "find-file" t))
   ("Javascript Files..."
    ,(auto-menu-file-dir "~/dev/javascript" "\\.js\\'" "find-file" t))
   ))
;; Load Menu:1 ends here

;; [[file:init-emacs.org::#menus-application-menu][Application Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Application Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Application Menu")

;; applications menu
(auto-menu
 "Applications"
 `(("Bookmarks" "load-bookmarks" "Load bookmarks.")
   ("Calc" "calc" "Run Calc (The Emacs Calculator).")
   ("Elfeed" "elfeed" "Run Elfeed (Emacs Atom/RSS feed reader).")
   ("Elpher" "elpher" "Run Elpher (Emacs Gopher Client).")
   ("ERC" "erc" "Run ERC (Emacs Internet Relay Chat client).")
   ("Gnus" "gnus" "Run Gnus (Newsreader)")
   ("Mastodon" "mastodon" "Run Mastodon (Social Client)")))

;; (when (fboundp 'term-ansi)

;;   (init-message 2 "Applications Menu")

;;   ;; applications menu
;;   (auto-menu
;;    "Applications"
;;    `(("ispell" "(term-ansi \"ispell\" \"ispell\" \"-a\")" "Run ispell spell checker.")
;;      ("mutt" "(term-ansi \"mutt\" \"mutt\")" "Run mutt email client.")
;;      ("pine" "(term-ansi \"pine\" \"pine\")" "Run pine email client.")
;;      ("centerim" "(term-ansi \"centerim\" \"centerim\" \"-a\")" "Run centerim instant messaging client.")
;;      ("tin" "(term-ansi \"tin\" \"tin\")" "Run tin news client.")
;;      ("slrn" "(term-ansi \"slrn\" \"slrn\")" "Run slrn news client.")
;;      ("irssi" "(term-ansi \"irssi\" \"irssi\")" "Run irssi irc client.")
;;      ("snownews" "(term-ansi \"snownews\" \"snownews\")" "Run snownews rss client.")
;;      ("lynx" "(term-ansi \"lynx\" \"lynx\" \"-book -useragent=\\\"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)\\\"\")" "Run lynx web browser.")
;;      ("orpie" "(term-ansi \"orpie\" \"orpie\")" "Run orpie calculator.")
;;      ("linuxtrade" "(term-ansi \"linuxtrade\" \"linuxtrade\")" "Run linuxtrade stock client.")
;;      )))
;; Application Menu:1 ends here

;; [[file:init-emacs.org::#menus-run-file-menu][Run-File Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Run-File Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Run-File Menu")

;; run-file menu
(auto-menu
 "Run-File"
 `(("init.el" ,(concat "(safe-load \"" (file-truename (expand-file-name "init.el" emacs-home-dir)) "\")") "Run init.el to re-initialize Emacs.")
   ;; ("Emacs Initialization..."
   ;;  ,(auto-menu-file `(("init.el" ,(concat "(safe-load \"" (file-truename (expand-file-name "init.el" emacs-home-dir)) "\")") "Run init.el to re-initialize Emacs.")
   ;;                     ("init-emacs.el" ,(concat "(safe-load \"" (file-truename (expand-file-name "init-emacs.el" emacs-home-dir)) "\")") "Run init-emacs.el to re-initialize Emacs."))))
   ;; ("Emacs Initialization..."
   ;;  ,(auto-menu-file-dir local-init-dir "\\.el\\'" "safe-load-compile"))
   ("Emacs Personal Modules..."
    ,(auto-menu-file-dir local-modules-dir "\\.el\\'" "safe-load-compile" t))
   ("Clojure Files..."
    ,(auto-menu-file-dir "~/dev/clojure" "\\.clj\\'" "slime-load-file" t))
   ("CLisp Files..."
    ,(auto-menu-file-dir "~/dev/clisp" "\\.lisp\\'" "slime-load-file" t))
   ))
;; Run-File Menu:1 ends here

;; [[file:init-emacs.org::#menus-website-menu][Website Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Website Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Website Menu")

;; website menu
(auto-menu
 "Website"
 `(("Blog"
    (("Blog Post" "org-website-blog-post-create" "Create a new blog post.")
     ("Update Blog Post Timestamp" "org-website-blog-post-update-posted" "Update blog post timestamp.")))
   ("Publish"
    (("Publish" "org-website-publish-async" "Publish entire website.")
     ("Force Publish" "(org-website-publish-async nil t)" "Force publish entire website.")
     ("nullman" "(org-website-publish-async \"nullman\")" "Publish nullman website.")
     ("nullman (Force)" "(org-website-publish-async \"nullman\" t)" "Force publish nullman website.")
     ("nulldot" "(org-website-publish-async \"nulldot\")" "Publish nulldot website.")
     ("nulldot (Force)" "(org-website-publish-async \"nulldot\" t)" "Force publish nulldot website.")
     ("nullware" "(org-website-publish-async \"nullware\")" "Publish nullware website.")
     ("nullware (Force)" "(org-website-publish-async \"nullware\" t)" "Force publish nullware website.")
     ("kylesherman" "(org-website-publish-async \"kylesherman\")" "Publish kylesherman website.")
     ("kylesherman (Force)" "(org-website-publish-async \"kylesherman\" t)" "Force publish kylesherman website.")
     ("shermanwest" "(org-website-publish-async \"shermanwest\")" "Publish shermanwest website.")
     ("shermanwest (Force)" "(org-website-publish-async \"shermanwest\" t)" "Force publish shermanwest website.")))
   ("Tangle and Publish"
    (("All" "org-website-tangle-publish-async" "Tangle and publish entire website.")
     ("Force All" "(org-website-tangle-publish-async nil t)" "Force tangle and publish entire website asynchronously.")
     ("nullman" "(org-website-tangle-publish-async \"nullman\")" "Tangle and publish nullman website.")
     ("nullman (Force)" "(org-website-tangle-publish-async \"nullman\" t)" "Force tangle and publish nullman website.")
     ("nulldot" "(org-website-tangle-publish-async \"nulldot\")" "Tangle and publish nulldot website.")
     ("nulldot (Force)" "(org-website-tangle-publish-async \"nulldot\" t)" "Force tangle and publish nulldot website.")
     ("nullware" "(org-website-tangle-publish-async \"nullware\")" "Tangle and publish nullware website.")
     ("nullware (Force)" "(org-website-tangle-publish-async \"nullware\" t)" "Force tangle and publish nullware website.")
     ("kylesherman" "(org-website-tangle-publish-async \"kylesherman\")" "Tangle and publish kylesherman website.")
     ("kylesherman (Force)" "(org-website-tangle-publish-async \"kylesherman\" t)" "Force tangle and publish kylesherman website.")
     ("shermanwest" "(org-website-tangle-publish-async \"shermanwest\")" "Tangle and publish shermanwest website.")
     ("shermanwest (Force)" "(org-website-tangle-publish-async \"shermanwest\" t)" "Force tangle and publish shermanwest website.")))
   ("Remote Synchronization"
    (;;("Website to localhost" "org-website-rsync-to-localhost" "Rsync website to localhost server.")
     ("Website to Morpheus" "org-website-rsync-to-morpheus-async" "Rsync website to morpheus server.")
     ("Website to DigitalOcean" "org-website-rsync-to-digitalocean-async" "Rsync website to DigitalOcean server.")
     ("Powerhouse to DigitalOcean" "(org-website-rsync-to-digitalocean-async \"powerhouse\")" "Rsync Powerhouse to DigitalOcean server.")
     ("Bloodmoon to DigitalOcean" "(org-website-rsync-to-digitalocean-async \"bloodmoon\")" "Rsync Bloodmoon to DigitalOcean server.")))
   ))
;; Website Menu:1 ends here

;; [[file:init-emacs.org::#menus-package-manager-menu][Package Manager Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Package Manager Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Package Manager Menu")

;; package manager menu
(auto-menu
 "Package Manager"
 `(("List Packages" "(package-list-packages :nofetch)" "List packages.")
   ("List Packages (Refresh)" "package-list-packages" "List packages (after refresh).")
   ("Install Package" "package-install" "Install package.")
   ("Refresh Recipes (straight)" "straight-pull-recipe-repositories" "Refresh recipe repositories.")
   ("Pull All (straight)" "straight-pull-all" "Pull all packages.")
   ("Remove Unused (straight)" "(straight-remove-unused-repos t)" "Remove unused recipies.")
   ;;("Straight Fetch All" "straight-fetch-all" "Fetch all packages.")
   ;;("Auto-Remove Packages" "package-autoremove" "Remove packages that are no more needed.")
   ;;("Quelpa Upgrade" "quelpa-upgrade" "Upgrade one Quelpa package.")
   ;;("Quelpa Upgrade All" "quelpa-upgrade-all" "Upgrade all Quelpa packages.")
   ;; ("Update Package List" "package-list-update" "Update `package-list' from currently installed packages.")
   ;; ("Install Missing Packages" "package-list-install-missing" "Install all missing packages in `package-list'.")
   ;; ("List Unaccounted Packages" "package-list-unaccounted-packages" "List installed ELPA packages that are not in `package-list'.")
   ;;("Compile Packages" "(byte-recompile-directory emacs-package-dir 0)" "Byte-compile ELPA packages.")
   ))
;; Package Manager Menu:1 ends here

;; [[file:init-emacs.org::#menus-miscellaneous-menu][Miscellaneous Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Miscellaneous Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Miscellaneous Menu")

;; bbs directory
(setq bbs-directory (file-truename (expand-file-name "~/doc/bbs")))

;; miscellaneous menu
(auto-menu
 "Misc"
 `(("BBS"
    (("Level 29 BBS Fetch"
      "(progn (find-file (file-truename (expand-file-name \"level-29-bbs.org\" bbs-directory))) (unless (fboundp 'l29-fetch-messages-new) (org-babel-execute-buffer)) (l29-fetch-messages-new))"
      "Run `l29-fetch-messages-new' to fetch new messages from the Level 29 BBS into level-29-bbs.org.")
     ("House of Lunduke BBS Fetch"
      "(progn (find-file (file-truename (expand-file-name \"house-of-lunduke-bbs.org\" bbs-directory))) (unless (fboundp 'lunduke-fetch-messages-new) (org-babel-execute-buffer)) (lunduke-fetch-messages-new))"
      "Run `lunduke-fetch-messages-new' to fetch new messages from the House of Lunduke BBS into house-of-lunduke-bbs.org.")))
   ("Byte-Compile"
    (("Generate `init-emacs.el'"
      "(org-babel-generate-elisp-file (file-truename (expand-file-name \"init-emacs.org\" emacs-home-dir)))"
      "Run `org-babel-generate-elisp-file' on init-emacs.org to produce init-emacs.el.")
     ("Generate and Byte-Compile `init-emacs.el'"
      "(org-babel-generate-elisp-file (file-truename (expand-file-name \"init-emacs.org\" emacs-home-dir)) t t)"
      "Run `org-babel-generate-elisp-file' on init-emacs.org to produce init-emacs.el and init-emacs.elc.")
     ,(list (concat "Compile Personal Modules Directory")
            (concat "(compile-elisp \"" local-modules-dir "\")")
            (concat "Byte-Compile `" local-modules-dir "' directory."))
     ;; ,(list (concat "Compile Emacs Modules Directory")
     ;;        (concat "(compile-elisp \"" emacs-modules-dir "\")")
     ;;        (concat "Byte-Compile `" emacs-modules-dir "' directory."))
     ))
   ("Command Log"
    (("Command Log Mode ON" "command-log-mode-on" "Turn on ‘command-log-mode’ and open the log buffer.")
     ("Command Log Mode OFF" "command-log-mode-off" "Turn off ‘command-log-mode’ and close the log buffer.")
     ("Clear Command Log Buffer" "clm/command-log-clear" "Clear the command log buffer.")))
   ("Coding System"
    (("UNIX Coding System" "(set-coding-system 'unix)" "Call `set-coding-system' to set the coding system to UNIX.")
     ("DOS Coding System" "(set-coding-system 'dos)" "Call `set-coding-system' to set the coding system to DOS.")
     ("Mac Coding System" "(set-coding-system 'mac)" "Call `set-coding-system' to set the coding system to Mac.")))
   ("Development"
    (("Racket REPL" "run-racket" "Start Racket REPL for interactively evaluating Racket expressions.")
     ("Kotlin REPL" "kotlin-repl" "Start Kotlin REPL for interactively evaluating Kotlin expressions.")
     ("Common Lisp SLIME Mode" "(slime 'clisp)" "Start SLIME mode for interactively evaluating Common Lisp expressions.")
     ("Steel Bank Common Lisp SLIME Mode" "(slime 'sbcl)" "Start SLIME mode for interactively evaluating Common Lisp expressions.")
     ("Clojure SLIME Mode" "(slime 'clojure)" "Start SLIME mode for interactively evaluating Clojure expressions.")
     ("Evaluate SLIME Buffer" "slime-eval-buffer" "Run `slime-eval-buffer' on the current buffer.")
     ("Python REPL" "elpy-shell-switch-to-shell" "Start Python REPL for interactively evaluating Python expressions.")))
   ("Display"
    (("World Time" "display-time-world" "Display the time in various time zones.")
     ("Colors Display" "list-colors-display" "List Emacs font colors.")
     ("Faces Display" "list-faces-display" "List Emacs font faces.")
     ("Character Sets Display" "list-character-sets" "List character sets.")
     ("Unicode Character Set Display" "list-charset-unicode" "List `unicode-bmp' character set.")))
   ("Edit"
    (("Replacer Replacements" "replacer-replacements-edit" "Edit `replacer-replacements'.")
     ("Elfeed Boookmarks" "elfeed-bookmarks-edit" "Edit Elfeeds bookmarks file.")
     ("Elpher Bookmarks" "elpher-bookmarks-edit" "Edit Elpher bookmarks file.")
     ("YouTube Get Videos" "(org-link-open-from-string \"file:~/config-private/common/org/init-home.org::get-youtube-videos\")" "Edit get-youtube-videos file.")))
   ("Export"
    (("Export Bookmarks to JSON" "(org-bookmarks-export-to-json \"~/org/bookmarks.org\" \"~/Desktop/bookmarks.json\")" "Export bookmarks.org to ~/Desktop/bookmarks.json.")
     ("Export Bookmarks to HTML" "(org-bookmarks-export-to-html \"~/org/bookmarks.org\" \"~/Desktop/bookmarks.html\")" "Export bookmarks.org to ~/Desktop/bookmarks.html.")
     ("Export Bookmarks to NYXT" "(org-bookmarks-export-to-nyxt \"~/org/bookmarks.org\" \"~/config/local/.local/share/nyxt/bookmarks.lisp\")" "Export bookmarks.org to ~/config/local/.local/share/nyxt/bookmarks.lisp.")))
   ("Fonts"
    (("Hack Nerd Font Mono-12" "(set-frame-font \"Hack Nerd Font Mono-12\" nil t)" "Call `set-frame-font` to set the font to 'Hack Nerd Font Mono-12'.")
     ("Hack Nerd Font Mono-14" "(set-frame-font \"Hack Nerd Font Mono-14\" nil t)" "Call `set-frame-font` to set the font to 'Hack Nerd Font Mono-14'.")
     ("BitstreamVeraSansMono Nerd Font Mono-12" "(set-frame-font \"BitstreamVeraSansMono Nerd Font Mono-12\" nil t)" "Call `set-frame-font` to set the font to 'BitstreamVeraSansMono Nerd Font Mono-12'.")
     ("BitstreamVeraSansMono Nerd Font Mono-14" "(set-frame-font \"BitstreamVeraSansMono Nerd Font Mono-14\" nil t)" "Call `set-frame-font` to set the font to 'BitstreamVeraSansMono Nerd Font Mono-14'.")
     ("DroidSansMono Nerd Font Mono-12" "(set-frame-font \"DroidSansMono Nerd Font Mono-12\" nil t)" "Call `set-frame-font` to set the font to 'DroidSansMono Nerd Font Mono-12'.")
     ("DroidSansMono Nerd Font Mono-14" "(set-frame-font \"DroidSansMono Nerd Font Mono-14\" nil t)" "Call `set-frame-font` to set the font to 'DroidSansMono Nerd Font Mono-14'.")
     ("9x15" "(set-frame-font \"9x15\" nil t)" "Call `set-frame-font` to set the font to '9x15'.")))
   ("Reformat"
    (("JSON Reformat" "json-pretty-print" "Reformat (pretty-print) JSON in current buffer.")
     ("XML Reformat" "xml-pretty-print" "Reformat (pretty-print) XML in current buffer.")
     ("Java Reformat" "java-pretty-print" "Reformat (pretty-print) Java code in current buffer.")
     ("Ruby Reformat" "ruby-pretty-print" "Reformat (pretty-print) Ruby code in current buffer.")
     ("C Reformat" "c-pretty-print" "Reformat (pretty-print) C code in current buffer.")))
   ("Toggle"
    (("Debug on Error Mode" "toggle-debug-on-error" "Toggle `debug-on-error'.")
     ("Debug on Quit Mode" "toggle-debug-on-quit" "Toggle `debug-on-quit'.")
     ("Truncate Line Mode" "toggle-truncate-lines" "Toggle `truncate-lines' in current buffer.")
     ("Visual Line Mode" "visual-line-mode" "Toggle `visual-line-mode' in current buffer.")
     ("Search Case Sensitivity" "toggle-case-fold-search" "Toggle case-fold-search in current buffer.")))
   ("TAGS"
    (("Visit Local TAGS" "(when (find-file-updir \"TAGS\") (visit-tags-table (find-file-updir \"TAGS\") t))" "Visit local tags table.")
     ("Create Local TAGS" "etags-create" "Create local tags table.")))
   ("Revert Buffer" "revert-buffer" "Run `revert-buffer' on current buffer.")
   ("Git Status" "(magit-status default-directory)" "Open Git Status buffer.")
   ("Web Jump" "webjump" "Jump to a Web site from a programmable hotlist.")
   ("IELM Mode" "ielm" "Open buffer for interactively evaluating Emacs Lisp expressions.")
   ("Evaluate Buffer" "eval-buffer" "Run `eval-buffer' on the current buffer.")
   ("Customize Group" "customize-group" "Run `customize-group' function.")
   ("Regular Expression Builder" "regexp-builder" "Start Regular Expression Builder in current buffer.")
   ("Restart Emacs Server" "server-start-maybe" "Restart Emacs server.")
   ))
;;("Tip of the Day" "totd" "Display a random emacs command.")
;;("Visit Local TAGS" "(when (file-exists-p \"TAGS\") (visit-tags-table \"TAGS\" t))" "Visit local tags table.")
;;("Visit Home TAGS" "(when (file-exists-p \"~/TAGS\") (visit-tags-table \"~/TAGS\"))" "Visit home tags table.")
;;("MySQL" "sql-mysql" "Launch SQL-MySQL.")
;;("MS-SQL" "sql-ms" "Launch SQL-MS.")
;;("Common Lisp Doc Mode" "(add-hook 'lisp-mode-hook #'turn-on-cldoc-mode)" "Turn on auto docs in CLisp mode.")
;; Miscellaneous Menu:1 ends here

;; [[file:init-emacs.org::#menus-manuals-menu][Manuals Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Manuals Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Manuals Menu")

;; manual menu
(auto-menu
 "Manuals"
 `(("Help Files" ,(auto-menu-file-dir (concat emacs-home-dir "/help") ".*" "find-or-browse-file" t))
   ("Man Pages" "woman" "Browse man pages.")
   ("Emacs Manual" "(info \"emacs-24/emacs\")" "Open Emacs manual.")
   ("Elisp Manual" "(info \"emacs-24/elisp\")" "Open Elisp manual.")
   ("Org Mode Manual" "(info \"/usr/share/info/org.gz\")" "Open Org Mode manual.")
   ("Screen Manual" "(info \"screen\")" "Open Screen manual.")
   ("SED Manual" "(info \"sed\")" "Open SED manual.")
   ("Grep Manual" "(info \"grep\")" "Open Grep pattern matching manual.")
   ("DC Manual" "(info \"dc\")" "Open arbitrary precision RPN Desktop Calculator manual.")
   ("Wget Manual" "(info \"wget\")" "Open Wget manual.")))
;;("Help Files" ,(auto-menu-file-dir (concat emacs-home-dir "/help") ".*" "find-file" t))
;; Manuals Menu:1 ends here

;; [[file:init-emacs.org::#menus-web-menu][Web Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Web Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Web Menu")

;; web menu
(when (boundp 'web-query-list)
  (auto-menu
   "Web"
   (mapcar (lambda (x)
             (list (car x)
                   (concat "(web-query \"" (car x) "\")")
                   (concat "Query web for \\\"" (car x) "\\\".")))
           web-query-list)))
;; Web Menu:1 ends here

;; [[file:init-emacs.org::#menus-insert-menu][Insert Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Insert Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Insert Menu")

;; insert menu
(auto-menu
 "Insert"
 '(("Org-Babel Inserts"
    (("Name" "org-insert-literate-programming-name" "Insert #+NAME.")
     ("Block" "org-insert-literate-programming-block" "Insert block.")
     ("Emacs Init Block" "org-insert-literate-programming-init-emacs-block" "Insert Emacs Init block.")
     ("Code Block" "org-insert-literate-programming-code-block" "Insert Code block.")
     ("Project Euler Block" "org-insert-literate-programming-project-euler-problem-block" "Insert Project Euler block.")
     ("Source Block" "org-insert-literate-programming-src" "Insert #+BEGIN_SRC ... #+END_SRC block.")
     ("Shell Block" "org-insert-literate-programming-src-sh" "Insert #+BEGIN_SRC sh ... #+END_SRC block.")
     ("Emacs Lisp Source Block" "org-insert-literate-programming-src-emacs-lisp" "Insert #+BEGIN_SRC emacs-lisp ... #+END_SRC block.")
     ("Racket Source Block" "org-insert-literate-programming-src-racket" "Insert #+BEGIN_SRC racket ... #+END_SRC block.")
     ("Kotlin Source Block" "org-insert-literate-programming-src-kotlin" "Insert #+BEGIN_SRC kotlin ... #+END_SRC block.")))
   ("Lisp Inserts"
    (("Lisp Comment Block (Equal)" "insert-lisp-comment-block-equal" "Insert Lisp style comment block using equals.")
     ("Lisp Comment Block (Dash)" "insert-lisp-comment-block-dash" "Insert Lisp style comment block using dashes.")))
   ("C Inserts"
    (("C Comment Block" "insert-c-comment-block" "Insert C/C++/Java style comment block.")
     ("C Comment Stub" "insert-c-comment-stub" "Insert C/C++/Java style comment stub.")))
   ("Date YYYY-MM-DD" "insert-date" "Insert date in YYYY-MM-DD format.")
   ("Date/Time YYYY-MM-DD HH:MM:SS" "insert-datetime" "Insert date/time in YYYY-MM-DD HH:MM:SS format.")
   ("Time HH:MM:SS" "insert-time" "Insert time in HH:MM:SS format.")
   ("UUID" "insert-uuid" "Insert a UUID.")
   ("GUID" "insert-guid" "Insert a GUID.")
   ("Password" "insert-password-20" "Insert a random password (length 20).")
   ("Password Phrase" "insert-password-phrase-three-hyphen" "Insert a random password phrase (three words, hyphenated).")
   ("Figlet" "insert-figlet" "Insert figlet text.")
   ("Equals" "append-equal-to-column-80" "Append `=' characters up to column 80.")
   ("Dashes" "append-dash-to-column-80" "Append `-' characters up to column 80.")
   ("Asterisks" "append-asterisk-to-column-80" "Append `*' characters up to column 80.")
   ("XML Header" "insert-xml-header" "Insert XML header line.")
   ("DB Change Log" "insert-db-change-log-template-line" "Insert template line for DB change log.")
   ("DB Change Log Legacy" "insert-db-change-log-template-line-legacy" "Insert template line for legacy DB change log.")
   ("Capture Table" "(table-capture (mark) (point) \"  \" \"\n\" 'left 20)" "Capture table from selected text.")
   ("Apostrophe" "(insert \"’\")" "Insert a fancy apostrophe `’'.")
   ("Lexical Binding" "insert-lexical-binding" "Insert elisp lexical binding header.")
   ("TOC Header" "insert-toc-header" "Insert org-mode table of contents header.")
   ))
;; ("Muse"
;;  ("Muse Header" "muse-header" "Insert Muse standard header line.")
;;  ("Muse Blog Header" "muse-blog-header" "Insert Muse blog header line."))
;; Insert Menu:1 ends here

;; [[file:init-emacs.org::#menus-weather-menu][Weather Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Weather Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Weather Menu")

;; weather menu
(when (and (functionp 'wttrin-query)
           (boundp 'wttrin-default-cities))
  (auto-menu
   "Weather"
   (mapcar (lambda (x)
             (list x
                   (concat "(wttrin-query \"" x "\")")
                   (concat "Show weather report for \\\"" x "\\\".")))
           wttrin-default-cities)))
;; Weather Menu:1 ends here

;; [[file:init-emacs.org::#menus-games-menu][Games Menu:1]]
;;------------------------------------------------------------------------------
;;; Menus: Games Menu
;;------------------------------------------------------------------------------

(init-message 2 "Menus: Games Menu")

;; manual menu
(auto-menu
 "Games"
 `(("5x5" "5x5" "Simple little puzzle game.")
   ("Blackbox" "blackbox" "Blackbox is a game of hide and seek.")
   ("Bubbles" "bubbles" "Remove all bubbles with as few moves as possible.")
   ("Doctor" "doctor" "Psychological help for frustrated users.")
   ("Dunnet" "dunnet" "Text adventure game.")
   ("Gomoku" "gomoku" "Gomoku game between you and Emacs.")
   ("Hanoi" "hanoi" "Towers of Hanoi diversion.")
   ("Life" "life" "John Horton Conway's game of Life.")
   ("Mpuz" "mpuz" "Multiplication puzzle.")
   ("Pong" "pong" "Classical implementation of pong.")
   ("Snake" "snake" "Implementation of the Snake game.")
   ("Solitaire" "solitaire" "Game of solitaire.")
   ("Tetris" "tetris" "Implementation of Tetris.")))
;; Games Menu:1 ends here

;;==============================================================================
;;; Snippets
;;==============================================================================

(init-message 1 "Snippets")

;;------------------------------------------------------------------------------
;;; Snippets: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Snippets: Configuration")

;;------------------------------------------------------------------------------
;;;; Snippets: Setup: Yasnippet
;;------------------------------------------------------------------------------

(init-message 3 "Snippets: Setup: Yasnippet")

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-c & n" . yas-new-snippet)
              ("C-c & s" . yas-insert-snippet)
              ("C-c & v" . yas-visit-snippet-file)
              ("C-/" . yas-insert-snippet)) ; default: `undo-tree-undo'
  :config
  ;; turn on globally
  (yas-global-mode 1)

  ;; turn off yas-expand on tab (all three are needed it seems)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)

  ;; (defun flyspell-incorrect-hook--inhibbit-when-yasnippet-active (beg end corrections)
  ;;   "Inhibbit flyspell mode as it interferes with yasnippet."
  ;;   (and yas-active-field-overlay
  ;;        (overlay-buffer yas-active-field-overlay)))
  ;; (add-hook 'flyspell-incorrect-hook #'flyspell-incorrect-hook--inhibbit-when-yasnippet-active)))
  )

;;------------------------------------------------------------------------------
;;;; Snippets: Setup: Yasnippet: Yasnippet Snippets
;;------------------------------------------------------------------------------

(init-message 4 "Snippets: Setup: Yasnippet: Yasnippet Snippets")

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))

;;------------------------------------------------------------------------------
;;; Snippets: Org-Mode
;;------------------------------------------------------------------------------

(init-message 2 "Snippets: Org-Mode")

;; [[file:init-emacs.org::#hydras][Hydras:1]]
;;==============================================================================
;;; Hydras
;;==============================================================================

(init-message 1 "Hydras")
;; Hydras:1 ends here

;; [[file:init-emacs.org::#hydras-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; Hydras: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "Hydras: Configuration")

(init-message 3 "hydra")

;; hydra
(use-package hydra
  :straight t)
;; Configuration:1 ends here

;; [[file:init-emacs.org::#windows-os][Windows OS:1]]
;;==============================================================================
;;; Windows OS
;;==============================================================================

(when window-system-windows
  (init-message 1 "Windows OS")

  ;; ;; turn on the menubar
  ;;(menu-bar-mode t)

  ;; ;; w32-feeling
  ;; (use-package w32-feeling
  ;;   :straight (:type built-in)
  ;;   :config (w32-feeling-maximize-frame))

  ;; set temp directories
  (setenv "TEMP" "c:/windows/temp")
  (setenv "TMP" "c:/windows/temp")

  ;; ;; cua
  ;; (cua-mode 1)           ; with C-x/c/v key bindings
  ;; ;;(cua-selection-mode 1) ; without C-x/c/v key bindings
  ;; (setq cua-delete-copy-to-register-0 t)
  ;; ;; key bindings
  ;; (bind-keys ("C-@" . cua-set-mark))

  ;; ;; clipboard
  ;; (setq select-enable-clipboard t
  ;;       interprogram-paste-function 'x-cut-buffer-or-selection-value)
  ;; (use-package pc-select
  ;;   :straight (:type built-in)
  ;;   :config (pc-selection-mode))

  ;; ;; turn on timeclock display (since I'm at work if I'm using Windows)
  ;; (timeclock-mode-line-display)

  ;; set page-up and page-down keys (again)
  (bind-keys ("<next>" . scroll-up-enhanced)
             ("<prior>" . scroll-down-enhanced)))
;; Windows OS:1 ends here

;; [[file:init-emacs.org::#gnus][Gnus:1]]
;;==============================================================================
;;; Gnus
;;==============================================================================

(init-message 1 "Gnus")

;; gnus (newsreader)
(use-package gnus
  :straight (:type built-in)
  :defines (gnus-subscribe-newsgroup-method
            sendmail-program mail-envelope-from
            smtpmail-smtp-server
            smtpmail-smtp-service)
  :config
  ;; newsserver
  ;;(setq gnus-select-method '(nntp "news.gmane.org"))

  ;; gmail imap server
  (setq gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port 993)
                                    (nnimap-stream ssl)))

  ;; ;; localhost imap server
  ;; (setq gnus-select-method '(nnnil "")
  ;;       gnus-secondary-select-methods
  ;;       '((nnimap "localhost"
  ;;                 (nnimap-address "localhost")
  ;;                 (nnimap-stream network)
  ;;                 (nnimap-authenticator login))))

  ;; gmail smtp server
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

  ;; do not fetch mail
  (setq mail-sources nil)

  ;; use gnus to send mail
  (setq mail-user-agent 'gnus-user-agent)

  ;; ;; store sent mail on local imap server
  ;; (setq gnus-message-archive-method '(nnimap "localhost"))
  ;; ;;(setq gnus-message-archive-group "nnimap+localhost:Sent")
  ;; (setq gnus-message-archive-group
  ;;       '((if (message-news-p)
  ;;             "nnimap+localhost:SentNews"
  ;;           "nnimap+localhost:Sent")))

  ;; fetch all messages and never expire them
  (setq gnus-agent-cache t)

  ;; expire certain directories
  (setq gnus-auto-expirable-newsgroups "Trash")
  ;;      "Trash\\|")

  ;; do not prompt when accessing large folders
  (setq gnus-large-newsgroup nil)

  ;; summary line format
  ;; default: "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
  (setq gnus-summary-line-format "%U%R%z %o %I%(%[%-25,25n%]%) %s\n")

  ;; automatically subscribe to new groups
  ;;(setq gnus-subscribe-newsgroup-method #'gnus-subscribe-alphabetically)

  ;; ;; article sort order
  ;; (setq gnus-article-sort-functions
  ;;       '(gnus-article-sort-by-number
  ;;         (lambda (t1 t2)
  ;;           (gnus-article-sort-by-date t2 t1))))

  ;; ;; thread sort order
  ;; (setq gnus-thread-sort-functions
  ;;       '(gnus-thread-sort-by-most-recent-number
  ;;     (lambda (t1 t2)
  ;;       (gnus-thread-sort-by-most-recent-date t1 t2))))

  ;; ;; use threads
  ;; (setq gnus-show-threads t)
  ;; (setq gnus-thread-hide-subtree nil)

  ;; set return address depending on folder
  ;; (setq gnus-posting-styles
  ;;       '((".*"
  ;;          (name user-full-name)
  ;;          ("X-URL" "http://nulldot.org/"))
  ;;         ("work"
  ;;          (address "kyle.sherman@dowjones.com"))))

  ;; send mail function
  ;; (setq send-mail-function #'sendmail-send-it
  ;;       message-send-mail-function #'sendmail-send-it)

  ;; msmtp setup
  (setq sendmail-program "/usr/bin/msmtp"
        mail-specify-envelope-from t
        mail-envelope-from 'header)

  ;; default from address
  (setq mail-from-style 'angles
        ;;mail-host-address "bofh.bofh"
        mail-host-address user-mail-address
        mail-interactive t)

  ;; add signature
  ;;(setq message-signature t)
  (setq message-signature #'signature)

  ;; spell check before sending mail
  (add-hook 'message-send-hook #'ispell-message))

;;------------------------------------------------------------------------------
;;;; supercite
;;------------------------------------------------------------------------------

(init-message 3 "supercite")

;; use supercite for quoting
(use-package supercite
  :straight t
  :after (gnus)
  :commands (sc-cite-original)
  :config
  (setq sc-citation-leader "")
  (setq message-cite-function 'sc-cite-original)
  (add-hook 'mail-yank-hooks #'sc-cite-original))
;; Gnus:1 ends here

;; [[file:init-emacs.org::#erc][ERC:1]]
;;==============================================================================
;;; ERC
;;==============================================================================

(init-message 1 "ERC")
;; ERC:1 ends here

;; [[file:init-emacs.org::#erc-configuration][Configuration:1]]
;;------------------------------------------------------------------------------
;;; ERC: Configuration
;;------------------------------------------------------------------------------

(init-message 2 "ERC: Configuration")

(use-package erc
  :straight (:type built-in)
  :config
  (require 'erc-imenu)
  (require 'erc-menu)
  (require 'erc-notify)
  (require 'erc-ring)
  (erc-button-mode 1)
  (erc-completion-mode 1)
  (erc-fill-mode 1)
  (erc-match-mode 1)
  (erc-netsplit-mode 1)
  (erc-services-mode 1)
  (erc-timestamp-mode 1)
  (erc-track-mode 1)
  (add-hook 'erc-mode-hook #'erc-add-scroll-to-bottom)
  ;; (add-to-list 'erc-nick-popup-alist
  ;;              '("DebianDB" .
  ;;                (shell-command
  ;;                 (format
  ;;                  "ldapsearch -x -P 2 -h db.debian.org -b dc=debian,dc=org ircnick=%s"
  ;;                  nick))) t)
  )
;; Configuration:1 ends here

;; [[file:init-emacs.org::#erc-customization][Customization:1]]
;;------------------------------------------------------------------------------
;;; ERC: Customization
;;------------------------------------------------------------------------------

(init-message 2 "ERC: Customization")

;; erc customization
(use-package erc
  :straight (:type built-in)
  :config
  ;; create a seperate buffer for private messages
  (setq erc-auto-query t)
  ;; have noticies appear in minibuffer
  (setq erc-echo-notices-in-minibuffer-flag t)
  ;; add users to bbdb
  ;;(setq erc-bbdb-auto-create-on-whois-p t)
  ;; wrap text at edge of window
  (setq erc-fill-column (- (window-width) 2))
  (setq erc-fill-function 'erc-fill-static)
  ;; interpret mIRC color codes
  (setq erc-interpret-mirc-color t)
  ;; hide certain types of messages
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  ;; default nick
  (setq erc-nick "nullman")
  ;; friends list for notifications
  (setq erc-pals '("jam" "dmitri" "peter" "glenn" "vin"))
  (setq erc-notify-list erc-pals)
  ;; channels to auto-join
  (setq erc-autojoin-channels-alist '((".*freenode\.net" . ("#emacs"
                                                            ;;"#elisp"
                                                            ;;"#erc"
                                                            "#lisp"
                                                            ;;"#clojure"
                                                            ))
                                      (".*dowjones\.net" . ("#dev"))
                                      ("sbkdevtick11" . ("#dev"))
                                      ("localhost" . ("#collab"))))
  ;; modules to load
  (mapc (lambda (x) (add-to-list 'erc-modules x t))
        '(autojoin button completion fill identd irccontrols list match menu
                   move-to-prompt netsplit networks noncommands readonly ring
                   scrolltobottom services stamp spelling track))
  ;; C-RET (or C-c RET or C-c C-RET) sends messages, insteasd of RET
  ;; (define-key erc-mode-map (kbd "RET") nil)
  ;; (define-key erc-mode-map (kbd "C-RET") 'erc-send-current-line)
  ;; (define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
  ;; (define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

  ;; ~/.erc-auth should contain the following:
  ;;
  ;; (setq erc-nickserv-passwords
  ;;      '((freenode (("nullman" . "PASSWORD")))
  ;;        ))

  ;; load nickserv authentication file
  (defconst erc-auth-file-name (file-truename (expand-file-name "~/.erc-auth")))
  (when (file-exists-p erc-auth-file-name)
    (setq erc-prompt-for-nickserv-password nil)
    (load erc-auth-file-name)))
;; Customization:1 ends here

;; [[file:init-emacs.org::#erc-functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; ERC: Functions
;;------------------------------------------------------------------------------

(init-message 2 "ERC: Functions")
;; Functions:1 ends here

;; [[file:init-emacs.org::#erc-functions-nick-from-system-name][Nick from System Name:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Functions: Nick from System Name
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Functions: Nick from System Name")

(use-package erc
  :straight (:type built-in)
  :config
  (defun erc-nick-from-system-name ()
    "Return a nickname based on machine name.

Defaults to \"nullman\" if no match is found."
    (let* ((server-nick '(
                          ;;("tank.nullware.com" . "nulltank")
                          ;;("tank" . "nulltank")
                          ;;("neo.nullware.com" . "nullhome")
                          ;;("neo" . "nullhome")
                          ;;("mouse1.nullware.com" . "nullman")
                          ;;("mouse1" . "nullman")
                          ;;("min-kyle-linux.production.bigcharts.com" . "nullwork")
                          ;;("min-kyle-linux" . "nullwork")
                          ))
           (nick (cdr (assoc (system-name) server-nick))))
      (or nick erc-nick))))
;; Nick from System Name:1 ends here

;; [[file:init-emacs.org::#erc-functions-localhost][Localhost:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Functions: Localhost
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Functions: Localhost")

(use-package erc
  :straight (:type built-in)
  :commands (erc)
  :config
  (defun erc-localhost ()
    "Connect to localhost irc server."
    (interactive)
    (let ((nick (erc-nick-from-system-name)))
      (erc-services-disable)
      (erc :server "localhost" :port "6667" :nick nick :password nil :full-name "Kyle Sherman"))))
;;(bind-keys ("C-c el" . erc-localhost)))
;; Localhost:1 ends here

;; [[file:init-emacs.org::#erc-functions-localhost-bitlbee][Localhost Bitlbee:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Functions: Localhost Bitlbee
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Functions: Localhost Bitlbee")

(use-package erc
  :straight (:type built-in)
  ;; :bind* ("C-c eb" . erc-localhost-bitlbee)
  :config
  (defun erc-localhost-bitlbee ()
    "Connect to localhost bitlbee server."
    (interactive)
    (let ((nick (erc-nick-from-system-name)))
      (erc-services-disable)
      (erc :server "localhost" :port "6668" :nick nick :password nil :full-name "Kyle Sherman"))))
;; Localhost Bitlbee:1 ends here

;; [[file:init-emacs.org::#erc-functions-freenode][Freenode:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Functions: Freenode
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Functions: Freenode")

(use-package erc
  :straight (:type built-in)
  ;; :bind* ("C-c ef" . erc-freenode)
  :config
  (defun erc-freenode ()
    "Connect to irc.freenode.net irc server."
    (interactive)
    (let ((nick (erc-nick-from-system-name)))
      (erc-services-enable)
      (erc :server "irc.freenode.net" :port "6667" :nick nick :password nil :full-name "Kyle Sherman"))))
;; Freenode:1 ends here

;; [[file:init-emacs.org::#erc-functions-work][Work:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Functions: Work
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Functions: Work")

(use-package erc
  :straight (:type built-in)
  :config
  (defun erc-work ()
    "Connect to work IRC server."
    (interactive)
    "Connect to irc.win.dowjones.net IRC server."
    (erc-services-disable)
    (erc :server "irc.win.dowjones.net" :port "6667" :nick "kyle" :password nil :full-name "Kyle Sherman")))
;;(bind-keys ("C-c ew" . erc-work)))
;; Work:1 ends here

;; [[file:init-emacs.org::#erc-commands][Commands:1]]
;;------------------------------------------------------------------------------
;;; ERC: Commands
;;------------------------------------------------------------------------------

(init-message 2 "ERC: Commands")
;; Commands:1 ends here

;; [[file:init-emacs.org::#erc-commands-uptime][UPTIME:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Commands: UPTIME
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Commands: UPTIME")

(use-package erc
  :straight (:type built-in)
  :commands (erc-send-message)
  :config
  (defun erc-cmd-UPTIME (&rest ignore)
    "Display the uptime of the system, as well as some load-related stuff,
to the current ERC buffer."
    (let ((uname-output
           (replace-regexp-in-string
            ", load average: " "] {Load average} ["
            ;; collapse spaces
            (replace-regexp-in-string
             " +" " "
             ;; remove beginning and trailing whitespace
             (replace-regexp-in-string
              "^ +\\|[ \n]+$" ""
              (shell-command-to-string "uptime"))))))
      (erc-send-message
       (concat "{Uptime} [" uname-output "]")))))
;; UPTIME:1 ends here

;; [[file:init-emacs.org::#erc-commands-wi][WI:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Commands: WI
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Commands: WI")

(use-package erc
  :straight (:type built-in)
  :config
  (defun erc-cmd-WI (nick &rest ignore)
    "`/WHOIS' command with extra user information."
    (erc-server-send (mapconcat #'identity (list "WHOIS" nick nick) " "))))
;; WI:1 ends here

;; [[file:init-emacs.org::#erc-commands-identify][IDENTIFY:1]]
;;------------------------------------------------------------------------------
;;;; ERC: Commands: IDENTIFY
;;------------------------------------------------------------------------------

(init-message 3 "ERC: Commands: IDENTIFY")

(use-package erc
  :straight (:type built-in)
  :commands (erc-server-send)
  :config
  (defun erc-cmd-IDENTIFY (password &rest ignore)
    "Send PASSWORD to NickServ, `/msg NickServ identify PASSWORD'."
    (erc-server-send (mapconcat #'identity (list "identify" password) " "))))
;; IDENTIFY:1 ends here

;; [[file:init-emacs.org::#work][Work:1]]
;;==============================================================================
;;; Work
;;==============================================================================

(when work-system
  (init-message 1 "Work"))
;; Work:1 ends here

;; [[file:init-emacs.org::#work-modules][Modules:1]]
;;------------------------------------------------------------------------------
;;; Work: Modules
;;------------------------------------------------------------------------------

(when work-system
  (init-message 2 "Work: Modules")

  ;; local paths
  ;; (when local-work-modules-dir
  ;;   (add-to-list 'load-path local-work-modules-dir t)) ; work elisp projects
  )
;; Modules:1 ends here

;; [[file:init-emacs.org::#work-settings][Settings:1]]
;;------------------------------------------------------------------------------
;;; Work: Settings
;;------------------------------------------------------------------------------

(when work-system
  (init-message 2 "Work: Settings")

  ;; set default browser
  (setq browse-url-browser-function #'browse-url-default-browser)

  ;; set secondary browser
  (setq browse-url-secondary-browser-function #'browse-url-default-browser)

  ;; add perl files to remove-tabs-exceptions
  (add-to-list 'remove-tabs-exceptions '(:file . "\\.t\\'") t)
  (add-to-list 'remove-tabs-exceptions '(:file . "\\.tt\\'") t)
  (add-to-list 'remove-tabs-exceptions '(:file . "\\.pm\\'") t)

  ;; sql setup
  (setq sql-product 'ms)
  (setq sql-ms-program "sqlcmd")
  ;; https://docs.microsoft.com/en-us/sql/tools/sqlcmd-utility?view=sql-server-ver15
  ;; -I (enable quoted identifiers)
  ;; -k[1 | 2] (remove or replace control characters)
  ;; -s col_separator
  ;; -w column_width
  ;; -y variable_length_type_display_width
  (setq sql-ms-options '("-I" "-k" "-s" "|"))

  ;; database connections
  (setq sql-connection-alist
        '((dev (sql-product 'ms)
               (sql-server "wk8683.infinitecampus.com")
               (sql-port 1344)
               (sql-database "CampusMasterMN")
               (sql-user "dev")
               (sql-password "devTest"))
          (issuetest (sql-product 'ms)
                     (sql-server "b4660797-app001.infinitecampus.com")
                     (sql-port 1344)
                     (sql-database "3384-littleton-20210801_2101")
                     (sql-user "IssueTestUser-3384-littleton-20210801_2101-962")
                     (sql-password "h*cjFmVd9S0Hoqui7GQpiZ%chEit@y"))))

  (defun sql-ms-dev ()
    "Connect to local dev database."
    (interactive)
    (sql-connect 'dev))

  (defun sql-ms-issuetest ()
    "Connect to issuetest database."
    (interactive)
    (sql-connect 'issuetest)))
;; Settings:1 ends here

;; [[file:init-emacs.org::#work-functions][Functions:1]]
;;------------------------------------------------------------------------------
;;; Work: Functions
;;------------------------------------------------------------------------------

(when work-system
  (init-message 2 "Work: Functions"))
;; Functions:1 ends here

;; [[file:init-emacs.org::#work-functions-work-insert-release-pr-list][+work-insert-release-pr-list+:1]]
;;------------------------------------------------------------------------------
;;;; Work: Functions: work-insert-release-pr-list
;;------------------------------------------------------------------------------

(when (and work-system
           (fboundp 'work-git-commit-start-end-log)
           (fboundp 'work-linkify-jira-card))
  (init-message 3 "Work: Functions: work-insert-release-pr-list")

  (defun work-insert-release-pr-list (&optional commit-start commit-end)
    "Insert a list of pull requests between COMMIT-START and COMMIT-END."
    (interactive)
    (cl-labels
        ;; clean up branch name
        ((fix-branch ()
                     ;; convert spaces to dashes
                     (save-mark-and-excursion
                       (save-match-data
                         (while (re-search-forward " " (line-end-position) :noerror)
                           (replace-match "-" t))))
                     ;; remove brackets
                     (save-mark-and-excursion
                       (save-match-data
                         (while (re-search-forward "\\(\\[\\|\\]\\)" (line-end-position) :noerror)
                           (replace-match "" t))))))
      (cl-multiple-value-bind (commit-start commit-end log)
          (work-git-commit-start-end-log commit-start commit-end)
        (let (prs)
          (with-temp-buffer
            (insert log)
            ;; remove leading "feature/" text
            (goto-char (point-min))
            (while (re-search-forward "\\bfeature/" nil :noerror)
              (replace-match ""))
            ;; handle different project names
            (goto-char (point-min))
            (while (re-search-forward "\\[?\\b[Aa][Nn][Dd][Rr][Oo][Ii][Dd]-" nil :noerror)
              (replace-match "ANDROID-" t)
              (fix-branch)
              (forward-line 0)
              (when (re-search-forward "\\b\\(ANDROID-[[:digit:]]+\\)\\([a-zA-Z0-9-_\.]*\\)\\b" nil :noerror)
                (cl-pushnew (match-string 0) prs :test 'string=))
              (goto-char (line-end-position)))
            (goto-char (point-min))
            (while (re-search-forward "\\[?\\b[Aa][Dd][Ss][Gg][Rr][Oo][Uu][Pp]-" nil :noerror)
              (replace-match "ADSGROUP-" t)
              (fix-branch)
              (forward-line 0)
              (when (re-search-forward "\\b\\(ADSGROUP-[[:digit:]]+\\)\\([a-zA-Z0-9-_\.]*\\)\\b" nil :noerror)
                (cl-pushnew (match-string 0) prs :test 'string=))
              (goto-char (line-end-position)))
            (goto-char (point-min))
            (while (re-search-forward "\\[?\\b[Bb][Ff][Oo]-" nil :noerror)
              (replace-match "BFO-" t)
              (fix-branch)
              (forward-line 0)
              (when (re-search-forward "\\b\\(BFO-[[:digit:]]+\\)\\([a-zA-Z0-9-_\.]*\\)\\b" nil :noerror)
                (cl-pushnew (match-string 0) prs :test 'string=))
              (goto-char (line-end-position)))
            (goto-char (point-min))
            (while (re-search-forward "\\[?\\b[Qq][Uu][Ii][Zz]-" nil :noerror)
              (replace-match "QUIZ-" t)
              (fix-branch)
              (forward-line 0)
              (when (re-search-forward "\\b\\(QUIZ-[[:digit:]]+\\)\\([a-zA-Z0-9-_\.]*\\)\\b" nil :noerror)
                (cl-pushnew (match-string 0) prs :test 'string=))
              (goto-char (line-end-position))))
          ;; output list
          (when prs
            (insert
             (with-temp-buffer
               (dolist (pr prs)
                 (insert
                  (concat "- " pr))
                 (work-linkify-jira-card)
                 (newline))
               (buffer-string)))))))))
;; +work-insert-release-pr-list+:1 ends here

;; [[file:init-emacs.org::#work-functions-work-fix-json-array][work-fix-json-array:1]]
;;------------------------------------------------------------------------------
;;;; Work: Functions: work-fix-json-array
;;------------------------------------------------------------------------------

(init-message 3 "Work: Functions: work-fix-json-array")

(defun work-fix-json-array ()
  "Fix invalid JavaScript JSON array."
  (interactive "*")
  (save-mark-and-excursion
    ;; remove anything outside of array brackets
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (= (char-after) ?\[)))
      (delete-char 1))
    (goto-char (point-max))
    (while (and (not (bobp))
                (not (= (char-before) ?\])))
      (backward-delete-char 1))
    ;; reformat json objects to one per line
    (goto-char (point-min))
    (while (re-search-forward "\{" nil :no-error)
      (let ((beg (point-marker))
            (end (progn
                   (re-search-forward "\}")
                   (forward-char -1)
                   (point-marker))))
        (goto-char (marker-position beg))
        (while (re-search-forward "\n *" (marker-position end) :no-error)
          (replace-match " "))))
    ;; remove any ending comas
    (goto-char (point-min))
    (while (re-search-forward ", \}" nil :no-error)
      (replace-match " }"))
    (goto-char (point-min))
    (while (re-search-forward "\},\n[ \t]*\]" nil :no-error)
      (replace-match "}\n]"))
    ;; convert single quotes to double quotes
    (goto-char (point-min))
    (while (re-search-forward "'" nil :no-error)
      (replace-match "\""))))
;; work-fix-json-array:1 ends here

;; [[file:init-emacs.org::#work-menu][Menu:1]]
;;------------------------------------------------------------------------------
;;; Work: Menu
;;------------------------------------------------------------------------------

(when work-system
  (init-message 2 "Work: Menu")

  ;; work menu
  (auto-menu
   "Work"
   `(("Dired..."
      ,(auto-menu-dired '(("work" . "~/work")
                          ("Tomcat" . "~/ICAS-DEV-999.2.29/INFINITECAMPUS/tomcat/tigger-999.2.29")
                          ("exceptionals-campus" . "~/work/exceptionals-campus")
                          ("exceptionals-tl-ui" . "~/work/exceptionals-tl-ui")
                          ("campus-5.0-apps" . "~/work/campus-5.0-apps"))))
     ("org-copy-to-clipboard" "org-copy-to-clipboard" "Reformat and copy org region to clipboard.")
     ("org-toggle-link-display" "org-toggle-link-display" "Toggle the literal or descriptive display of links.")
     ("org-toggle-headline-checkbox" "org-toggle-headline-checkbox" "Toggle between an Org headline and checkbox on current line.")
     ("org-table-remove-commas" "org-table-remove-commas" "Remove all commas in current Org table.")
     ("org-table-convert-region" "org-table-convert-region" "Convert region to a table."))))
;; Menu:1 ends here

;; [[file:init-emacs.org::#other][Other:1]]
;;==============================================================================
;;; Other
;;==============================================================================

(init-message 1 "Other")
;; Other:1 ends here

;; [[file:init-emacs.org::#other-apply-advice][Apply Advice:1]]
;;------------------------------------------------------------------------------
;;; Other: Apply Advice
;;------------------------------------------------------------------------------

(init-message 2 "Other: Apply Advice")
;; Apply Advice:1 ends here

;; [[file:init-emacs.org::#other-apply-advice-compile-goto-error-org][Compile Goto Error Org:1]]
;;------------------------------------------------------------------------------
;;;; Other: Apply Advice: Compile Goto Error Org
;;------------------------------------------------------------------------------

(init-message 3 "Other: Apply Advice: Compile Goto Error Org")

(defun compile-goto-error--org (&optional event)
  "Open compilation bugs in org file for errors in tangled elisp code."
  (when (eq major-mode 'emacs-lisp-mode)
    (ignore-errors (org-babel-tangle-jump-to-org))))

(defun compilation-mode-hook--compile-goto-error ()
  "Hook to advise `compile-goto-error'."
  (advice-add 'compile-goto-error :after #'compile-goto-error--org))
(add-hook 'compilation-mode-hook #'compilation-mode-hook--compile-goto-error)
;; Compile Goto Error Org:1 ends here

;; [[file:init-emacs.org::#other-apply-patches][Apply Patches:1]]
;;------------------------------------------------------------------------------
;;; Other: Apply Patches
;;------------------------------------------------------------------------------

(init-message 2 "Other: Apply Patches")

;; force `find-file' to load local variables
(add-hook 'find-file-hook #'hack-local-variables)
;; Apply Patches:1 ends here

;; [[file:init-emacs.org::#aliases][Aliases:1]]
;;==============================================================================
;;; Aliases
;;==============================================================================

(init-message 1 "Aliases")
;; Aliases:1 ends here

;; [[file:init-emacs.org::#aliases-general][General:1]]
(let ((data '(("Alias" "Function") ("lml" "list-matching-lines") ("qrr" "query-replace-regexp") ("rb" "revert-buffer") ("rxb" "regexp-builder"))))
;;------------------------------------------------------------------------------
;;; Aliases: General
;;------------------------------------------------------------------------------

(init-message 2 "Aliases: General")

(mapc (lambda (x) (defalias (intern (car x)) (intern (cadr x)))) (cdr data))
)
;; General:1 ends here

;; [[file:init-emacs.org::#final][Final:1]]
;;==============================================================================
;;; Final
;;==============================================================================

(init-message 1 "Final")
;; Final:1 ends here

;; [[file:init-emacs.org::#final-setup-set-key-bindings][Set Key Bindings:1]]
;;------------------------------------------------------------------------------
;;; Final: Set Key Bindings
;;------------------------------------------------------------------------------

(init-message 2 "Final: Set Key Bindings")

(custom-key-bindings-set-all)
;; Set Key Bindings:1 ends here

;; [[file:init-emacs.org::#final-setup-compile-personal-modules][Compile Personal Modules:1]]
;;------------------------------------------------------------------------------
;;; Final: Compile Personal Modules
;;------------------------------------------------------------------------------

(init-message 2 "Final: Compile Personal Modules")

;; compile personal modules if needed
(let ((dir local-modules-dir))
  (when (file-exists-p dir)
    (dolist (file (directory-files dir t))
      (when (file-readable-p file)
        (cond
         ((string-match "^\\.\\.?$" (file-name-nondirectory file)))
         ((string-match ".*\\.el\\'" file)
          (when (not (file-directory-p file))
            (compile-file-if-needed file))))))))
;; Compile Personal Modules:1 ends here

;; [[file:init-emacs.org::#final-setup-start-emacs-server][Start Emacs Server:1]]
;;------------------------------------------------------------------------------
;;; Final: Start Emacs Server
;;------------------------------------------------------------------------------

(init-message 2 "Final: Start Emacs Server")

;; start emacs server
(when (fboundp 'server-start-maybe)
  (when-lock-file-acquired (expand-file-name "emacs-server-lock-file"
                                            temporary-file-directory)
    (server-start-maybe)))
;; Start Emacs Server:1 ends here

;; [[file:init-emacs.org::#final-setup-remove-logging-buffers][Remove Logging Buffers:1]]
;;------------------------------------------------------------------------------
;;; Final: Remove Logging Buffers
;;------------------------------------------------------------------------------

(init-message 2 "Final: Remove Logging Buffers")

;; ;; remove compile log buffer
;; (when (get-buffer "*Compile-Log*")
;;   (kill-buffer "*Compile-Log*"))

;; remove symbol mapping buffer
(when (get-buffer "Map_Sym.txt")
  (kill-buffer "Map_Sym.txt"))
;; Remove Logging Buffers:1 ends here

;; [[file:init-emacs.org::#final-setup-fix-info-directory-list][Fix Info-Directory-List:1]]
;;------------------------------------------------------------------------------
;;; Final: Fix Info-Directory-List
;;------------------------------------------------------------------------------

(init-message 2 "Final: Fix Info-Directory-List")

;; make sure Info-default-directory-list is added to Info-directory-list
(when (boundp 'Info-directory-list)
  (mapc (lambda (x) (add-to-list 'Info-directory-list x t))
        Info-default-directory-list))
;; Fix Info-Directory-List:1 ends here

;; [[file:init-emacs.org::#final-setup-turn-off-scroll-bar][Turn off Scroll Bar:1]]
;;------------------------------------------------------------------------------
;;; Final: Turn off Scroll Bar
;;------------------------------------------------------------------------------

(init-message 2 "Final: Turn off Scroll Bar")

;; turn off scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; Turn off Scroll Bar:1 ends here

;; [[file:init-emacs.org::#final-setup-reset-emacs-lisp-garbage-collection-threshold][Reset Emacs Lisp Garbage Collection Threshold:1]]
;;------------------------------------------------------------------------------
;;; Final: Reset Emacs Lisp Garbage Collection Threshold
;;------------------------------------------------------------------------------

(init-message 2 "Final: Reset Emacs Lisp Garbage Collection Threshold")

;; reset frequency of garbage collections (to 800000)
(setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
;; Reset Emacs Lisp Garbage Collection Threshold:1 ends here

;; [[file:init-emacs.org::#end][End:1]]
(init-message 1 "End")

;;==============================================================================
;;; init-emacs.el ends here
;;==============================================================================
;; End:1 ends here
