;;; emacs-modules.el --- Fetch and Install Emacs Modules
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-02-16
;; Version:  1.0
;; Keywords: module install
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Provides `emacs-modules' function that searches for and displays known
;; emacs modules that when selected are downloaded and installed.
;;
;;; Installation:
;;
;; Put `emacs-modules.el' where you keep your elisp files and add
;; something like the following to your .emacs file:
;;
;;   (require 'emacs-modules)
;;
;;; Usage:
;;
;; Call `emacs-modules' with a module name, or with no parameter for a
;; list to pick from.
;;
;; Example:
;;
;;   (emacs-modules "ibuffer")

;;; TODO Outline:
;; - Write background fetching apps to get lists of available el packages for download
;; - Check list versions against installed versions and highlight ones that are outdated
;; - Put this one at the top of the list if it is outdated
;; - List modules from local saved data file
;; - Background loader updates saved data file
;; - Store code to download, unpack, install, compile, etc in lisp data object
;;   (list), so no new functions need to be made/altered with new package
;;   logic
;; - After loading a new version of this modules, you should be able to exec
;;   it and run the main function

;;; Code:

;; w3m
(require 'w3m)

;; customize group
(defgroup emacs-modules nil
  "Fetch and install Emacs modules."
  :prefix "emacs-modules-"
  :group 'applications)

;; display buffer name
(defvar emacs-modules-buffer-name
  "*Emacs-Modules*"
  "Buffer name to use for displaying and selecting emacs modules to install.")

;; elisp home
(defcustom emacs-modules-elisp-home
  (expand-file-name "~/.emacs.d/modules")
  "Location to store fetched Elisp files."
  :type 'file
  :group 'emacs-modules)

;; temp directory to hold downloads
(defvar emacs-modules-tmp-dir
  (expand-file-name "~/tmp/emacs-modules")
  "Temp directory to hold downloads.
\nIt is created and deleted before and after every fetch.")

;; install file macro
(defmacro emacs-modules-install-file (file)
  "Simple install macro to copy an Elisp FILE to `emacs-modules-elisp-home'."
  `(lambda ()
     ;; copy fetched file to elisp dir
     (shell-command (concat
                     "cp " (shell-quote-argument ,file)
                     " " (shell-quote-argument emacs-modules-elisp-home)))
     ;; compile file
     (batch-byte-compile-file ,file)))

;; install tarball macro
(defmacro emacs-modules-install-tarball-dir (file dir)
  "Install macro to unpack FILE and copy the contents to directory DIR in `emacs-modules-elisp-home'."
  `(lambda ()
     ;; unpack fetched file
     (shell-command (concat
                     "tar xvfz " (shell-quote-argument ,file)))
     ;; create elisp dir
     (shell-command (concat
                     "mkdir -p " (shell-quote-argument emacs-modules-elisp-home)
                     "/" (shell-quote-argument ,dir)))
     ;; copy fetched dir to elisp dir
     (shell-command (concat
                     "cp -r " (shell-quote-argument ,file) "/*"
                     " " (shell-quote-argument emacs-modules-elisp-home)
                     "/" (shell-quote-argument ,dir) "/"))
     ;; compile file
     (batch-byte-compile-file ,file)))

;; ;; database to hold module list information
;; ;; TODO: not sure if i'm going to use this or not
;; (defvar emacs-modules-database-list
;;   '(("Emacs Lisp List"
;;      "http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html"
;;      `(lambda ()
;;         ())))
;;   "Database of information about how to fetch module lists, how
;; to fetch modules, how to unpack and install them, etc.

;; The format is a list of lists as follows:

;;   ((NAME URL PARSE-CODE) ...)

;;   NAME is the name.
;;   URL is the url of the list.
;;   PARSE-CODE is a lambda function to eval in the buffer returned
;;   by the URL fetch."
;;   :type 'list
;;   :group 'Emacs-Modules)

;; database to hold module fetch/install information
(defvar emacs-modules-database-install
  `(("apel" "a portable emacs library"
     "ftp://ftp.m17n.org/pub/mule/apel/apel-9.23.tar.gz"
     "apel.tar.gz"
     ,(emacs-modules-install-tarball-dir "apel-*.tar.gz" "apel"))
    ("ascii" "ascii table"
     "http://www.emacswiki.org/cgi-bin/emacs/download/ascii.el"
     "ascii.el"
     ,(emacs-modules-install-file "ascii.el")))
  "Database of information about to fetch modules, unpack, and install them.

The format is a list of lists as follows:

  ((NAME DESC URL FILE CODE) ...)

  NAME is the name of the module.
  DESC is a longer description.
  URL is the url to fetch.
  FILE is the temp file to use when downloading.
  CODE is a function to run that will unpack, install, and
  compile the module.")

;; fetch a url and store the result in a file
(defun emacs-modules-fetch-url-to-file (url file)
  "Fetch URL to FILE."
  (let (cmd)
    ;; create request call
    (setq cmd (concat
               "wget"
               " \"-O\" \"" (shell-quote-argument file) "\""
               " \"" (shell-quote-argument url) "\""))
    (shell-command cmd)))

;; install module
(defun emacs-modules-install (module)
  "Install MODULE using information from ??? `emacs-modules-database-fetch'.
\nThe URL part of the module information is fetched, then the CODE
portion is executed to handle installing it."
  (interactive)
  ;; get module information
  (let* ((info (assoc module emacs-modules-database-install))
         (name (first info))
         (desc (second info))
         (url (third info))
         (file (fourth info))
         (code (fifth info)))
    ;; make sure temp dir exists
    (unless (file-exists-p emacs-modules-tmp-dir)
      (make-directory emacs-modules-tmp-dir))
    ;; fetch url
    (emacs-modules-fetch-url-to-file url file)
    ;; install module
    (eval code)))

(provide 'emacs-modules)

;;; emacs-modules.el ends here
