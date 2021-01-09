;;; auto-menu.el --- Auto Menu Functions
;;
;;; Copyright (C) 2007,2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2007-05-22
;; Version:  1.0
;; Keywords: menu
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Provides `auto-menu', `auto-menu-select', `auto-menu-file-dir',
;; `auto-menu-dired', and `auto-menu-dired-remote' functions to help creating
;; Emacs menus.
;;
;;; Installation:
;;
;; Put `auto-menu.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'auto-menu)
;;
;;; Usage:
;;
;; The `auto-menu' function creates a new menu.  You pass it a name and a list
;; of items.  Each item is either: 1) a list contianing a name, function, and
;; help text, or 2) a sub-menu containing a name and a list of items.  This
;; functionality recurses so you can have n-depth sub-menus.
;;
;; The `auto-menu-select' function is similar to `auto-menu' except that it
;; opens the menu in a buffer and prompts the user to select an item.  It also
;; does not use keymaps and can handle lambda functions.  This function is
;; good to use in other functions that need to prompt the user with a list of
;; options to select from.
;;
;; The `auto-menu-file-dir' function creates a new menu based on a given
;; directory.  It creates an entry for every file matching a pattern (defaults
;; to `.*') and applies a function to it (defaults to `find-file').  There is
;; also an option to recurse through sub-directories or not (defaults to no).
;;
;; The `auto-menu-file' function creates a new menu of `find-file' commands.
;; It creates an entry for every file given.
;;
;; The `auto-menu-dired' function creates a new menu of `dired' commands.  It
;; creates an entry for every directory given.
;;
;; The `auto-menu-dired-remote' function creates a new menu of `dired'
;; commands to connect with remote servers.  It takes a list of servers and
;; users and creates a menu of servers that each lead to sub-menus of users
;; that when selected will open a dired buffer at that location.
;;
;; Some examples from my configuration (edited for size):
;;
;;   ;; dired menu
;;   (auto-menu
;;    "Dired"
;;    (auto-menu-dired '(("home" . "~/")
;;                       (".emacs.d" . "~/.emacs.d")
;;                       (".elisp" . "~/.elisp")
;;                       ("clojure" . "~/clojure")
;;                       ("clisp" . "~/clisp")
;;                       ("bin" . "~/bin"))))
;;
;;   ;; load menu
;;   (auto-menu
;;    "Load"
;;    `(("Restore Context" "(context-restore)" "Restore previous context save.")
;;      ("Home Files..."
;;       ,(auto-menu-file '((".profile" . "~/.profile")
;;                          (".bashrc" . "~/.bashrc"))))
;;      ("Emacs Settings..."
;;       ,(append '((".emacs" "(find-file \"~/.emacs\")" "Load `~/.emacs' file."))
;;                (auto-menu-file-dir "~/.emacs.d" "\\.el$" "find-file")))
;;      ("Elisp Files..."
;;       ,(auto-menu-file-dir "~/.emacs.d" "\\.el$" "find-file" t))
;;      ("Clojure Files..."
;;       ,(auto-menu-file-dir "~/clojure" "\\.clj$" "find-file" t))
;;      ("CLisp Files..."
;;       ,(auto-menu-file-dir "~/clisp" "\\.lisp$" "find-file" t))
;;      ("Org Files..."
;;       ,(auto-menu-file-dir "~/org" "\\.\\(org\\|org\\.cpt\\)$" "find-file" t))))
;;
;;   ;; run menu
;;   (auto-menu
;;    "Run"
;;    '(("Emacs Server" "server-start-maybe" "Restart Emacs server.")
;;      ("Visit TAGS" "(when (file-exists-p \"~/TAGS\") (visit-tags-table \"~/TAGS\"))" "Visit tags table.")
;;      ("IELM Mode" "ielm" "Open buffer for interactively evaluating Emacs Lisp expressions.")
;;      ("SLIME Mode" "slime" "Start SLIME mode for interactively evaluating CLISP expressions.")
;;      ("Evaluate Current Buffer" "eval-buffer" "Run eval-buffer on the current buffer.")
;;      ("Evaluate Current SLIME Buffer" "slime-eval-buffer" "Run slime-eval-buffer on the current buffer.")
;;      ("Compile ~/.elisp Directory" "compile-elisp" "Byte compile `~/.elisp' directory.")
;;      ("Customize Group" "customize-group" "Run customize group function.")
;;      ))
;;
;; ;; TODO: add auto-menu-select examples here
;; (auto-menu-select "test" `(("item1" ,(lambda () (message "test1"))) ("item2" ,(lambda () (message "test2"))) ("version" ,(version))))
;; (auto-menu-select "test" `(("menu1" (("item1" ,(lambda () (message "test1"))) ("item2" ,(lambda () (message "test2")))))))

;;; Code:

;; easymenu
(require 'easymenu)

;; auto menu sanatize
(defun auto-menu-sanatize (name)
  "Return a sanatized version of NAME.

Spaces are converted to dashes and anything not in this string is
removed:

abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"
  (let ((legal-regexp "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012\
3456789_-]"))
    (map 'string (lambda (x) x)
         (cl-loop for x across name
                  if (string-match legal-regexp (char-to-string x))
                  collect x
                  else
                  if (= x 32)
                  collect 45))))

;; auto menu
;;;###autoload
(defun auto-menu (name items &optional submenu)
  "Create a menu called NAME consisting of ITEMS and add it to the menu bar.

ITEMS is a list of items.  An ITEM is either a list containing
the following elements that define a menu item:

  NAME     is the menu item name.
  FUNCTION is a string containing a function name or a function
           definition (without the interactive call).
  HELP     is the help text.

or an ITEM is a named list (a string and a list) defining a
sub-menu where:

  NAME  is the sub-menu name.
  ITEMS is a list of items.

SUBMENU is for internal use."
  (let* ((lname (auto-menu-sanatize (downcase name)))
         (fname (if submenu
                    (concat submenu "-" lname)
                  (concat "auto-menu-function-" lname)))
         (menu-name (concat "auto-menu-" lname "-menu"))
         (menu-map-name (concat "auto-menu-" lname "-menu-map"))
         (menu ""))
    ;; add menu node
    (setq menu (concat menu "  (list \"" name "\"\n"))
    ;; loop through each item in items
    (dolist (item items)
      (let ((item-name (car item))
            (item-fname (concat fname "-" (auto-menu-sanatize (downcase (car item)))))
            (item-thing (cadr item))
            (item-help (caddr item)))
        ;; if item is a submenu, add it and recursively call
        (if (listp item-thing)
            (setq menu (concat menu (auto-menu item-name item-thing fname)))
          ;; otherwise, add menu item
          (let ((funct
                 (if (commandp (intern item-thing))
                     ;; if item is an interactive function use it directly
                     item-thing
                   (progn
                     ;; otherwise, create an interactive function
                     (eval (read (concat "(defun " item-fname " ()"
                                         " \"Auto menu function for `" item-name "'.\""
                                         " (interactive)"
                                         " " (if (string= (substring item-thing 0 1) "(")
                                                 item-thing
                                               (concat "(" item-thing ")"))
                                         ")")))
                     item-fname))))
            ;; add item to the menu
            (setq menu (concat menu
                               "    [\"" item-name "\" " funct
                               (if item-help
                                   (concat " :help \"" item-help "\"")
                                 "")
                               "]\n"))))))
    ;; close menu node
    (setq menu (concat menu "  )\n"))

    ;; return from submenu or finialize result
    (if submenu
        ;; return submenu
        menu
      ;; otherwise, add header and footer code and return full result
      (progn
        ;; create menu map var
        (eval (read (concat "(defvar " menu-map-name " nil \"" name " menu map.\")")))
        ;; create menu const
        (eval (read (concat "(defconst " menu-name "\n" menu ")")))
        ;; create menu map
        (eval (read (concat "(easy-menu-define " menu-map-name " nil "
                            "\"Auto menu for `" name "'.\""
                            " " menu-name ")")))
        ;; remove menu item if it currently exists, then add it back
        (eval (read (concat "(easy-menu-remove-item (current-global-map) '(\"menu-bar\") \"" menu-map-name "\")")))
        (eval (read (concat "(easy-menu-add-item (current-global-map) '(\"menu-bar\") " menu-map-name ")")))
        ;; return t
        t))))

;; auto menu select buffer name
(defvar auto-menu-select-buffer-name
  "*Auto-Menu-Select*"
  "Buffer name to use for select menu.")

;; ;; auto menu select text property
;; (defconst auto-menu-select-text-property
;;   "auto-menu-select-action"
;;   "Name of text property to store menu action.")

;; ;; auto menu select mode map
;; (defvar auto-menu-select-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\r" 'auto-menu-select-item)
;;     map))

;; ;; auto menu select mode
;; (defun auto-menu-select-mode ()
;;   "Major mode for displaying auto menu select menus."
;;   ;;(interactive)
;;   (kill-all-local-variables)
;;   (setq major-mode 'auto-menu-select-mode)
;;   (setq mode-name "Auto Menu Select")
;;   (use-local-map auto-menu-select-mode-map)
;;   (setq buffer-read-only t)
;;   (run-hooks 'auto-menu-select-mode-hook))

;; (defun auto-menu-select-mode-move (direction)
;;   "Move to next menu item in select menu.

;; If DIRECTION is a positive integer, move that many items forward.
;; If DIRECTION is a negative integer, move that many items backward.
;; DIRECTION defaults to 1."
;;   ;; TODO: code this
;;   (goto-char (point-at-bol))
;;   (set-mark (point))
;;   (goto-char (point-at-eol)))

;; ;; auto menu select print
;; (defun auto-menu-select-print (name items)
;;   "Print menu called NAME of ITEMS.

;; Imbed item actions in text properties."
;;   ;;(interactive "*")
;;   ;; make sure cursor is at the start of a new line
;;   (when (not (point-at-bol))
;;     (goto-char (point-at-eol))
;;     (newline))
;;   ;; add header
;;   (insert (concat "= " name " ="))
;;   (newline)
;;   (newline)
;;   ;; interrogate items and print menu
;;   (dolist (item items)
;;     (let ((name (car item))
;;           (item (cadr item))
;;           action)
;;       (if (and (listp item)
;;                (listp (car item))
;;                (stringp (caar item))
;;                (listp (cadar item)))
;;           ;; sub-menu
;;           (setq action `(lambda () (auto-menu-select ,name (quote ,item))))
;;         ;; everything else is just added as-is
;;         (setq action item))
;;       ;; add item name and properties to menu
;;       (let ((beg (point)))
;;         (insert name)
;;         (add-text-properties beg (point)
;;                              (list auto-menu-select-text-property
;;                                    action))
;;         (newline)))))

;; ;; auto menu select item
;; (defun auto-menu-select-item ()
;;   "Get ACTION from text property
;; `auto-menu-select-text-property', kill current (menu) buffer,
;; then execute ACTION."
;;   (interactive)
;;   ;; get properties
;;   (let ((action (get-text-property (point) auto-menu-select-text-property)))
;;     ;;(message "action: %S" action)
;;     ;; continue if action looks good
;;     (when (fboundp action)
;;       ;; kill menu buffer
;;       (kill-buffer nil)
;;       ;; perform action
;;       (funcall action))))

;; ;; auto menu select
;; (defun auto-menu-select (name items)
;;   "Create a menu called NAME consisting of ITEMS and prompt the
;; user to select one.

;; ITEMS is a list of items.  An ITEM is either a list containing
;; the following elements that define a menu item:

;;   NAME     is the menu item name.
;;   SYMBOL   either a VARIABLE, FUNCTION, or STRING.

;; and SYMBOL is one of:

;;   VARIABLE is a lisp variable.
;;   FUNCTION is either a function name or a lambda definition.
;;   STRING   is a string to be returned verbatim.

;; or an ITEM is a named list (a string and a list) defining a
;; sub-menu where:

;;   NAME  is the sub-menu name.
;;   ITEMS is a list of items."
;;   (interactive)
;;   (let (buffer)                         ; menu buffer
;;     ;; setup auto-menu-select-buffer-name buffer
;;     (setq buffer (generate-new-buffer auto-menu-select-buffer-name))
;;     (set-buffer buffer)
;;     ;;(setq buffer-read-only nil)
;;     ;;(erase-buffer)
;;     ;; call auto menu print function
;;     (auto-menu-select-print name items)
;;     ;; set auto menu buffer to read-only
;;     (setq buffer-read-only t)
;;     ;; more setup
;;     (switch-to-buffer buffer)
;;     (goto-char (point-min))
;;     (auto-menu-select-mode)
;;     (forward-line 2)))

;; ;; auto menu select print
;; (defun auto-menu-select-print (name items)
;;   "Print menu called NAME of ITEMS."
;;   ;;(interactive "*")
;;   ;; make sure cursor is at the start of a new line
;;   (when (not (point-at-bol))
;;     (widget-insert "\n"))
;;   ;; add header
;;   (widget-insert (concat "= " name " =\n\n"))
;;   ;; interrogate items and print menu
;;   (dolist (item items)
;;     (let ((name (car item))
;;           (item (cadr item))
;;           action)
;;       (if (and (listp item)
;;                (listp (car item))
;;                (stringp (caar item))
;;                (listp (cadar item)))
;;           ;; sub-menu
;;           (setq action `(lambda () (auto-menu-select ,name (quote ,item))))
;;         ;; everything else is just added as-is
;;         (setq action item))
;;       ;; create and add widget to menu
;;       ;;(widget-insert "  ")
;;       (widget-create 'push-button
;;                      :value name
;;                      :notify (lambda (&rest ignore)
;;                                (kill-buffer nil)
;;                                (funcall action)))
;;       (widget-insert "\n"))))

;; auto menu select
;;;###autoload
(defun auto-menu-select (name items)
  "Create a menu called NAME consisting of ITEMS,
and prompt the user to select one.

ITEMS is a list of items.  An ITEM is either a list containing
the following elements that define a menu item:

  NAME   is the menu item name.
  SYMBOL either a VARIABLE, FUNCTION, or STRING.

and SYMBOL is one of:

  VARIABLE is a lisp variable.
  FUNCTION is either a function name or a lambda definition.
  STRING   is a string to be returned verbatim.

or an ITEM is a named list (a string and a list) defining a
sub-menu where:

  NAME  is the sub-menu name.
  ITEMS is a list of items."
  (interactive)
  (let (buffer)                         ; menu buffer
    ;; setup auto-menu-select-buffer-name buffer
    (setq buffer (generate-new-buffer auto-menu-select-buffer-name))
    (set-buffer buffer)
    (kill-all-local-variables)
    ;;(setq buffer-read-only nil)
    ;;(erase-buffer)
    ;; add header
    (widget-insert (concat name "\n\n"))
    ;; interrogate items and print menu
    (dolist (item items)
      (let ((name (car item))
            (item (cadr item))
            action)
        (if (and (listp item)
                 (listp (car item))
                 (stringp (caar item))
                 (listp (cadar item)))
            ;; sub-menu
            (setq action `(lambda () (auto-menu-select ,name (quote ,item))))
          ;; everything else is just added as-is
          (setq action item))
        ;; create and add widget to menu
        ;;(widget-insert "  ")
        (widget-create 'push-button
                       :value name
                       :notify `(lambda (&rest ignore)
                                  (kill-buffer nil)
                                  (if (fboundp ,action)
                                      (funcall ,action)
                                    ,action)))
        (widget-insert "\n")))
    ;; final setup
    ;;(setq buffer-read-only t)
    (use-local-map widget-keymap)
    (widget-setup)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (widget-forward 1)))

;; auto menu file dir
;;;###autoload
(defun auto-menu-file-dir (dir &optional match funct recurse updir)
  "Return an auto-menu items list containing an item for every
file in DIR that matches the regexp MATCH (defaults to `.*') with
FUNCT applied to it (defaults to `find-file').

If RECURSE is non-nil sub-directories will be recursed (defaults
to nil).
UPDIR is for internal use."
  ;;(interactive "DDirectory: ")
  ;; expand dir to full path
  (setq dir (expand-file-name (file-truename dir)))
  ;; make sure dir is a directory
  ;; (unless (file-directory-p dir)
  ;;   (error (format "`%s' is not a directory" dir)))
  (when (file-directory-p dir)
    (let ((match (or match ".*"))
          (funct (or funct "find-file"))
          items                                       ; items list to populate
          (files (directory-files dir t)))            ; files in dir
      ;; loop through files
      (dolist (file files)
        ;; is item accessable?
        (when (file-readable-p file)
          (let ((base-name (file-name-nondirectory file)))
            ;; branch on type of item
            (cond
             ;; ignore `.' and `..'
             ((string-match "^\\.\\.?$" base-name)
              t)
             ;; directory
             ((file-directory-p file)
              (when recurse
                ;; ignore `.git' and `.svn'
                (when (not (or
                            (string-match "^\\.git$" base-name)
                            (string-match "^\\.svn$" base-name)))
                  (push (list (concat base-name " (dir)")
                              (auto-menu-file-dir file match funct recurse dir)) items)))
              t)
             ;; matching file (add to menu)
             ((string-match match file)
              ;;(let ((file-name (file-name-sans-extension base-name)))
              (let ((file-name base-name))
                ;; add menu item
                (push (list
                       file-name
                       (concat "(" funct " \"" file "\")")
                       (concat "Apply `" funct "' to `" file "' file."))
                      items))
              t)))))
      (nreverse items))))

;; TODO: refactor the following two functions into a macro that generates them

;; auto menu file
;;;###autoload
(defun auto-menu-file (files)
  "Return an auto-menu items list containing an item for every file in FILES.

FILES is either a list of files an association list containing
name/file pairs in this format:

  ((NAME . FILE) ...)"
  (if (listp (car files))
      ;; handle alist
      (mapcar (lambda (item)
                (let ((name (car item))
                      (file (file-truename (cdr item))))
                  (list name
                        (concat "(find-file \"" file "\")")
                        (concat "Load `" file "' file."))))
              files)
    ;; handle list
    (mapcar (lambda (item)
              (let ((file (file-truename (car item))))
                (list file
                      (concat "(find-file \"" file "\")")
                      (concat "Load `" file "' file."))))
            files)))

;; auto menu dired
;;;###autoload
(defun auto-menu-dired (dirs)
  "Return an auto-menu items list containing an item for every directory in DIRS.

DIRS is either a list of directories or an association list
containing name/directory pairs in this format:

  ((NAME . DIR) ...)"
  (if (listp (car dirs))
      ;; handle alist
      (mapcar (lambda (item)
                (let ((name (car item))
                      (dir (file-truename (cdr item))))
                  (list name
                        (concat "(dired \"" dir "\")")
                        (concat "Open dired buffer at `" dir "'."))))
              dirs)
    ;; handle list
    (mapcar (lambda (item)
              (let ((dir (file-truename (car item))))
                (list dir
                      (concat "(dired \"" dir "\")")
                      (concat "Open dired buffer at `" dir "'."))))
            dirs)))

;; auto menu dired remote
;;;###autoload
(defun auto-menu-dired-remote (users servers)
  "Return an auto-menu items list containing a submenu for every
server directory in SERVERS, each of which contains a list of
USERS.

USERS is an association list of users and directories in this format:

  ((NAME . DIR)
   (NAME . (DIR1 DIR2 ...))

SERVERS is a list of servers."
  (let (result)
    (dolist (server servers)
      (push (list server
                  (do* ((user-dir users (cdr user-dir))
                        (user (caar user-dir) (caar user-dir))
                        (dir (cdar user-dir) (cdar user-dir))
                        result)
                      ((not user-dir) (nreverse result))
                    (unless (listp dir)
                      (setq dir (list dir)))
                    (dolist (d dir)
                      (push (list (concat user "@" server)
                                  (concat "(dired \"/" user "@" server ":" d "\")")
                                  (concat "Open dired buffer at `" user "@" server ":" d "'."))
                            result))))
            result))
    (nreverse result)))

;;; Tests:

;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test1$$$")
;; (auto-menu
;;  "$$$Test1$$$"
;;  '(("Test" "(message \"Hello Test\")" "Print msg to minibuffer.")))
;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test1$$$")

;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test2$$$")
;; (auto-menu
;;  "$$$Test2$$$"
;;  '(("SubMenu"
;;     (("Test" "(message \"Hello Test\")" "Print msg to minibuffer.")))))
;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test2$$$")

;; (auto-menu-map
;;  "$$$Test1$$$"
;;  '(("SubMenu"
;;     (("Test" "(message \"Hello Test\")" "Print msg to minibuffer.")))))

;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test2$$$")
;; (auto-menu-map
;;  "$$$Test2$$$"
;;  '(("SubMenu"
;;     (("Test" "(message \"Hello Test\")" "Print msg to minibuffer."))))
;;  t)
;; (easy-menu-remove-item (current-global-map) '("menu-bar") "$$$Test2$$$")

(provide 'auto-menu)

;;; auto-menu.el ends here
