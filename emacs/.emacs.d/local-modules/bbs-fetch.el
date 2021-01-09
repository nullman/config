;;; bbs-fetch.el --- BBS Login and Fetch to org-mode
;;
;;; Copyright (C) 2020 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2020-09-20
;; Version:  1.0
;; Keywords: bbs telnet
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
;; ???
;; Provides `bbs-fetch', `bbs-fetch-select', `bbs-fetch-file-dir',
;; `bbs-fetch-dired', and `bbs-fetch-dired-remote' functions to help creating
;; Emacs menus.
;;
;;; Installation:
;;
;; Put `bbs-fetch.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'bbs-fetch)
;;
;;; Usage:
;;
;; ???
;; The `bbs-fetch' function creates a new menu.  You pass it a name and a list
;; of items.  Each item is either: 1) a list contianing a name, function, and
;; help text, or 2) a sub-menu containing a name and a list of items.  This
;; functionality recurses so you can have n-depth sub-menus.
;;
;; The `bbs-fetch-select' function is similar to `bbs-fetch' except that it
;; opens the menu in a buffer and prompts the user to select an item.  It also
;; does not use keymaps and can handle lambda functions.  This function is
;; good to use in other functions that need to prompt the user with a list of
;; options to select from.
;;
;; The `bbs-fetch-file-dir' function creates a new menu based on a given
;; directory.  It creates an entry for every file matching a pattern (defaults
;; to `.*') and applies a function to it (defaults to `find-file').  There is
;; also an option to recurse through sub-directories or not (defaults to no).
;;
;; The `bbs-fetch-file' function creates a new menu of `find-file' commands.
;; It creates an entry for every file given.
;;
;; The `bbs-fetch-dired' function creates a new menu of `dired' commands.  It
;; creates an entry for every directory given.
;;
;; The `bbs-fetch-dired-remote' function creates a new menu of `dired'
;; commands to connect with remote servers.  It takes a list of servers and
;; users and creates a menu of servers that each lead to sub-menus of users
;; that when selected will open a dired buffer at that location.
;;
;; Some examples from my configuration (edited for size):
;;
;;   ;; dired menu
;;   (bbs-fetch
;;    "Dired"
;;    (bbs-fetch-dired '(("home" . "~/")
;;                       (".emacs.d" . "~/.emacs.d")
;;                       (".elisp" . "~/.elisp")
;;                       ("clojure" . "~/clojure")
;;                       ("clisp" . "~/clisp")
;;                       ("bin" . "~/bin"))))
;;
;;   ;; load menu
;;   (bbs-fetch
;;    "Load"
;;    `(("Restore Context" "(context-restore)" "Restore previous context save.")
;;      ("Home Files..."
;;       ,(bbs-fetch-file '((".profile" . "~/.profile")
;;                          (".bashrc" . "~/.bashrc"))))
;;      ("Emacs Settings..."
;;       ,(append '((".emacs" "(find-file \"~/.emacs\")" "Load `~/.emacs' file."))
;;                (bbs-fetch-file-dir "~/.emacs.d" "\\.el$" "find-file")))
;;      ("Elisp Files..."
;;       ,(bbs-fetch-file-dir "~/.emacs.d" "\\.el$" "find-file" t))
;;      ("Clojure Files..."
;;       ,(bbs-fetch-file-dir "~/clojure" "\\.clj$" "find-file" t))
;;      ("CLisp Files..."
;;       ,(bbs-fetch-file-dir "~/clisp" "\\.lisp$" "find-file" t))
;;      ("Org Files..."
;;       ,(bbs-fetch-file-dir "~/org" "\\.\\(org\\|org\\.cpt\\)$" "find-file" t))))
;;
;;   ;; run menu
;;   (bbs-fetch
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

;;; Code:

(defun l29-get-last-message-id ()
  "Return last message id in archive."
  (let ((buffer-name "level-29-bbs.org"))
    (unless (string= (buffer-name) buffer-name)
      (error "Buffer is not '%s'" buffer-name))
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (unless (re-search-forward "^[ \t]*:CUSTOM_ID: messages$" nil :noerror)
          (error "Messages section not found in buffer '%s'!" buffer-name))
        (goto-char (point-max))
        (if (re-search-backward "^[ \t]*:CUSTOM_ID: message-\\([0-9]+\\)$" nil :noerror)
            (string-to-number (match-string 1))
          0)))))

(defun l29-send-string (process string &optional with-cr)
  "Send STRING to `comint' PROCESS in a way that works with telnet.

  If WITH-CR is non-nil, then a carrage return will be added to the end."
  (process-send-string process (concat string (if with-cr "\r" ""))))

(defun l29-parse-message (archive-buffer)
  "Get message in current buffer and add it to ARCHIVE-BUFFER.

  POINT should be at the end of the message.

  If the message is a reply, then it will be inserted as a
  subheading under the original message.

  Message header should look like:

  Message #NUM
  Subject:     SUBJECT
               SUBJECT CAN WRAP [optional]
  From:        NAME
  Date:        YYYY-MM-DD HH:MM:SS
  Connection:  DESCRIPTION [optional]
  Replying to: NAME, message #NUM [optional]

  MESSAGE

  Select: ..."
  (save-mark-and-excursion
    (let ((end (progn
                 (forward-line -1)
                 (point))))
      (when (re-search-backward "^==========*\n\nMessage" nil :noerror)
        (forward-line 2)
        (when (looking-at "^Message #\\([0-9]+\\)$")
          (let ((message-id (string-to-number (match-string-no-properties 1)))
                (start (point))
                properties)
            (forward-line 1)
            (while (re-search-forward "^\\(.*?\\):[ \t]*\\(.*\\)$" (point-at-eol) :noerror)
              (let ((property (upcase (replace-regexp-in-string " " "_" (match-string-no-properties 1))))
                    (value (match-string-no-properties 2)))
                (forward-line 1)
                (while (looking-at "^[\t ]+\\(.*\\)$")
                  (setq value (concat value " " (match-string-no-properties 1)))
                  (forward-line 1))
                (push (cons property value) properties)))
            (forward-line 1)
            (let* ((message (buffer-substring-no-properties start end))
                   (subject (remove-if-not (lambda (x) (string= (car x) "SUBJECT")) properties))
                   (reply (remove-if-not (lambda (x) (string= (car x) "REPLYING_TO")) properties))
                   (reply-id (and reply
                                  (string-to-number
                                   (replace-regexp-in-string "^.* message #" "" (cdar reply))))))
              (with-current-buffer archive-buffer
                (goto-char (point-max))
                (let* ((reply-indent
                        (if reply-id
                            (progn
                              (re-search-backward (format ":CUSTOM_ID: message-%d$" reply-id))
                              (org-show-entry)
                              (- (+ (point) 2) (point-at-bol)))
                          4))
                       (header (make-string (1- reply-indent) ?*))
                       (spacer (make-string reply-indent ? )))
                  (when reply-id
                    (let ((pos (point)))
                      (org-forward-heading-same-level 1 :invisible-ok)
                      (while (and (< (point) pos)
                                  (> (org-outline-level) 1))
                        (outline-up-heading 1 :invisible-ok)
                        (org-forward-heading-same-level 1 :invisible-ok)))
                    (if (= (org-outline-level) 1)
                        (goto-char (point-max))
                      (forward-line -1)))
                  (newline)
                  (insert (format "%s %d%s\n" header message-id (if subject (concat " - " (cdar subject)) "")))
                  (insert (format "%s:PROPERTIES:\n" spacer))
                  (insert (format "%s:CUSTOM_ID: message-%d\n" spacer message-id))
                  (mapcar (lambda (x) (insert (format "%s:%s: %s\n" spacer (car x) (cdr x))))
                          (nreverse properties))
                  (insert (format "%s:END:\n" spacer))
                  (forward-line -1)
                  (org-indent-drawer)
                  (forward-line 1)
                  (newline)
                  (insert (replace-regexp-in-string "^\*" "," message)) ; lines starting with asterisk need to be escaped with a coma in org-mode
                  (let ((point (point)))
                    (while (invisible-p point)
                      (goto-char point)
                      (org-show-entry))))))
            (setq l29-last-message-id message-id)))))))

(defun l29-output-filter (process string)
  "Output incoming data to process buffer."
  (with-current-buffer (process-buffer process)
    (let* ((last-insertion (marker-position (process-mark process)))
           (delta (- (point) last-insertion))
           (input-end (and comint-last-input-end
                           (marker-position comint-last-input-end)))
           (window (get-buffer-window (current-buffer)))
           (window-start (and window (window-start window))))
      (goto-char last-insertion)
      (insert string)
      (set-marker comint-last-output-start last-insertion)
      (set-marker (process-mark process) (point))
      (when window-start
        (set-window-start window window-start :noforce))
      (when input-end
        (set-marker comint-last-input-end input-end))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
                    (> (point) last-insertion))
        (delete-region (1- (point)) (point)))
      (goto-char (process-mark process))
      (when (> delta 0)
        (goto-char (+ (process-mark process) delta))))))

(defun l29-state-filter (process string)
  "State based filter.

  Possible states:
    :login     - Perform initial login
    :init      - Start initial message fetching
    :init-next - Continue fetching initial messages
    :new       - Handle new message fetching
    :new-next  - Continuefetching new messages
    :logout    - Perform final logout

  BUFFER is used internally for recursive calls."
  (let* ((env (expand-file-name "~/.level-29-bbs-login"))
         (username (shell-command-to-string (concat "sed -n 's/username=//p' " env)))
         (password (shell-command-to-string (concat "sed -n 's/password=//p' " env)))
         (case-fold-search t))
    (with-current-buffer (process-buffer process)
      (l29-output-filter process string)
      (let ((point (point)))
        (goto-char (point-at-bol))
        (case l29-state
          ;; login and navigate to main prompt
          (:login
           (cond
            ((looking-at "^User: $")
             (goto-char point)
             (l29-send-string process username))
            ((looking-at "^Password: $")
             (goto-char point)
             (l29-send-string process password)
             (clear-this-command-keys))
            ((looking-at "^Terminal size .*: $")
             (goto-char point)
             (l29-send-string process "80x100" :with-cr))
            ((looking-at "^Terminal type .*: $")
             (goto-char point)
             (l29-send-string process "dumb" :with-cr))
            ((looking-at "^Character set .*: $")
             (goto-char point)
             (l29-send-string process "ASCII" :with-cr))
            ((looking-at "^Select: \\[.*\\] $")
             (goto-char point)
             ;; move to state based on l29-mode
             (setq l29-state l29-mode)
             (l29-state-filter process ""))))
          ;; check for linked messages not in this archive
          (:init
           (cond
            ((looking-at "^Select: \\[.*#.*\\] $")
             (goto-char point)
             (l29-send-string process "#"))
            ((looking-at "^Go to message number: $")
             (goto-char point)
             (l29-send-string
              process
              (number-to-string (if (< l29-last-message-id 2) 2 l29-last-message-id))
              :with-cr))
            ((looking-at "^Select: \\[\\(.*\\)\\] $")
             (let ((options (match-string-no-properties 1)))
               ;; if starting with an empty archive, parse the first message
               ;; otherwise, last-message-id will have already been parsed
               (when (< l29-last-message-id 2)
                 (l29-parse-message l29-archive-buffer))
               (goto-char point)
               (if (string-match "e" options)
                   (progn
                     (setq l29-state :init-next)
                     (l29-send-string process "e"))
                 (progn
                   (setq l29-state :new)
                   (l29-send-string process "q")))))))
          ;; parse all linked messages and add them to this archive
          (:init-next
           (cond
            ((looking-at "^Select: \\[\\(.*\\)\\] $")
             (let ((options (match-string-no-properties 1)))
               (l29-parse-message l29-archive-buffer)
               (goto-char point)
               (if (string-match "e" options)
                   (l29-send-string process "e")
                 (progn
                   (setq l29-state :new)
                   (l29-send-string process "q")))))))
          ;; check for new messages not in this archive
          (:new
           (cond
            ((looking-at "^Select: \\[\\(.*\\)\\] $")
             (let ((options (match-string-no-properties 1))
                   (messages (not (re-search-backward "There are 0 unread messages" nil :no-error))))
               (goto-char point)
               (if (and (string-match "n" options)
                        messages)
                   (progn
                     (setq l29-state :new-next)
                     (l29-send-string process "n"))
                 (progn
                   (setq l29-state :logout)
                   (if (string-match "q" options)
                       (l29-send-string process "q")
                     (l29-state-filter process ""))))))))
          ;; parse all new messages and add them to this archive
          (:new-next
           (cond
            ((looking-at "^Select: \\[\\(.*\\)\\] $")
             (let ((options (match-string-no-properties 1)))
               (l29-parse-message l29-archive-buffer)
               (goto-char point)
               (if (string-match "n" options)
                   (l29-send-string process "n")
                 (progn
                   (setq l29-state :logout)
                   (if (string-match "q" options)
                       (l29-send-string process "q")
                     (l29-state-filter process ""))))))))
          ;; manual
          (:manual
           (cond
            ((looking-at "^Select: \\[\\([^#]*\\)\\] $")
             (let ((options (match-string-no-properties 1)))
               (l29-parse-message l29-archive-buffer)
               (goto-char point)))
            (t
             (goto-char point))))
          ;; logout
          (:logout
           (cond
            ((looking-at "^Select: \\[\\(.*\\)\\] $")
             (goto-char point)
             (l29-send-string process "o"))
            ((looking-at "^Really log off\\? ")
             (goto-char point)
             (set-process-filter process #'l29-output-filter)
             (l29-send-string process "y"))))
          (otherwise
           (goto-char point)))))))

(defun l29-fetch-messages (mode)
  "Telnet into <<host>>, fetch new messages, and add them to this archive.

  Where MODE is one of:

    :init    Initialize archive by pulling down all linked messages
    :new     Pull down new messages
    :manual  Login then fetch any messages manually viewed by the user"
  (interactive)
  (let* ((host "<<host>>")
         (port <<port>>)
         (buffer-name "<<archive-buffer-name>>")
         (buffer (current-buffer))
         (comint-program "<<program>>")
         (comint-name "<<process-name>>")
         (comint-buffer "<<process-buffer-name>>")
         (last-message-id (l29-get-last-message-id)))
    (unless (string= (buffer-name) buffer-name)
      (error "Buffer is not '%s'" buffer-name))
    ;; delete leftover process, if there is one
    (when (get-process comint-name)
      (delete-process (get-process comint-name)))
    (make-comint-in-buffer comint-name comint-buffer comint-program)
    (switch-to-buffer-other-window comint-buffer)
    (let ((comint-process (get-buffer-process comint-buffer)))
      (with-current-buffer comint-buffer
        (set (make-local-variable 'l29-archive-buffer) buffer-name)
        (set (make-local-variable 'l29-last-message-id) last-message-id)
        (set (make-local-variable 'l29-mode) mode)
        (set (make-local-variable 'l29-state) :login)
        (set-process-filter comint-process #'l29-state-filter)
        (accept-process-output comint-process)
        (erase-buffer)
        (comint-simple-send comint-process (concat "open " host " " (number-to-string port)))))))

(defun l29-fetch-messages-new ()
  "Call `l29-fetch-messages' in init mode to pull down all linked
  messages.  This should only be run once."
  (interactive)
  (l29-fetch-messages :init))

(defun l29-fetch-messages-new ()
  "Call `l29-fetch-messages' in new mode to pull down new
  messages."
  (interactive)
  (l29-fetch-messages :new))

(defun l29-fetch-messages-manual ()
  "Call `l29-fetch-messages' in manual mode to login then fetch
  any messages manually viewed by the user."
  (interactive)
  (l29-fetch-messages :manual))

(defun l29-stop-process ()
  "Stop the `l29-fetch-messages' process."
  (interactive)
  (set-process-filter (get-buffer-process "<<process-buffer-name>>") #'l29-output-filter))

;; (defun l29-post-message (&optional reply-id)
;;   "Prompt user to write a message, then telnet into <<host>> and post it.

;; If optional REPLY-ID is non-nil, then reply to that message
;; instead of posting a new one."
;;   (interactive)
;;   (save-mark-and-excursion
;;   (let ((message
;;          (if reply-id
;;               (with-current-buffer archive-buffer
;;                 (goto-char (point-max))
;;              (let* ((reply-indent
;;                      (re-search-backward (format ":CUSTOM_ID: message-%d$" reply-id))
;;                      (org-show-entry)
;;                      (- (+ (point) 2) (point-at-bol)))

;;                          (header (make-string (1- reply-indent) ?*))
;;                          (spacer (make-string reply-indent ? )))
;;            (if reply-number
;;              (save-mark-and-excursion
;;                (goto-char (point-max))
;;                (re-search-backward "^[ \t]*:CUSTOM_ID: message-\\([0-9]+\\)$")
;;     (let ((x (1- (string-to-number (match-string-no-properties 1)))))
;;       (while (re-search-backward (format "^[ \t]*:CUSTOM_ID: message-%d$" x) nil :noerror)
;;         (goto-char (point-max))
;;         (setq x (1- x)))
;;       x)))

(defun l29-find-missing-messages ()
  "Find missing messages."
  (interactive)
  (let ((messages (make-hash-table :test 'eq))
        max
        missing
        (ignore '(6012 6014)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (re-search-forward "^[ \t]*:CUSTOM_ID: messages$")
      (while (re-search-forward "^[ \t]*:CUSTOM_ID: message-\\([0-9]+\\)$" nil :noerror)
        (let ((id (string-to-number (match-string-no-properties 1))))
          (puthash id id messages)
          (setq max id)))
      (do ((id 1 (1+ id)))
          ((> id max))
        (unless (or (gethash id messages)
                    (< id 4000) ; only start looking for missing ids greater than 4000
                    (member id ignore))
          (push id missing))))
    (let ((buffer "*Level 29 Missing Messages*"))
      ;; setup buffer
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; insert header
      (insert "Level 29 Missing Messages")
      (newline)
      (newline)
      ;; insert table header
      (insert "|----------|")
      (newline)
      (insert "| Messages |")
      (newline)
      (insert "|----------|")
      (newline)
      ;; insert data
      (seq-do
       (lambda (id)
         (insert (format "| %s |" id))
         (newline))
       (nreverse missing))
      ;; insert table footer
      (insert "|----------|")
      (newline)
      ;; set mode
      (org-mode)
      ;; align table
      (forward-line -2)
      (org-table-align)
      ;; set buffer to read-only
      (setq buffer-read-only t)
      ;; switch to buffer
      (switch-to-buffer buffer)
      (goto-char (point-min)))))

(defun l29-post-stats ()
  "Output statistics on posting frequency."
  (interactive)
  (let ((post-frequency (make-hash-table :test 'equal)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (re-search-forward "^[ \t]*:CUSTOM_ID: messages$")
      (while (re-search-forward "^[ \t]*:FROM: +\\(.*\\)$" nil :noerror)
        (let* ((user (match-string 1))
               (posts (or (gethash user post-frequency) 0)))
          (puthash user (1+ posts) post-frequency))))
    (let ((buffer "*Level 29 Post Statistics*"))
      ;; setup buffer
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; insert header
      (insert "Level 29 Post Statistics")
      (newline)
      (newline)
      ;; insert table header
      (insert "|------|-------|")
      (newline)
      (insert "| User | Posts |")
      (newline)
      (insert "|------|-------|")
      (newline)
      ;; insert data
      (maphash
       (lambda (key value)
         (insert (format "| %s | %s |" key value))
         (newline))
       post-frequency)
      ;; insert table footer
      (insert "|------|-------|")
      (newline)
      ;; set mode
      (org-mode)
      ;; align table
      (forward-line -2)
      (org-table-align)
      ;; sort table by number of posts, decrementing
      (org-table-next-field)
      (org-table-next-field)
      (org-table-sort-lines nil ?N)
      ;; set buffer to read-only
      (setq buffer-read-only t)
      ;; switch to buffer
      (switch-to-buffer buffer)
      (goto-char (point-min)))))

(provide 'bbs-fetch)

;;; bbs-fetch.el ends here
