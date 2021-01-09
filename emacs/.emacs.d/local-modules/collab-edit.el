;;; collab-edit.el --- Collaborative Editing
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-03-04
;; Version:  0.1
;; Keywords: collaborative edit
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
;; Provides `collab-edit' function that ... ???
;;
;;; Installation:
;;
;; Put `collab-edit.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'collab-edit)
;;
;;; Usage:
;;
;;
;; Example:
;;
;;   (collab-edit ... ???

;;; Code:

;; use erc for irc connectivity
(require 'erc)

;; customize group
(defgroup collab-edit nil
  "Collaborative editing."
  :prefix "collab-edit-"
  :group 'programming)

;; irc server
(defcustom collab-edit-irc-server
  "localhost"
  "IRC server to use."
  :type 'string
  :group 'collab-edit)

;; irc port
(defcustom collab-edit-irc-port
  "6667"
  "IRC port to use."
  :type 'string
  :group 'collab-edit)

;; irc nick
(defcustom collab-edit-irc-nick
  (or user-login-name
      "nick")
  "IRC nickname to use."
  :type 'string
  :group 'collab-edit)

;; irc password
(defcustom collab-edit-irc-password
  nil
  "IRC password to use."
  :type 'string
  :group 'collab-edit)

;; irc name
(defcustom collab-edit-irc-name
  (or user-full-name
      "Full Name")
  "IRC name to use."
  :type 'string
  :group 'collab-edit)

;; irc channel prefix
(defcustom collab-edit-irc-channel-prefix
  "#collab"
  "IRC channel prefix to use."
  :type 'string
  :group 'collab-edit)

;; local irc settings
(defvar collab-edit-local-irc-settings
  '((:server . nil)
    (:port . nil)
    (:nick . nil)
    (:password . nil)
    (:name . nil)
    (:channel-prefix . nil))
  "Buffer local IRC settings association list.")
(make-variable-buffer-local 'collab-edit-local-irc-settings)

;; get value
(defmacro collab-edit-get-value (list key)
  "Return value in association LIST for KEY."
  `(cadr (assq ,key ,list)))

;; set value
(defmacro collab-edit-set-value (list key value)
  "Assign VALUE to KEY in association LIST."
  `(setf (cadr (assq ,key ,list)) ,value))

;; local irc server buffer name
(defvar collab-edit-local-irc-server-buffer-name
  nil
  "Buffer local irc server buffer name.")
(make-variable-buffer-local 'collab-edit-local-irc-server-buffer-name)

;; local irc channel buffer name
(defvar collab-edit-local-irc-channel-buffer-name
  nil
  "Buffer local irc channel buffer name.")
(make-variable-buffer-local 'collab-edit-local-irc-channel-buffer-name)

;; local id
(defvar collab-edit-local-id
  nil
  "Buffer local unique id.")
(make-variable-buffer-local 'collab-edit-local-id)

;; local authentication list
(defvar collab-edit-local-auth
  nil
  "Buffer local authentication list.
\nUsers listed in this list are allowed to edit the buffer.
Everyone in the irc channel is allowed to read the buffer.")
(make-variable-buffer-local 'collab-edit-local-auth)

;; local incoming queue
(defvar collab-edit-local-queue-input
  nil
  "Buffer local queue to hold incoming change requests.")
(make-variable-buffer-local 'collab-edit-input)

;; local outgoing queue
(defvar collab-edit-local-queue-output
  nil
  "Buffer local queue to hold processed change requests.")
(make-variable-buffer-local 'collab-edit-output)

;; generate uuid
(defun collab-edit-uuid ()
  "Generate a UUID."
  (replace-regexp-in-string
   "^ +\\|[ \n]+$" ""
   (shell-command-to-string "mcookie")))

;; generate irc server buffer name
(defun collab-edit-irc-server-buffer-name (&optional server port)
  "Return generated irc server buffer name in the form of \"SERVER:PORT\".
\nSERVER and PORT are determined using the priority of optional
functions parameters first, local values second, and global
values last."
  (concat (or server (collab-edit-get-value collab-edit-local-irc-settings :server) collab-edit-irc-server)
          ":" (or port (collab-edit-get-value collab-edit-local-irc-settings :port) collab-edit-irc-port)))

;; generate irc channel buffer name
(defun collab-edit-irc-channel-buffer-name (&optional prefix nick document)
  "Return generated irc channel name in the form of \"#prefix-nick-document\".
\nCHANNEL is determined using the priority of optional function
parameters first, local values second, and global values last."
  (concat (or prefix (collab-edit-get-value collab-edit-local-irc-settings :channel-prefix) collab-edit-irc-channel-prefix)
          "-" (or nick (collab-edit-get-value collab-edit-local-irc-settings :nick) collab-edit-irc-nick)
          "-" (or document (buffer-name))))

;; connect to irc server
(defun* collab-edit-connect (&key server port nick password name force)
  "Connect to an irc server via `erc' if not already connected.
\nIf FORCE is non-nil, then kill existing connection and reconnect.
If any other keyword parameters are given, the corresponding
custom variable is also set."
  (let ((server-buffer-name (collab-edit-irc-server-buffer-name server port))
        (buffer (current-buffer)))
    (save-excursion
      ;; kill existing irc buffer if it exists and force is non-nil
      (when (and force (get-buffer server-buffer-name))
        (kill-buffer server-buffer-name))
      ;; connect to irc server if not already connected
      (unless (get-buffer server-buffer-name)
        ;; set custom vars if caller supplies them
        (when server (collab-edit-set-value collab-edit-local-irc-settings :server server))
        (when port (collab-edit-set-value collab-edit-local-irc-settings :port port))
        (when nick (collab-edit-set-value collab-edit-local-irc-settings :nick nick))
        (when password (collab-edit-set-value collab-edit-local-irc-settings :password password))
        (when name (collab-edit-set-value collab-edit-local-irc-settings :name name))
        ;; connect to irc server
        (erc :server (collab-edit-get-value collab-edit-local-irc-settings :server)
             :port (collab-edit-get-value collab-edit-local-irc-settings :port)
             :nick (collab-edit-get-value collab-edit-local-irc-settings :nick)
             :password (collab-edit-get-value collab-edit-local-irc-settings :password)
             :full-name (collab-edit-get-value collab-edit-local-irc-settings :name))
        ;; forcefully return to original buffer
        (switch-to-buffer buffer)))
    ;; make sure server is up
    (if (get-buffer server-buffer-name)
        (progn
          (set-buffer server-buffer-name)
          ;; set buffer local vars from server settings
          (collab-edit-set-value collab-edit-local-irc-settings :server (erc-compute-server))
          (collab-edit-set-value collab-edit-local-irc-settings :port (erc-compute-port))
          (collab-edit-set-value collab-edit-local-irc-settings :nick (erc-compute-nick))
          (collab-edit-set-value collab-edit-local-irc-settings :password (erc-compute-nick))
          (collab-edit-set-value collab-edit-local-irc-settings :name (erc-compute-full-name))
          ;; set buffer local irc server buffer name var
          (setq collab-edit-local-irc-server-buffer-name server-buffer-name))
      (error (format "Could not get irc server information from erc")))))

;; create channel for buffer
(defun* collab-edit-join-channel (&optional channel key &key force)
  "Join CHANNEL with optional security KEY.
\nIf FORCE is non-nil, then part existing channel and rejoin."
  (let ((channel-buffer-name (collab-edit-irc-channel-buffer-name channel))
        (buffer (current-buffer)))
    ;; make sure local irc server buffer exists
    (unless (get-buffer collab-edit-local-irc-server-buffer-name)
      (error "Cannot join channel until an irc server connection has been made with `collab-edit-connect'"))
    (save-excursion
      ;; kill existing irc channel if it exists and force is non-nil
      (when (and force (get-buffer channel-buffer-name))
        (kill-buffer channel-buffer-name))
      ;; connect to irc channel if not already connected
      (unless (get-buffer channel-buffer-name)
        ;; set custom vars if caller supplies them
        (when channel (collab-edit-set-value collab-edit-local-irc-settings :channel-prefix channel))
        ;; set buffer local vars from custom if not set
        (unless (collab-edit-get-value collab-edit-local-irc-settings :channel-prefix)
          (collab-edit-set-value collab-edit-local-irc-settings :channel-prefix collab-edit-irc-channel-prefix))
        ;; switch to irc server buffer
        (set-buffer collab-edit-local-irc-server-buffer-name)
        ;; connect to irc channel
        (erc-join-channel channel-buffer-name key)
        ;; pause to give erc time to join channel
        ;; TODO: find a better way to do this
        (sit-for 0.5 t)
        ;; ;; set buffer local vars
        ;; (setq collab-edit-local-irc-channel-buffer-name channel-buffer-name)
        ;; forcefully return to original buffer
        (switch-to-buffer buffer)))
    ;; set buffer local irc channel buffer var
    (if (get-buffer channel-buffer-name)
        (setq collab-edit-local-irc-channel-buffer-name channel-buffer-name)
      (error "Could not join irc channel"))))

;; send message
(defun collab-edit-send-message (buffer line &optional user)
  "Send LINE to the erc channel in BUFFER or to USER (if non-nil)."
  (save-excursion
    (if user
        ;; switch to user buffer
        ;; TODO: add user logic
        nil
      ;; switch to collab channel buffer
      (set-buffer buffer))
    ;; send message
    (erc-send-message line)))

;; start collaborative editing with a buffer
;; create:
;;   a buffer local var to hold a guid to use for all transactions
;;   a buffer local list of users allowed to modify buffer
;;   a buffer local change queue (input) that is written to via an erc callback
;;   a buffer local change queue (output) that is written to via a local process
;; the last two are used to make sure all changes occur in the correct order
;; when idle or any local change occurs the incoming queue is checked and processed in fifo order
;; local changes are added to the output queue (and to the input queue, via the erc callback as they are published to the irc channel)
;; if the processed queue is ever in a different order than the input queue, the buffer changes are rolled-back to a known correct state, then the input queue changes are processed
;; when items in the output queue match the input queue, they get popped off of both
(defun collab-edit-buffer (&optional buffer)
  "Start collaborative editing with BUFFER (or current buffer if nil)."
  (let ((buffer (or buffer (current-buffer))))
    ;; connect to server, if needed
    (collab-edit-connect)
    ;; join channel, if needed
    (collab-edit-join-channel)
    ;; create buffer local id
    (setq collab-edit-local-id (collab-edit-uuid))
    ;; create buffer local auth list
    (setq collab-edit-local-auth nil)
    ;; create local incoming change queue
    (setq collab-edit-local-queue-input nil)
    ;; create local outgoing change queue
    (setq collab-edit-local-queue-output nil)
    ;; set erc callback hook
    ;; setup idle/movement/change process
    ))

;; process queues
(defun collab-edit-process-queues ()
  "Check queues for changes to be applied to the curent buffer, and process them.
\nQueues are stored in `collab-edit-local-queue-input' and
`collab-edit-local-queue-output' "
  ;; if output queue has entries, only continue when they appear in input queue
  ;; a counter needs to be used here to detect errors when a change never appears in the input queue
  ;; if an error does occur, attempt to reconnect to irc and submit changes again
  ;; or inform user and clear auth list

  ;; if input queue has entries, compare with output queue
  ;; if matches are found, remove from both queues
  ;; if output queue is empty, process input queue change
  )


    ;; (run-with-idle-timer 0.5 t
    ;;                      (lambda ()
    ;;                        (let ((count (number-to-string (count-words-paragraph))))
    ;;                          (while (< (length count) 3)
    ;;                            (setq count (concat count " ")))
    ;;                          (setq count-words-paragraph count)))))


;; (defvar resume-timer nil
;;             "Timer that `timer-function' used to reschedule itself, or nil.")

;;           (defun timer-function ()
;;             ;; If the user types a command while resume-timer
;;             ;; is active, the next time this function is called from
;;             ;; its main idle timer, deactivate resume-timer.
;;             (when resume-timer
;;               (cancel-timer resume-timer))
;;             ...do the work for a while...
;;             (when taking-a-break
;;               (setq resume-timer
;;                     (run-with-idle-timer
;;                       ;; Compute an idle time break-length
;;                       ;; more than the current value.
;;                       (time-add (current-idle-time)
;;                                 (seconds-to-time break-length))
;;                       nil
;;                       'timer-function))))


;;; Tests:

;; (collab-edit-connect :server "irc.win.dowjones.net" :nick "kyletest" :name "Kyle Sherman")
;; (collab-edit-join-channel)

;;; Prototypes:

;; proto 1

(defvar collab-edit-proto1-buffer-name
  "*Collab-Edit-Change-Report*"
  "Buffer name to use for prototype 1.")

(defun collab-edit-proto1 ()
  "Prototype 1."
  (let ((nick "test")
        id
        buffer)
    ;; create buffer local id
    (setq id (collab-edit-uuid))
    ;; setup buffer
    (setq buffer (generate-new-buffer collab-edit-proto1-buffer-name))
    ;; add change tracking function
    (add-to-list 'after-change-functions
                 `(lambda (beg end len)
                    (let ((text (buffer-substring begin end)))
                      ;; a change is a list like this: '(uuid user begin end len text)
                      (message (format "%S" (list ,id ,nick begin end len text)))
                      (collab-edit-send-message ,collab-edit-proto1-buffer-name
                                                (format "%S" (list ,id ,nick begin end len text))))))))

;; proto 2

(defun collab-edit-proto2 ()
  "Prototype 2."
  ;; connect to local irc server
  (collab-edit-connect :server "localhost" :port "6667")
  ;; connect to work irc server
  ;;(collab-edit-connect :server "irc.win.dowjones.net" :port "6667")
  ;; connect to channel
  (collab-edit-join-channel)
  (let ((nick (collab-edit-get-value collab-edit-local-irc-settings :nick)))
    ;; create buffer local id
    (setq collab-edit-local-id (collab-edit-uuid))
    (message (concat "collab-edit-local-irc-channel-buffer-name: " collab-edit-local-irc-channel-buffer-name))
    (message (concat "collab-edit-local-id: " collab-edit-local-id))
    (message (concat "collab-edit-local-irc-nick: " nick))
    ;; add change tracking function
    ;;(add-to-list 'after-change-functions 'collab-edit-proto2-change))
    (add-to-list 'after-change-functions
                 `(lambda (beg end len)
                    ;; report on all buffer changes to irc
                    (let ((text (buffer-substring beg end)))
                      ;; a change is a list like this: '(uuid user beg end len text)
                      (message (format "%S" (list ,collab-edit-local-id ,nick beg end len text)))
                      (collab-edit-send-message ,collab-edit-local-irc-channel-buffer-name
                                                (format "%S" (list ,collab-edit-local-id ,nick beg end len text))))))))

(provide 'collab-edit)

;;; collab-edit.el ends here
