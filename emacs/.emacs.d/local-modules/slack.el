;;; slack.el --- Slack client
;;
;;; Copyright (C) 2013-2015 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2013-12-10
;; Version:  0.1
;; Keywords: slack client
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
;; slack is a client for the slack service (http://slack.com/).
;;
;; It can be customized with the following command:
;;
;;   (customize-group "slack")
;;
;; `slack-auth' must be set to one or more of your teams and tokens found
;; here:
;;
;;   https://api.slack.com/
;;
;; If, like me, you store your customization.el file in a git repository, you
;; can instead do the following:
;;
;;   $ touch ~/.slack-auth && chmod 600 ~/.slack-auth
;;
;; Now edit this file and add something like the following:
;;
;;   (setq slack-auth
;;         '(("TEAM-1" . "TOKEN-1")
;;           ("TEAM-2" . "TOKEN-2")
;;           ("TEAM-N" . "TOKEN-N")))

;;; Code:

(require 'json)

;; customize group
(defgroup slack nil
  "Slack client."
  :prefix "slack-"
  :group 'applications)

;; slack buffer name
(defcustom slack-buffer-name
  "*Slack*"
  "Slack buffer name."
  :type 'string
  :group 'slack)

;; slack token
(defconst slack-auth-default-team nil)
(defconst slack-auth-default-token nil)
(defconst slack-auth-file-name (expand-file-name "~/.slack-auth"))
(defcustom slack-auth
  `((,slack-auth-default-team . ,slack-auth-default-token))
  "Slack authorizations."
  :type 'list
  :group 'slack)

;; slack api url
(defconst slack-url "https://slack.com/api/")

;; data
(defvar slack-team nil
  "Current slack team name.")
(defvar slack-token nil
  "Current slack token.")
(defvar slack-state
  '((:top . nil)
    (:teams . nil))
  "Current state of client.")
(defvar slack-data (make-hash-table)
  "Most recent data queried from server.")
(defvar slack-data-new nil
  "Newer server data to process.")

;; json true
(defmacro slack-json-true (val)
  "Return non-nil if val is not `eq' to ':json_false',
otherwise return nil."
  `(not (eq ,val :slack-json-false)))

;; json false
(defmacro slack-json-false (val)
  "Return non-nil if val is `eq' to ':json_false',
otherwise return nil."
  `(eq ,val :slack-json-false))

;; generate header
(defmacro slack-generate-header (text)
  "Return TEXT as a header."
  `(propertize ,text 'face 'bold))

;; submit query
(defun slack-query (callback type method &optional args)
  "Call CALLBACK with resulting JSON from submitting ARGS to slack METHOD."
  (let* ((url-request-method "GET")
         (args (concat "?token=" (url-hexify-string slack-token)
                       (mapconcat (lambda (arg)
                                    (concat (url-hexify-string (car arg))
                                            "="
                                            (url-hexify-string (cdr arg))))
                                  args
                                  "&")))
         (query (concat slack-url method args))
         url-http-end-of-headers)
    ;;(message query)
    (url-retrieve query `(lambda (status)
                           (goto-char url-http-end-of-headers)
                           ;;(message "%s" (json-read))))))
                           (funcall (function ,callback) ,type (json-read))))))

;; if cached data has changed, refresh window
(defun slack-data-new-set (type json)
  "Set data TYPE to JSON."
  (push (cons type json) slack-data-new)
  (when (slack-load-data-sets)
    (slack-refresh)))

;; load data and return changed status
(defun slack-load-data-sets ()
  "Process new data sets and return non-nil if a change was made."
  (let (change)
    (while (plusp (length slack-data-new))
      (let ((new (pop slack-data-new)))
        (let ((cur (gethash (car new) slack-data)))
          (when (cl-set-difference new cur)
            (setf (gethash (car new) slack-data) (cdr new)
                  change t)))))
    change))

;; auth list
(defun slack-auth-list (&optional team token)
  "Return list of authorizations (team token pairs)."
  (let* ((dups-list (append (and team token (cons team token))
                            (and slack-auth (car slack-auth) (caar slack-auth) slack-auth)
                            (and (file-exists-p slack-auth-file-name) (load slack-auth-file-name)
                                 slack-auth (car slack-auth) (caar slack-auth) slack-auth)))
         (auth-list (cl-do ((dups dups-list (cdr dups))
                            (auth-list))
                        ((not dups) auth-list)
                      (unless (member (caar dups) (mapcar #'car auth-list))
                        (push (car dups) auth-list)))))
    (nreverse auth-list)))

;; switch team
(defun slack-switch-team (&optional team token)
  "Switch to TEAM using TOKEN.
\nTEAM defaults to `slack-team'.
TOKEN will be looked up in `slack-auth-list' if not given."
  (let* ((team (or team slack-team))
         (token (or token (cdr (assoc team (slack-auth-list))) slack-token)))
    (when (or (not (equal team slack-team))
              (not (equal token slack-token)))
      (setq slack-team team
            slack-token token))
    (setcdr (assq :teams slack-state) nil) ; turn off show teams mode
    (slack-group-list)
    (slack-channel-list)))

;; ((groups . [])
;;  (ok . t))

;; ((channels . [((num_members . 1) (purpose (last_set . 0) (creator . ) (value . This channel is for team-wide communication and announcements. All team members are in this channel.)) (topic (last_set . 0) (creator . ) (value . )) (members . [U0321RGMR]) (is_member . t) (is_general . t) (is_archived . :slack-json-false) (creator . U0321RGMR) (created . 1416769379) (is_channel . t) (name . general) (id . C0321RGMX))
;;               ((num_members . 1) (purpose (last_set . 0) (creator . ) (value . A place for non-work banter, links, articles of interest, humor or anything else which you'd like concentrated in some place other than work-related channels.)) (topic (last_set . 0) (creator . ) (value . )) (members . [U0321RGMR]) (is_member . t) (is_general . :slack-json-false) (is_archived . :slack-json-false) (creator . U0321RGMR) (created . 1416769379) (is_channel . t) (name . random) (id . C0321RGMZ))
;;               ])
;;  (ok . t))

;; refresh slack window and widgets
(defun slack-refresh ()
  "Display data and interface according to `slack-state'."
  (save-excursion
    (cl-labels ((widget-create-team (team token &optional spaces)
                                    (widget-insert (make-string (or spaces 0) ? ))
                                    (widget-create 'push-button
                                                   :value (format "%s" team)
                                                   :notify `(lambda (&rest ignore)
                                                              (slack-switch-team ,team ,token)))
                                    (widget-insert "\n"))
                (widget-create-channel (name &optional spaces)
                                       (widget-insert (make-string (or spaces 0) ? ))
                                       (widget-create 'push-button
                                                      :value (format "%s" name)
                                                      :notify `(lambda (&rest ignore)
                                                                 (message "Channel: %s" ,name)))
                                       (widget-insert "\n")))
      ;; setup buffer
      (when (get-buffer slack-buffer-name)
        (kill-buffer slack-buffer-name))
      (let ((buffer (get-buffer-create slack-buffer-name))
            (channels-json (gethash :channel slack-data)))
        (set-buffer buffer)
        ;; add team header and selector
        (if (cdr (assq :teams slack-state))
            (progn
              ;; show team selection list
              (widget-insert (slack-generate-header "Teams"))
              (widget-insert "\n\n")
              (mapc (lambda (x) (widget-create-team (car x) (cdr x) 2)) (slack-auth-list))
              (widget-insert "\n"))
          (progn
            ;; show normal menu
            (widget-create 'push-button
                           :value slack-team
                           :notify (lambda (&rest ignore)
                                     (setcdr (assq :teams slack-state) t) ; turn on show teams mode
                                     (slack-refresh)))
            (widget-insert "\n\n")
            ;; add channels
            (widget-insert (slack-generate-header "Channels"))
            (widget-insert "\n\n")
            (if (slack-json-true (cdr (assoc 'ok channels-json)))
                (let* ((channels (cdr (assoc 'channels channels-json)))
                       (non-member-channels (cl-remove-if (lambda (x) (slack-json-false (cdr (assoc 'is_member x)))) channels))
                       (member-channels (cl-remove-if (lambda (x) (slack-json-true (cdr (assoc 'is_member x)))) channels)))
                  (when (and non-member-channels (plusp (length non-member-channels)))
                    (mapc (lambda (x) (widget-create-channel (cdr (assoc 'name x)) 2)) non-member-channels)
                    (widget-insert "\n"))
                  (when (and member-channels (plusp (length member-channels)))
                    (mapc (lambda (x) (widget-create-channel (cdr (assoc 'name x)) 2)) member-channels)
                    (widget-insert "\n")))
              (widget-insert (format "Error: %s\n\n" (cdr (assoc 'error channels-json)))))))
        ;; add refresh button
        (widget-create 'push-button
                       :value "Refresh"
                       :notify (lambda (&rest ignore)
                                 (slack-switch-team)))
        (widget-insert "\n")
        ;; add customize button
        (widget-create 'push-button
                       :value "Customize"
                       :notify (lambda (&rest ignore)
                                 (customize-group 'slack)))
        ;; final setup
        (use-local-map widget-keymap)
        (widget-setup)
        (switch-to-buffer buffer)
        (goto-char (point-min))
        (widget-forward 1)))))

;; query groups and refresh
(defun slack-group-list ()
  "Fetch list of groups."
  (slack-query 'slack-data-new-set :group "groups.list"))

;; query channels and refresh
(defun slack-channel-list ()
  "Fetch list of channels."
  (slack-query 'slack-data-new-set :channel "channels.list"))

;; start slack client
;;;###autoload
(defun slack (&optional team token)
  "Start Slack client."
  ;; load slack authentication file if it exists and `slack-auth' has not been customized
  (interactive)
  (let* ((auth-list (slack-auth-list team token))
         (auth (or (and slack-team slack-token (cons slack-team slack-token))
                   (and auth-list (car auth-list) (caar auth-list) (cdar auth-list) (car auth-list))
                   (cons (read-string "Team: ") (read-string "Token: "))))
         (team (car auth))
         (token (cdr auth)))
    (when (or (not (equal team slack-team))
              (not (equal token slack-token)))
      (setq slack-team team
            slack-token token))
    ;; setup buffer
    ;; (when (get-buffer slack-buffer-name)
    ;;   (kill-buffer slack-buffer-name))
    (let ((buffer (get-buffer-create slack-buffer-name)))
      (set-buffer buffer)
      ;; (kill-all-local-variables)
      (unless (local-variable-if-set-p 'slack-state)
        (make-local-variable 'slack-state))
      (unless (local-variable-if-set-p 'slack-data)
        (make-local-variable 'slack-data))
      (unless (local-variable-if-set-p 'slack-data-new)
        (make-local-variable 'slack-data-new))
      (slack-switch-team))))

(provide 'slack)

;;; slack.el ends here
