;; twitter.el --- Twitter Client
;;
;;; Copyright (C) 2008 Kyle W T Sherman, with portions Copyright 2007 Theron Tlax
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-08-29
;; Version:  1.0
;; Keywords: twitter
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
;; Provides basic twitter functionality, including viewing recent posts (with
;; auto-updating), submitting posts, etc.
;;
;; Note that a significant portion of this code was copied from Theron Tlax's
;; excellent twit.el package found here:
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/twit.el
;;
;;; Installation:
;;
;; Put `twitter.el' where you keep your elisp files and add something like the
;; following to your .emacs file:
;;
;;   (require 'twitter)
;;
;;; Usage:
;;
;;

;;; Code:

(require 'xml)
(require 'url)

;; customize group
(defgroup twitter nil
  "Twitter client."
  :prefix "twitter-"
  :group 'hypermedia)

;; username
(defcustom twitter-username
  nil
  "Twitter username. Will prompt if not set."
  :type 'string
  :group 'twitter)

;; password
(defcustom twitter-password
  nil
  "Twitter password. Will prompt if not set"
  :type 'string
  :group 'twitter)

;; public timeline buffer name
(defcustom twitter-public-timeline-buffer-name
  "*Twitter-Public-Timeline*"
  "Buffer name to use for twitter public timeline."
  :type 'string
  :group 'twitter)

;; friends timeline buffer name
(defcustom twitter-friends-timeline-buffer-name
  "*Twitter-Friends-Timeline*"
  "Buffer name to use for twitter friends timeline."
  :type 'string
  :group 'twitter)

;; friends buffer name
(defcustom twitter-friends-buffer-name
  "*Twitter-Friends*"
  "Buffer name to use for twitter friends."
  :type 'string
  :group 'twitter)

;; followers buffer name
(defcustom twitter-followers-buffer-name
  "*Twitter-Followers*"
  "Buffer name to use for twitter followers."
  :type 'string
  :group 'twitter)

;; fonts
(copy-face 'bold 'twitter-message-face)
(set-face-attribute 'twitter-message-face nil
                    :family "helv"
                    :height 1.2
                    :weight 'semi-bold
                    :width 'semi-condensed)
(copy-face 'bold 'twitter-author-face)
(set-face-attribute 'twitter-author-face nil
                    :family 'unspecified
                    :weight 'semi-bold
                    :width 'semi-condensed)

;; base url
(defvar twitter-base-url "http://twitter.com/")

;; api urls
(defvar twitter-api-urls
  (mapcar (lambda (x) (list (car x) (concat twitter-base-url (cdr x))))
          '((:public-timeline . "statuses/public_timeline.xml")
            (:friends-timeline . "statuses/friends_timeline.xml")
            (:friends . "statuses/friends.xml")
            (:followers . "statuses/followers.xml")
            (:update . "statuses/update.xml")))
  "URLs for the twitter API.")

;; submit query
(defun twitter-request (type &optional username password message)
  "Submit an API request to twitter.
\nTYPE is an API type found in `twitter-api-urls'.
USERNAME is the twitter username, if nil `twitter-username' is used.
PASSWORD is the twitter password, if nil `twitter-password' is used.
MESSAGE is the string to post if doing an `:update'."
  (interactive
   (list (or username twitter-username (read-from-minibuffer "Twitter username: " ""))
         (or password twitter-password (read-from-minibuffer "Twitter password: " ""))
         (and (eq type :update) (or message (read-from-minibuffer "Message: " "")))))
  (let ((url (cadr (assq type twitter-api-urls)))
        (url-request-method "GET")
        (url-request-data "")
        result)
    (unless url
      (error "Invalid type given: %s" type))
    (save-current-buffer
      (if (eq type :update)
          ;; handle update request
          (progn
            (when (> (length message) 140)
              (error "Twitter message cannot be longer than 140 characters"))
            (setq url-request-data (concat "status=" (url-hexify-string message)))
            (message "%s" url-request-data)
            (url-retrieve url (lambda (x) (kill-buffer (current-buffer))))
            ;; TODO check response for errors
            (setq result t))
        ;; all other requests return parsed xml
        (progn
          (set-buffer (url-retrieve-synchronously url))
          (goto-char (point-min))
          (setq result (xml-parse-region))
          (kill-buffer (current-buffer)))))
    result))

;; parse result
;; taken from Theron Tlax's excellent twit.el
(defun twitter-parse-result (result)
  "Parse result XML returned from `twitter-request' into current buffer."
  (cl-labels ((insert-with-overlay-attributes (text attributes)
                                              (let ((start (point)))
                                                (insert text)
                                                (let ((overlay (make-overlay start (point))))
                                                  (dolist (spec attributes)
                                                    (overlay-put overlay (car spec) (cdr spec))))))
              (xml-first-child (node attr)
                               (car (xml-get-children node attr)))
              (xml-first-childs-value (node addr)
                                      (car (xml-node-children (xml-first-child node addr)))))
    (save-excursion
      (erase-buffer)
      (insert (format-time-string "Last updated: %c\n\n"))
      (dolist (status-node (xml-get-children (cadr result) 'status))
        (let* ((user-info (xml-first-child status-node 'user))
               (user-id (or (xml-first-childs-value user-info 'screen_name) "??"))
               (user-name (xml-first-childs-value user-info 'name))
               (location (xml-first-childs-value user-info 'location))
               (src-info (xml-first-childs-value status-node 'source))
               (timestamp (xml-first-childs-value status-node 'created_at))
               (message (xml-first-childs-value status-node 'text))
               ;;margin
               )
          (when (and src-info (string-match (concat "<a h" "ref=.*>\\(.*\\)<" "/a>") src-info))
            ;; remove the HTML link info
            (setq src-info (match-string 1 src-info)))
          ;; insert name
          ;; (insert-with-overlay-attributes (format "%25s" (concat user-id
          ;;                                                        (if user-name
          ;;                                                            (concat " (" user-name ")")
          ;;                                                          "")))
          ;;                                 '((face . "twitter-author-face")))
          (insert-with-overlay-attributes (format "%s" (concat user-id
                                                               (if user-name
                                                                   (concat " (" user-name ")")
                                                                 "")))
                                          '((face . "twitter-author-face")))
          ;;(insert ": ")
          (newline)
          ;;(setq margin (- (point) (point-at-bol)))
          (let ((pos (point)))
            (insert "  ")
            (insert-with-overlay-attributes message
                                            '((face . "twitter-message-face")))
            (fill-region pos (point)))
          (newline)
          (when (or timestamp location src-info)
            ;;(insert (spaces-string (1- margin)))
            (insert " ")
            (when timestamp
              (insert (concat " posted " timestamp)))
            (when location
              (insert (concat " from " location)))
            (when src-info
              (insert (concat " (via " src-info ")")))
            (newline))
          (newline))))))

;; submit twitter update message
(defun twitter-update (&optional message username password)
  "Submit an update MESSAGE to twitter.
\nUSERNAME is the twitter username, if nil `twitter-username' is used.
PASSWORD is the twitter password, if nil `twitter-password' is used."
  (interactive
   (list (or message (read-from-minibuffer "Message: " ""))
         (or username twitter-username (read-from-minibuffer "Twitter username: " ""))
         (or password twitter-password (read-from-minibuffer "Twitter password: " ""))))
  (twitter-request :update username password message))

;; show twitter
(defun twitter-show (type buffer-name &optional username password)
  "Show twitter request in a buffer.
\nUSERNAME is the twitter username, if nil `twitter-username' is used.
PASSWORD is the twitter password, if nil `twitter-password' is used."
  ;; setup buffer
  (get-buffer-create buffer-name)
  (set-buffer buffer-name)
  (setq buffer-read-only nil)

  ;; call twitter request function
  (twitter-parse-result (twitter-request type username password))

  ;; set buffer to read-only
  (setq buffer-read-only t)

  ;; more setup
  (switch-to-buffer buffer-name)
  (goto-char (point-min)))

;; show public timeline
(defun twitter-show-public-timeline ()
  "Display the twitter public timeline in a buffer."
  (interactive)
  (twitter-show :public-timeline twitter-public-timeline-buffer-name))

          ;; '((:public-timeline . "statuses/public_timeline.xml")
          ;;   (:friends-timeline . "statuses/friends_timeline.xml")
          ;;   (:friends . "statuses/friends.xml")
          ;;   (:followers . "statuses/followers.xml")
          ;;   (:update . "statuses/update.xml")))

(provide 'twitter)

;;; twitter.el ends here
