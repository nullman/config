;;; web-query.el --- Search Web Sites
;;
;;; Copyright (C) 2008 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-10-03
;; Version:  1.0
;; Keywords: web query
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
;; Provides `web-query' and `web-query-google-define' functions to query
;; specific sites using the appropriate method. Most of the code for the
;; `web-query-google-define' function came from Rodrigo Lazo.
;;
;;; Installation:
;;
;; Put `web-query.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;; (require 'web-query)
;;
;; Optionally you may want to add some key bindings:
;;
;; (when (fboundp 'web-query-google-define)
;;   (global-set-key (kbd "<f5>") 'web-query-google-define))
;; (when (fboundp 'web-query)
;;   (global-set-key (kbd "<f6>") 'web-query))
;;
;; Can be customized with the following command:
;;
;;   (customize-group "web-query")
;;
;;; Usage:
;;
;; Simply call `web-query' with or without parameters. If neither NAME or
;; QUERY parameters are provided you will be prompted for them (with history).
;; The history for NAME is seeded from `web-query-list'.
;;
;; Call `web-query-google-define' to do a Google define query with the result
;; displayed in the minibuffer.

;;; Code:

;; mm-url
;;(require 'mm-url)

;; browse url
(require 'browse-url)

;; check for url browse function
(unless (fboundp browse-url-browser-function)
  (error "URL browse function `browse-url-browser-function' is not defined: %s" browse-url-browser-function))

;; customize group
(defgroup web-query nil
  "Search web sites."
  :prefix "web-query-"
  :group 'hypermedia)

;; ;; web query prefix
;; (defcustom web-query-prefix
;;   "http://www.google.com/search?q="
;;   "URL prefix to use for querying.
;; \nExample: \"http://www.google.com/search?q=\" for google."
;;   :type 'string
;;   :group 'web-query)

;; web query list
(defcustom web-query-list
  '(("all" . (:google "http://"))
    ("define" . (:custom "http://www.google.com/search?hl=en&q=define%3A"))
    ("word" . (:custom "http://www.answers.com/main/ntquery?s="))
    ;;("wiktionary" . (:custom "http://en.wiktionary.org/wiki/"))
    ;;("dictionary" . (:custom "http://dictionary.reference.com/browse/"))
    ;;("wikipedia" . (:google "http://www.wikipedia.org/"))
    ;;("wikipedia" . (:custom "http://en.wikipedia.org/wiki/"))
    ("wikipedia" . (:custom "http://en.wikipedia.org/wiki/Special:Search?search="))
    ;;("emacs-lisp" . (:google "http://www.gnu.org/software/emacs/manual/"))
    ("emacs-lisp" . (:google "http://www.gnu.org/emacs/"))
    ("emacswiki" . (:google "http://www.emacswiki.org/"))
    ("lisp" . (:custom "http://lispdoc.com/?search=Basic+search&q="))
    ("html" . (:google "http://www.htmlquick.com/reference/"))
    ("css" . (:google "http://www.w3schools.com/css/"))
    ("javascript" . (:google "http://www.w3schools.com"))
    ("java" . (:google "http://java.sun.com/javase/6/docs/api/"))
    ("android" . (:custom "http://developer.android.com/search.html#q=" "t=0"))
    ("clojure" . (:google "http://clojure.org/api/"))
    ("ruby" . (:google "http://ruby-doc.org/"))
    ("perl" . (:google "http://perldoc.perl.org/search.html?q="))
    ("python" . (:google "http://docs.python.org/ref/"))
    ;;("freshmeat" . (:google "http://freshmeat.net/"))
    ("freshmeat" . (:custom "http://freshmeat.net/search/?section=projects&q="))
    ("nullman" . (:google "http://nullman.net/")))
  "Association list of names to query type and URL to use with `web-query'.

The following format is used:

  ((NAME . (TYPE URL [SUFFIX]))
   (...))

  NAME is a descriptive name.

  TYPE is one of:
    :google
    :custom

  URL is dependent on TYPE as follows:
    If :google then URL is used for 'site:'.
    If :custom then URL is used as prefix to query string.

  SUFFIX is an optional string to be added to the end of the
  query."
  :type 'list
  :group 'web-query)

;; web query lucky
(defcustom web-query-lucky
  t
  "If non-nil, perform Google lucky query instead of normal query."
  :type 'boolean
  :group 'web-query)

;; name history
(defvar web-query-name-history
  (mapcar 'car web-query-list)
  "History of `web-query' name arguments.
\nInitialized with `web-query-list'.")

;; query history
(defvar web-query-query-history
  nil
  ;;(mapcar (lambda (x) (list (car x) nil)) web-query-list)
  "History of `web-query' query arguments.
\nOne association list per query name.")

;; web query custom
(defun web-query-custom (url query &optional suffix)
  "Browse result of a custom QUERY with optional SUFFIX appended to URL.
\nUsed internally by `web-query'."
  (funcall browse-url-browser-function
           (concat url (url-hexify-string query)
                   (if suffix
                       suffix
                     ""))))

;; google url
(defun web-query-google-url (url)
  "Return string created from splitting URL into \"site:\" and \"inurl:\" Google parts."
  (string-match "\\(http://.*?\\)\\(/.*\\)" url)
  (concat
   "site:" (or (match-string 1 url) "")
   " inurl:" (or (match-string 2 url) "")))

;; web query google
(defun web-query-google (url query lucky)
  "Browse result of a Google QUERY limited to URL site.
\nUsed internally by `web-query'.
If LUCKY is non-nil, Google's \"I'm Feeling Lucky\" query is used."
  (funcall browse-url-browser-function
           (concat "http://www.google.com/search?q="
                   (url-hexify-string (concat query " " (web-query-google-url url)))
                   (if lucky
                       "&btnI=Search"
                     "&btnG=Search"))))

;; web query
;;;###autoload
(defun web-query (&optional name query)
  "Lookup NAME in `web-query-list', then submit associated URL and QUERY to `web-query-google'."
  (interactive)
  (let* ((name (or name
                   (read-from-minibuffer "Name: "
                                         (car web-query-name-history)
                                         nil nil
                                         'web-query-name-history)))
         (hist (progn
                 (unless (assoc name web-query-query-history)
                   (push (list name nil) web-query-query-history))
                 (cadr (assoc name web-query-query-history))))
         (query (or query
                    (read-from-minibuffer "Query: "
                                          nil nil nil
                                          'hist))))
    ;; update history
    (when (member query hist)
      (setq hist (remove query hist)))
    (push query hist)
    (setcdr (assoc name web-query-query-history) (list hist))
    ;; perform query
    (multiple-value-bind (type url suffix) (cdr (assoc name web-query-list))
      (case type
        (:custom (web-query-custom url query suffix))
        (:google (web-query-google url query web-query-lucky))))))

;; web query google define
;; Original Author: Rodrigo Lazo
(when (require-if-available 'mm-url)
  (when (load "mm-url" t)
    (defun web-query-google-define (query)
      "Submit QUERY to Google Define and print the result in the minibuffer."
      (interactive "sQuery: ")
      (let* ((url (concat
                   "http://www.google.com/search?hl=en&q=define%3A"
                   (replace-regexp-in-string " " "%20" query)))
             (definition
               (save-excursion
                 (with-temp-buffer
                   (mm-url-insert url)
                   (goto-char (point-min))
                   (if (search-forward "No definitions were found" nil t)
                       "No definitions were found"
                     (buffer-substring (search-forward "<li>") (- (search-forward "<") 1)))))))
        (message "%s: %s" query definition)))
    (defun web-query-google-define-at-point ()
      "Call `web-query-google-define' with word at point or active region."
      (interactive)
      (let ((query (replace-regexp-in-string
                    " " "%20"
                    (if (and transient-mark-mode mark-active)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (substring-no-properties (thing-at-point 'word))))))
        (web-query-google-define query)))))

;; web query word
;;;###autoload
(defun web-query-word (word)
  "Submit WORD to a dictionary site (as specified in `web-query-list')."
  (interactive "sWord: ")
  (web-query "word" word))

;; web query word at point
;;;###autoload
(defun web-query-word-at-point ()
  "Call `web-query-word' with word at point or active region.
\nIf no word is found, user is prompted for one."
  (interactive)
  (let ((word (replace-regexp-in-string
                " " "%20"
                (if (and transient-mark-mode mark-active)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (substring-no-properties (or (thing-at-point 'word) ""))))))
    (if (> (length word) 0)
        (web-query-word word)
      (web-query-word (read-from-minibuffer "Word: ")))))

;; web query symbol by mode
;; Original Author: David Avraamides
;;;###autoload
(defun web-query-symbol-by-mode (query)
  "Call `web-query' with QUERY using current `major-mode' to determine what type of query to perform.

Supported modes:

  `emacs-lisp-mode'
  `html-mode'
  `html-helper-mode'
  `css-mode'
  `javascript-mode'
  `java-mode'
  `jde-mode'
  `clojure-mode'
  `ruby-mode'
  `perl-mode'
  `python-mode'

Defaults to dictionary word lookup."
  (interactive "sQuery: ")
  (web-query
   (cond
    ((equal major-mode 'emacs-lisp-mode)
     "emacs-lisp")
    ((or (equal major-mode 'html-mode)
         (equal major-mode 'html-helper-mode))
     "html")
    ((equal major-mode 'css-mode)
     "css")
    ((equal major-mode 'javascript-mode)
     "javascript")
    ((or (equal major-mode 'java-mode)
         (equal major-mode 'jde-mode))
     "java")
    ((equal major-mode 'clojure-mode)
     "clojure")
    ((equal major-mode 'ruby-mode)
     "ruby")
    ((equal major-mode 'perl-mode)
     "perl")
    ((equal major-mode 'python-mode)
     "python")
    (t
     "word"))
   query))

;;;###autoload
(defun web-query-symbol-by-mode-at-point ()
  "Call `web-query-symbol-by-mode' with symbol at point or active region.
\nIf no symbol is found, user is prompted for one."
  (interactive)
  (let ((query (replace-regexp-in-string
                " " "%20"
                (if (and transient-mark-mode mark-active)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (substring-no-properties (or (thing-at-point 'symbol) ""))))))
    (web-query-symbol-by-mode
     (if (> (length query) 0)
         query
       (read-from-minibuffer "Query: ")))))

(provide 'web-query)

;;; web-query.el ends here
