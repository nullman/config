;;; hnb.el --- Hierarchal Notebook
;;
;;; Copyright (C) 2006,2007 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2007-05-07
;; Version:  0.1
;; Keywords: hierarchal notebook note org hnb
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
;; I've use hnb (http://hnb.sourceforge.net/) on my various underpowered text
;; only laptops for a long time now. It's a great program for storing all of
;; my outliner type information. However, it's not Emacs, and I've longed for
;; the day when I could have my hnb and Emacs too. So I wrote this elisp mode.
;;
;; If you currently use hnb, you can use your existing hnb files directly. The
;; same file format is supported. Please backup your existing files first! I
;; will not be responsible for any data loss (see above legalese).
;;
;;; Installation:
;;
;; Put `hnb.el' where you keep your elisp files and add the following to your
;; .emacs file:
;;
;; (require 'hnb-mode)
;; (when (load "hnb-mode" t)
;;   (autoload 'hnb-mode "hnb-mode" "Hierarchal Notebook editing mode." t)
;;   (add-to-list 'auto-mode-alist '("\\.hnb$" . hnb-mode)))
;;
;; This will cause the mode to be entered when you load an hnb file.
;;
;; You must have `nxml-mode' installed.

;;; Code:

(require 'nxml-mode)

;; regular expressions

;; hnb node
(defconst hnb-node
  "<node>"
  "Matches a node.")

;; hnb data
(defconst hnb-data
  "<data>"
  "Matches a data element.")

;; hnb mode
(defun hnb-mode ()
  "Major mode for editing hnb files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'hnb-mode)
  (setq mode-name "Hierarchal Notebook")
  ;;(use-local-map hnb-mode-map)
  (setq buffer-read-only t)
  ;;(hnb-load)
  (run-hooks 'hnb-mode-hook))

;; load file into buffer
(defun hnb-load-file-to-buffer (file-name buffer-name)
  "Load FILE-NAME from disk into a new buffer named BUFFER-NAME.

Return the created buffer."
  (let (buffer)
   (when (file-readable-p file-name)
      ;; create buffer named BUFFER-NAME
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        ;; insert FILE-NAME contents into buffer
        (insert-file-contents file-name)
        ;; make buffer read-only
        (setq buffer-read-only t)))
    ;; return buffer
    buffer))

;; load file
(defun hnb-load-file (file-name &optional buffer)
  "Load FILE-NAME as an hnb file.

Creates an edit buffer the user interacts with and a read-only
storage buffer for the XML saved version.")

;; get current token
(defun hnb-get-nxml-token ()
  "Return current nXML token."
  (nxml-mark-token-after)
  (copy-region-as-kill (mark) (point)))

;; hnb parse buffer
(defun hnb-parse (buffer loc)
  "Parse an hnb scratch BUFFER starting at LOC into current buffer."
  (let ((edit-buffer (current-buffer))
        data)
    (save-current-buffer
      (set-buffer buffer)
      (nxml-mode)
      ;; process buffer
      (goto-char (point-min))
      (while (not (eobp))
        ;; enter <tree> node
        (nxml-forward-balanced-item)
        ;; for each <node> print the data
        (nxml-forward-element)

        (forward-line 1)))))

(provide 'hnb)

;;; Tests:

;; temp test file and buffer names
(defconst hnb-test-file "~/hnb_test/quotes.hnb")
(defconst hnb-test-edit-buffer "hnb-quotes")
(defconst hnb-test-scratch-buffer (concat "*" hnb-test-edit-buffer "*"))

;; run test
(defun hnb-test ()
  "Test hnb."
  (interactive)
  (let (scratch-buffer
        edit-buffer)
    ;; load hnb file into scratch buffer
    (setq scratch-buffer (hnb-load-file hnb-test-file hnb-test-scratch-buffer))
    ;; create buffer for editing
    (setq edit-buffer (get-buffer-create hnb-test-edit-buffer))
    ;; switch to editing buffer
    (switch-to-buffer edit-buffer)
    ;; parse scratch buffer into editing buffer
    (hnb-parse scratch-buffer 1)))

;;; hnb.el ends here
