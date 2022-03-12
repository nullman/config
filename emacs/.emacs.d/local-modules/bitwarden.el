;;; bitwarden.el --- BitWarden Client
;;
;;; Copyright (C) 2021 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2021-01-23
;; Version:  1.0
;; Keywords: bitwarden mode password
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
;; Provides `bitwarden-mode'.
;;
;;; Installation:
;;
;; Put `bitwarden.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (require 'bitwarden)
;;
;;; Usage:
;;
;; TODO

;;; Code:

(defgroup bitwarden nil
  "BitWarden customization"
  :group 'applications)

(defconst bitwarden-default-buffer-name "*BitWarden*"
  "Default buffer name to use when `bitwarden-file' is nil.")

(defvar bitwarden-buffer-password nil
  "Master BitWarden password.  Stored as a buffer-local variable.")
(make-variable-buffer-local 'bitwarden-buffer-password)
(put 'bitwarden-buffer-password 'permanent-local t)

(defun bitwarden-read-passwd (&optional confirm default)
  (read-passwd "BitWarden master password: " confirm default))

(defun bitwarden-get-buffer-password (&optional buffer)
  "Return BitWarden master password for BUFFER, or nil if not set.
If BUFFER is nil, current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    bitwarden-buffer-password))

(defun bitwarden-set-buffer-password (&optional password buffer)
  "Set BitWarden master password for BUFFER to PASSWORD.
If PASSWORD is nil, prompt for password.
If BUFFER is nil, current buffer is assumed."
  (interactive)
  (let ((password (or password (bitwarden-read-passwd))))
    (with-current-buffer (or buffer (current-buffer))
      (setq bitwarden-buffer-password password))))

;;(defun bitwarden-unlock


(provide 'bitwarden)

;;; bitwarden.el ends here
