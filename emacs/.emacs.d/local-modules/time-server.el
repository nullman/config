;;; time-server.el --- Emacs Time Server
;;
;; Taken from here: http://www.emacswiki.org/emacs/EmacsDaytimeServer
;;
;; Keywords: time date server example
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
;; time-server is a simple TCP server that returns the time.
;;
;; Set `time-server-port' to the port the service should run on.
;;
;; Use `time-server-start' to start the service. Test it with:
;;
;;   telnet localhost 8080
;;
;; Use `time-server-stop' to stop the service.

;;; Code:

(defconst time-server-process-name
  "time-server"
  "Name of time server process.")

(defconst time-server-buffer-name
  "*time-server*"
  "Name of time server buffer.")

(defvar time-server-port
  8080
  "Time server port.")

(defun time-server-start ()
  "Start time server."
  (interactive)
  (unless (process-status time-server-process-name)
    (when (featurep 'make-network-process '(:server t))
      (make-network-process
       :name time-server-process-name
       :buffer time-server-buffer-name
       :service time-server-port
       :family 'ipv4
       :server t
       :noquery t
       :sentinel nil
       :filter (lambda (proc string) nil)
       :log 'time-server-log))))

(defun time-server-stop ()
  "Stop time server."
  (interactive)
  (delete-process time-server-process-name))

(defun time-server-log (server client message)
  "Send current time to the requesting client.
\nThe time and client information is also inserted into the
`time-server-buffer-name' buffer."
  (process-send-string client (concat (current-time-string) "\n"))
  (with-current-buffer time-server-buffer-name
    (goto-char (point-max))
    (insert (current-time-string) (format " %s\n" client)))
  (delete-process client))

(provide 'time-server)

;;; time-server.el ends here
