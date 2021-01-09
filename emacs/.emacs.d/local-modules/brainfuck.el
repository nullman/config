;;; brainfuck.el --- Brainfuck Compiler/Interpreter
;;
;;; Copyright (C) 2016 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2016-03-18
;; Version:  1.0
;; Keywords: brainfuck parser compiler interpreter mode
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
;; Provides `brainfuck-mode' function that implements syntax highlighting.
;;
;; If `*brainfuck-debug*' is putting ellipsis' in the output, set the
;; following variables:
;;
;;   (setq print-length nil)
;;   (setq print-level nil)
;;
;;; Installation:
;;
;; Put `brainfuck.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (autoload 'brainfuck-mode "brainfuck" "Major mode for editing Brainfuck code." t)
;;   (add-to-list 'auto-mode-alist '("\\.bf$" . brainfuck-mode))
;;
;;; Usage:
;;
;; Load a Brainfuck (.bf) file and press \C-cr or eval:
;;
;;   (brainfuck-run)
;;
;; Example (Hello World! from https://en.wikipedia.org/wiki/Brainfuck):
;;
;;   +++++ +++               Set Cell #0 to 8
;;   [
;;       >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
;;       [                   as the cell will be cleared by the loop
;;           >++             Add 2 to Cell #2
;;           >+++            Add 3 to Cell #3
;;           >+++            Add 3 to Cell #4
;;           >+              Add 1 to Cell #5
;;           <<<<-           Decrement the loop counter in Cell #1
;;       ]                   Loop till Cell #1 is zero; number of iterations is 4
;;       >+                  Add 1 to Cell #2
;;       >+                  Add 1 to Cell #3
;;       >-                  Subtract 1 from Cell #4
;;       >>+                 Add 1 to Cell #6
;;       [<]                 Move back to the first zero cell you find; this will
;;                           be Cell #1 which was cleared by the previous loop
;;       <-                  Decrement the loop Counter in Cell #0
;;   ]                       Loop till Cell #0 is zero; number of iterations is 8
;;   >>.                     Cell #2 has value 72 which is 'H'
;;   >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
;;   +++++++..+++.           Likewise for 'llo' from Cell #3
;;   >>.                     Cell #5 is 32 for the space
;;   <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
;;   <.                      Cell #3 was set to 'o' from the end of 'Hello'
;;   +++.------.--------.    Cell #3 for 'rl' and 'd'
;;   >>+.                    Add 1 to Cell #5 gives us an exclamation point
;;   >++.                    And finally a newline from Cell #6
;;
;; Or just:
;;
;;   ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.

;;; Code:

;; customize group
(defgroup brainfuck nil
  "Brainfuck compiler/interpreter."
  :prefix "brainfuck-"
  :group 'programming)

;; initial data size
(defconst brainfuck-data-size
  10000
  "Initial program data side in bytes.")

;;; Keywords and Font Locking

;; commands regexp
(defconst brainfuck-keywords-commands-regexp
  "[][><+-.,]"
  "Brainfuck commands.")

;; comments regexp (i.e. everything else)
(defconst brainfuck-keywords-comments-regexp
  "[^][><+-.,]"
  "Brainfuck comments.")

;; commands
(defconst brainfuck-keywords-commands
  '(?> ?< ?+ ?- ?. ?, ?\[ ?\])
  "Brainfuck commands.")

;; font lock settings
(defconst brainfuck-mode-font-lock-keywords
  `(("[><]" 0 font-lock-builtin-face)
    ("[+-]" 0 nil)
    ("[.,]" 0 font-lock-keyword-face)
    ("[][]" 0 font-lock-variable-name-face)
    (,brainfuck-keywords-comments-regexp 0 font-lock-comment-face))
  "Brainfuck keywords used by font-lock.")

;; brainfuck output buffer name
(defconst brainfuck-output-buffer-name
  "*brainfuck-output*"
  "Buffer name to use for Brainfuck output.")

;;; Brainfuck Mode

;; mode map
(defvar brainfuck-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") 'brainfuck-send-paragraph)
    ;; (define-key map (kbd "C-c C-r") 'brainfuck-send-region)
    ;; (define-key map (kbd "C-c C-s") 'brainfuck-send-string)
    ;; (define-key map (kbd "C-c C-b") 'brainfuck-send-buffer)
    ;;(define-key map (kbd "C-c C-c") 'brainfuck-compile)
    (define-key map (kbd "C-c C-i") 'brainfuck-interpret-run)
    (define-key map (kbd "C-c C-c") 'brainfuck-run)
    (define-key map (kbd "C-c C-r") 'brainfuck-run)
    map)
  "Mode map used for `brainfuck-mode'.")

;;;###autoload
;;(define-derived-mode brainfuck-mode fundamental-mode "Brainfuck"
(defun brainfuck-mode ()
  "Major mode to edit, compile, and execute Brainfuck programs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'brainfuck-mode)
  (setq mode-name "Brainfuck")
  ;; set local key map
  (use-local-map brainfuck-mode-map)
  ;; set font lock (case insensitive)
  (setq font-lock-defaults '((brainfuck-mode-font-lock-keywords) nil t))
  ;; set buffer to case insensitive
  (setq case-fold-search t)
  ;; run hooks
  (run-hooks 'brainfuck-mode-hook))

;;; Parser

;; list of relevant ascii character codes and their enumerated types
(defvar brainfuck-parse-grammar-char-type
  `(,(cons (string-to-char ">") :greater-than)
    ,(cons (string-to-char "<") :less-than)
    ,(cons (string-to-char "+") :plus)
    ,(cons (string-to-char "-") :minus)
    ,(cons (string-to-char ".") :period)
    ,(cons (string-to-char ",") :comma)
    ,(cons (string-to-char "[") :left-bracket)
    ,(cons (string-to-char "]") :right-bracket))
  "List of characters and their types.")

;; hash table of codes to types
(defvar brainfuck-parse-grammar-char-type-hash
  (make-hash-table)
  "Map of characters to their types.")
(dolist (item brainfuck-parse-grammar-char-type)
  (setf (gethash (car item) brainfuck-parse-grammar-char-type-hash)
        (cdr item)))

;; map of enumerated types to their ascii character codes
(defvar brainfuck-parse-grammar-type-char-hash
  (make-hash-table)
  "Map of types to their characters.")
(dolist (item brainfuck-parse-grammar-char-type)
  (let ((entry (gethash (cdr item) brainfuck-parse-grammar-type-char-hash)))
    (if entry
        (if (listp entry)
            (setf (gethash (cdr item) brainfuck-parse-grammar-type-char-hash)
                  (append entry (list (car item))))
          (setf (gethash (cdr item) brainfuck-parse-grammar-type-char-hash)
                (list entry (car item))))
      (setf (gethash (cdr item) brainfuck-parse-grammar-type-char-hash)
            (car item)))))

;; parse statement
(defun brainfuck-parse-statement (statement)
  "Parse Brainfuck STATEMENT into a list of tokens."
  (let (tokens                        ; generated token list
        current                       ; current string
        (jump 0))                     ; nested jump count
    ;; loop over all characters in statement
    (cl-loop for char across statement
             do (let ((type (gethash char brainfuck-parse-grammar-char-type-hash)))
                  (when type
                    (cond ((eq type :left-bracket)
                           (setq jump (1+ jump)))
                          ((eq type :right-bracket)
                           (setq jump (1- jump))))
                    (push type tokens)
                    (when (< jump 0)
                      (error "Parsed ']' command without a matching '['")))))
    (when (> jump 0)
      (error "Parsed '[' command without a matching ']'"))
    ;; return tokens
    (nreverse tokens)))

;; parse buffer or region
(defun brainfuck-parse (&optional start end)
  "Parse current buffer or region and return a list of tokens."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    ;; parse buffer and convert syntax to tokens
    (brainfuck-parse-statement (buffer-substring-no-properties start end))))

;;; Interpreter

;; interpret tokens and run brainfuck program
(defun brainfuck-interpret (tokens &optional input)
  "Interpret TOKENS with optional INPUT and run Brainfuck program."
  (let ((data (make-vector brainfuck-data-size 0)) ; initial data block
        (data-pointer 0)                           ; data pointer
        jump-stack                                 ; jump stack
        output                                     ; output
        (input (if (stringp input)                 ; input as a list of bytes
                   (nconc (string-to-list input) (list -1))
                 input)))
    ;; iterate over tokens
    (while tokens
      ;;(message "%S %S %S" (aref data 0) (aref data 1) (aref data 2))
      (let ((token (pop tokens)))
        ;; interpret token
        (cond ((eq token :greater-than)   ; '>'
               (cl-incf data-pointer)
               (when (= data-pointer (length data))
                 (setq data (vconcat data (make-vector brainfuck-data-size 0)))))
              ((eq token :less-than)      ; '<'
               (decf data-pointer)
               (when (minusp data-pointer)
                 (error "Data pointer cannot go below zero")))
              ((eq token :plus)           ; '+'
               ;;(aset data data-pointer (1+ (aref data data-pointer))))
               (cl-incf (aref data data-pointer)))
              ((eq token :minus)          ; '-'
               ;;(aset data data-pointer (1- (aref data data-pointer))))
               (decf (aref data data-pointer)))
              ((eq token :period)         ; '.'
               (push (aref data data-pointer) output))
              ((eq token :comma)          ; ','
               (aset data data-pointer
                     (progn
                       (unless input    ; if input is empty, prompt for more
                         (setq input (string-to-list (read-from-minibuffer "? "))))
                       (if input
                           (pop input)  ; return next input character
                         -1))))         ; default to -1 if no input given
              ((eq token :left-bracket) ; '['
               (if (zerop (aref data data-pointer))
                   ;; if data at point is zero then skip to end of jump
                   (let ((js 1))
                     (while (plusp js)
                       (setq token (pop tokens))
                       (cond ((eq token :left-bracket)
                              (cl-incf js))
                             ((eq token :right-bracket)
                              (decf js)))))
                 ;; otherwise, loop through code block
                 (push (cons token tokens) jump-stack)))
              ((eq token :right-bracket)  ; ']'
               ;; loop
               (setq tokens (pop jump-stack))))))
    ;; return output
    (concat (nreverse output))))

;; run buffer with interpreter
;;;###autoload
(defun brainfuck-interpret-run (&optional input buffer)
  "Interpret a Brainfuck program in BUFFER with optional INPUT.
\nBUFFER defaults to `current-buffer'."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        output-buffer)
    (save-current-buffer
      (set-buffer buffer)
      ;; parse buffer
      (condition-case err
          (let ((output-buffer (get-buffer-create brainfuck-output-buffer-name)) ; output buffer
                (tokens (brainfuck-parse))) ; parse tokens
            ;; setup output buffer
            (delete-other-windows)
            (split-window-vertically)
            (other-window 1)
            (set-buffer output-buffer)
            (switch-to-buffer output-buffer)
            (setq buffer-read-only nil)
            (erase-buffer)
            ;; compile and run program
            (condition-case err
                (insert (brainfuck-interpret tokens input))
              ('error
               (insert (format "Compiler Error: %s" err)))))
        ('error
         (insert (format "Parser Error: %s" err)))
        ;; make output buffer read-only
        (setq buffer-read-only t)))))

;;; Compiler

;; compile tokens into elisp code
(defun brainfuck-compile (tokens)
  "Compile TOKENS and return a block of Emacs Lisp code."
  (cl-labels ((while-loop ()
                          (let (done code)
                            (while (and (not done) tokens) ; iterate over tokens generating code
                              (let ((token (pop tokens)))
                                (cond ((eq token :greater-than)
                                       (push `(funcall data-pointer-incr) code))
                                      ((eq token :less-than)
                                       (push `(funcall data-pointer-decr) code))
                                      ((eq token :plus)
                                       (push `(funcall data-incr) code))
                                      ((eq token :minus)
                                       (push `(funcall data-decr) code))
                                      ((eq token :period)
                                       (push `(funcall data-output) code))
                                      ((eq token :comma)
                                       (push `(funcall data-input) code))
                                      ((eq token :left-bracket) ; start new loop
                                       (push `(while (not (zerop (aref data data-pointer))) ,@(while-loop)) code))
                                      ((eq token :right-bracket) ; return from loop
                                       (setq done t)))))
                            (nreverse code))))
    ;; return generated code
    `(lambda (&optional input)
     ;; global data
     (let ((data (make-vector brainfuck-data-size 0)) ; initial data block
           (data-pointer 0)                           ; data pointer
           jump-stack                                 ; jump stack
           output                                     ; output stream
           (input (if (stringp input)   ; input as a list of bytes
                      (nconc (string-to-list input) (list -1))
                    input)))
       ;; functions
       (let ((data-pointer-incr         ; '>'
              (lambda ()
                (cl-incf data-pointer)
                (when (= data-pointer (length data))
                  (setq data (vconcat data (make-vector brainfuck-data-size 0))))))
             (data-pointer-decr         ; '<'
              (lambda ()
                (decf data-pointer)
                (when (minusp data-pointer)
                  (error "Data pointer cannot go below zero"))))
             (data-incr                 ; '+'
              (lambda ()
                (cl-incf (aref data data-pointer))))
             (data-decr                 ; '-'
              (lambda ()
                (decf (aref data data-pointer))))
             (data-output               ; '.'
              (lambda ()
                (push (aref data data-pointer) output)))
             (data-input                ; ','
              (lambda ()
                (aset data data-pointer
                      (progn
                        (unless input   ; if input is empty, prompt for more
                          (setq input (string-to-list (read-from-minibuffer "? "))))
                        (if input
                            (pop input) ; return next input character
                          -1))))))      ; default to -1 if no input given
         ,@(while-loop)
         ;; return output
         (concat (nreverse output)))))))

;; compile and run buffer
;;;###autoload
(defun brainfuck-run (&optional input buffer)
  "Compile and run a Brainfuck program in BUFFER with optional INPUT.
\nBUFFER defaults to `current-buffer'."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        output-buffer)
    (save-current-buffer
      (set-buffer buffer)
      ;; parse buffer
      (condition-case err
          (let ((output-buffer (get-buffer-create brainfuck-output-buffer-name)) ; output buffer
                (tokens (brainfuck-parse))) ; parse tokens
            ;; setup output buffer
            (delete-other-windows)
            (split-window-vertically)
            (other-window 1)
            (set-buffer output-buffer)
            (switch-to-buffer output-buffer)
            (setq buffer-read-only nil)
            (erase-buffer)
            ;; compile and run program
            (condition-case err
                (let ((code (brainfuck-compile tokens)))
                  (message "Brainfuck compiled code: %S" code)
                  (insert (funcall code input)))
              ('error
               (insert (format "Compiler Error: %s" err)))))
        ('error
         (insert (format "Parser Error: %s" err)))
        ;; make output buffer read-only
        (setq buffer-read-only t)))))

(provide 'brainfuck)

;;; brainfuck.el ends here
