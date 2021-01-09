;;; basic.el --- Applesoft BASIC Compiler/Interpreter
;;
;;; Copyright (C) 2008-2014 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-03-29
;; Version:  0.1
;; Keywords: applesoft basic parser compiler interpreter mode
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
;; Provides `basic-mode' function that implements syntax highlighting.
;;
;; If `*basic-debug*' is putting ellipsis' in the output, set the following
;; variables:
;;
;;   (setq print-length nil)
;;   (setq print-level nil)
;;
;;; Installation:
;;
;; Put `basic.el' where you keep your elisp files and add something like
;; the following to your .emacs file:
;;
;;   (autoload 'basic-mode "basic" "Major mode for editing BASIC code." t)
;;   (add-to-list 'auto-mode-alist '("\\.bas$" . basic-mode))
;;
;;; Usage:
;;
;; Load a BASIC (.bas) file and press \C-cr or eval:
;;
;;   (basic-run)
;;
;; Example:
;;
;; 10 REM test BASIC example program
;; 20 PRINT "This is a test program"
;; 30 PRINT "Summing the numbers from 1 to 100"
;; 40 LET total = 0
;; 50 FOR i = 1 TO 100
;; 60 LET total = total + i
;; 70 NEXT i
;; 80 PRINT "The total of all digits from 1 to 100 is " total
;; 90 END
;;
;; To generate debugging output, turn on the debug-log:
;;
;;   (setq basic-debug-log t)
;;
;;; ToDo:
;;
;; - Only add functions that are needed.  Loop through tokens and build a
;;   functions needed list.
;;
;; - Add correct run-time errors.
;;
;; - Build a parsed list with start and end positions in source buffer for
;;   each token.  (This can be used to highlight errors in source code as well
;;   as at run-time.)
;;
;; - Look at Applesoft BASIC reference/book and make sure this is compliant.

;;; Code:

;; customize group
(defgroup basic nil
  "Applesoft BASIC compiler/interpreter."
  :prefix "basic-"
  :group 'programming)

(defcustom basic-debug-log
  nil
  "If non-nil debugging log entries are written to `basic-debug-buffer-name'."
  :type 'boolean
  :group 'basic)

;;(setq basic-debug-log nil)
(setq basic-debug-log t)

(defcustom basic-test-mode
  nil
  "If non-nil test-mode is used."
  :type 'boolean
  :group 'basic)

;;; Regular Expression Matches

;; ;; blank line regular expression match
;; (defconst basic-match-blank-regexp
;;   ;;"^[\t ]*$"
;;   "^\\s-*$"
;;   "Regular expression that matches a blank line.")

;; line number regular expression match
(defconst basic-match-line-number-regexp
  "^\\s-*\\<\\([0-9]+\\)\\>"
  "Regular expression that matches a line number.")

;; comment regular expression match
(defconst basic-match-comment-regexp
  ;;"\\(^\\s-*[0-9]+\\brem\\b.*$\\|;\\brem\\b.*$\\|;rem\\b.*$\\)"
  "\\<rem\\>.*$"
  "Regular expression that matches a comment.")

;; ;; statement regular expression match
;; (defconst basic-match-statement-regexp
;;   "\\(:\\)"
;;   "Regular expression that matches a statement.")

;; ;; quote regular expression match
;; (defconst basic-match-quote-regexp
;;   "\\(\"[^\"]*\"\\)"
;;   "Regular expression that matches a quoted string.")

;; ;; quote before colon regular expression match
;; (defconst basic-match-quote-before-colon-regexp
;;   "\\([^:]*\"[^\"]*\"\\)"
;;   "Regular expression that matches a quoted string appearing before a colon.")

;; target line number regular expression match
(defconst basic-match-target-line-number-regexp
  ;;"\\<\\(goto\\|gosub\\|then\\)\\>\\<\\([0-9]+\\)\\>"
  "\\<\\(goto\\|gosub\\|then\\)\\>\\s-*\\(\\<\\([0-9]+\\)\\>\\(,\\<[0-9]+\\>\\)*\\)"
  "Regular expression that matches a line number.")

;;; Keywords and Font Locking

;; statements
(defconst basic-keywords-statements
  '("data" "def" "dim" "end" "goto" "gosub" "input" "let" "print" "read" "rem"
    "restore" "return" "stop")
  "BASIC statements.")

;; reserved words
(defconst basic-keywords-reserved
  '("and" "for" "if" "mod" "next" "on" "or" "step" "then" "to")
  "BASIC reserved words.")

;; functions
(defconst basic-keywords-functions
  '("abs" "asc" "atn" "chr$" "cos" "exp" "int" "left$" "len" "log" "mid$"
    "rnd" "right$" "sgn" "sin" "sqr" "str$" "tab" "tan" "val")
  "BASIC functions.")

;; font lock settings
;; font-lock-type-face is not used
(defconst basic-mode-font-lock-keywords
  `((,basic-match-line-number-regexp 0 font-lock-reference-face)
    (,basic-match-target-line-number-regexp 2 font-lock-reference-face)
    (,basic-match-comment-regexp 0 font-lock-comment-face)
    (,(regexp-opt basic-keywords-statements 'words) 0 font-lock-keyword-face)
    (,(regexp-opt basic-keywords-reserved 'words) 0 font-lock-builtin-face)
    (,(regexp-opt basic-keywords-functions 'words) 0 font-lock-function-name-face))
  "BASIC keywords used by font-lock.")

;; basic output buffer name
(defconst basic-output-buffer-name
  "*basic-output*"
  "Buffer name to use for basic output.")

;; basic debug buffer name
(defconst basic-debug-buffer-name
  "*basic-debug*"
  "Buffer name to use for basic debugging output.")

;;; Buffer Local Variables

;; ;; token hash
;; (defvar basic-token-hash
;;   nil
;;   "Buffer local variable to hold current compiled tokens.

;; Format:

;;   ((LINE-NUMBER1 . (TOKEN1, TOKEN2, ...))
;;    (LINE-NUMBER2 . (TOKEN1, TOKEN2, ...))
;;    ...)")

;; ;; run-time code
;; (defvar basic-run-time-code
;;   nil
;;   "Buffer local variable to hold generated run-time code.")

;; ;; line numbers
;; (defvar basic-line-numbers
;;   nil
;;   "Buffer local variable to hold ordered line number list of
;;   `basic-run-time-code'.")

;;; Error Codes

;; error codes
(defconst basic-error-codes
  '((:syntax-error . "syntax error")
    (:missing-line-number . "missing line number")
    ;;(:invalid-symbol . "invalid symbol")
    (:unclosed-quote . "unclosed quote")
    (:missing-jump-target "jump target has no corresponding line number")
    (:expected-value . "expected value not found")
    (:missing-left-hand-value . "missing left hand value of expression")
    (:missing-assignment . "missing assignment operator (equal sign)")
    (:variable-reserved-word . "variable cannot be a reserved word")
    (:invalid-index . "invalid dimension index")
    (:argument-error . "invalid argument")
    (:invalid-function-name . "invalid function name")
    (:next-without-for . "NEXT without FOR"))
  "Error codes and print strings.")

;; error code string
(defmacro basic-error-code-string (error)
  "Return error string from ERROR code."
  `(cdr (assoc ,error basic-error-codes)))

;; error structure
(cl-defstruct basic-error
  code                                  ; code from `*basic-error-codes*'
  (line 0)                              ; line number
  (column 0))                           ; column number

;; print error
(defun basic-print-error (basic-error)
  "Return `basic-error' in GNU standard error format:

\"LINENO:COLUMN: MESSAGE.\""
  (format "%d:%d: %s"
          (basic-error-line basic-error)
          (basic-error-column basic-error)
          (basic-error-code-string (basic-error-code basic-error))))

;;; Basic Mode

;; mode map
(defconst basic-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") 'basic-send-paragraph)
    ;; (define-key map (kbd "C-c C-r") 'basic-send-region)
    ;; (define-key map (kbd "C-c C-s") 'basic-send-string)
    ;; (define-key map (kbd "C-c C-b") 'basic-send-buffer)
    ;;(define-key map (kbd "C-c C-c") 'basic-compile)
    (define-key map (kbd "C-c C-c") 'basic-run)
    (define-key map (kbd "C-c C-r") 'basic-run)
    map)
  "Mode map used for `basic-mode'.")

;;;###autoload
;;(define-derived-mode basic-mode fundamental-mode "BASIC"
(defun basic-mode ()
  "Major mode to edit, compile, and execute BASIC programs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'basic-mode)
  (setq mode-name "BASIC")
  ;; set local key map
  (use-local-map basic-mode-map)
  ;; (when basic-mode-menu
  ;;   (easy-menu-add basic-mode-menu))      ; xemacs
  ;; (set-syntax-table basic-mode-syntax-table)
  ;; set font lock (case insensitive)
  (setq font-lock-defaults '((basic-mode-font-lock-keywords) nil t))
  ;;(setq font-lock-keywords-case-fold-search t)
  ;;(font-lock-fontify-buffer)
  ;; set comment start
  (setq comment-start "REM")
  (setq comment-end "")
  ;; set buffer to case insensitive
  (setq case-fold-search t)
  ;; run hooks
  (run-hooks 'basic-mode-hook))

;;; Debug

;; clear debug buffer
(defun basic-debug-clear ()
  "Clear debugging buffer."
  (when basic-debug-log
    (save-excursion
      (get-buffer-create basic-debug-buffer-name)
      (set-buffer basic-debug-buffer-name)
      (erase-buffer)
      (emacs-lisp-mode))))

;; log to debug buffer
(defun basic-debug-log (message)
  "Write MESSAGE to debugging buffer."
  (when basic-debug-log
    (save-excursion
      (get-buffer-create basic-debug-buffer-name)
      (set-buffer basic-debug-buffer-name)
      (goto-char (point-max))
      (insert message)
      (newline))))

;;; Parser

;; list of ascii character codes and their enumerated types
(defconst basic-parse-grammar-char-type
  `(,@(cl-loop for c from (string-to-char "A") to (string-to-char "Z") collect (cons c :letter))
    ,@(cl-loop for c from (string-to-char "a") to (string-to-char "z") collect (cons c :letter))
    ,@(cl-loop for c from (string-to-char "0") to (string-to-char "9") collect (cons c :number))
    ,(cons (string-to-char "`") :back-tick)
    ,(cons (string-to-char "~") :tilde)
    ,(cons (string-to-char "!") :bang)
    ,(cons (string-to-char "@") :at)
    ,(cons (string-to-char "#") :pound)
    ,(cons (string-to-char "$") :dollar)
    ,(cons (string-to-char "%") :percent)
    ,(cons (string-to-char "^") :carret)
    ,(cons (string-to-char "&") :ampersand)
    ,(cons (string-to-char "*") :asterisk)
    ,(cons (string-to-char "(") :left-paren)
    ,(cons (string-to-char ")") :right-paren)
    ,(cons (string-to-char "-") :minus)
    ,(cons (string-to-char "_") :underscore)
    ,(cons (string-to-char "=") :equal)
    ,(cons (string-to-char "+") :plus)
    ,(cons (string-to-char "[") :left-bracket)
    ,(cons (string-to-char "]") :right-bracket)
    ,(cons (string-to-char "{") :left-curly-bracket)
    ,(cons (string-to-char "}") :right-curly-bracket)
    ,(cons (string-to-char "\\") :backslash)
    ,(cons (string-to-char "|") :pipe)
    ,(cons (string-to-char ";") :semicolon)
    ,(cons (string-to-char ":") :colon)
    ,(cons (string-to-char "'") :tick)
    ,(cons (string-to-char "\"") :quote)
    ,(cons (string-to-char ",") :comma)
    ,(cons (string-to-char ".") :period)
    ,(cons (string-to-char "<") :less-than)
    ,(cons (string-to-char ">") :greater-than)
    ,(cons (string-to-char "/") :slash)
    ,(cons (string-to-char "?") :question)
    ,@(list (cons (string-to-char " ") :whitespace)
            (cons (string-to-char "\t") :whitespace)))
  "List of characters and their types.")

;; hash table of codes to types
(defconst basic-parse-grammar-char-type-hash
  (make-hash-table)
  "Map of characters to their types.")
(dolist (item basic-parse-grammar-char-type)
  (setf (gethash (car item) basic-parse-grammar-char-type-hash)
        (cdr item)))

;; map of enumerated types to their ascii character codes
(defconst basic-parse-grammar-type-char-hash
  (make-hash-table)
  "Map of types to their characters.")
(dolist (item basic-parse-grammar-char-type)
  (let ((entry (gethash (cdr item) basic-parse-grammar-type-char-hash)))
    (if entry
        (if (listp entry)
            (setf (gethash (cdr item) basic-parse-grammar-type-char-hash)
                  (append entry (list (car item))))
          (setf (gethash (cdr item) basic-parse-grammar-type-char-hash)
                (list entry (car item))))
      (setf (gethash (cdr item) basic-parse-grammar-type-char-hash)
            (car item)))))

;; list of statements, reserverd words, and functions and their symbols
(defconst basic-parse-grammar-keyword-type
  (sort
   `(,@(cl-loop for key in basic-keywords-statements
             for dkey = (downcase key)
             collect (eval (read (concat "(cons " "\"" dkey "\"" " :" dkey ")"))))
     ,@(cl-loop for key in basic-keywords-reserved
             for dkey = (downcase key)
             collect (eval (read (concat "(cons " "\"" dkey "\"" " :" dkey ")"))))
     ,@(cl-loop for key in basic-keywords-functions
             for dkey = (downcase key)
             collect (eval (read (concat "(cons " "\"" dkey "\"" " :" dkey ")")))))
   (lambda (a b) (string< (car a) (car b)))))

;; hash table of keywords to types
(defconst basic-parse-grammar-keyword-type-hash
  (make-hash-table :test 'equal)
  "Map of keywords to their types.")
(dolist (item basic-parse-grammar-keyword-type)
  (setf (gethash (car item) basic-parse-grammar-keyword-type-hash)
        (cdr item)))

;; variable types
(defconst basic-parse-variable-types
  '(:variable-string :variable-numeric)
  "Variable types.")

;; parse statement
(defun basic-parse-statement (statement)
  "Parse basic STATEMENT into a list of tokens.

Format returned:

  ((TYPE1 . VALUE1)
   (TYPE2 . VALUE2)
   ...)"
  (let (tokens                        ; generated token list
        current                       ; current string
        (state :start)                ; current state
        minus)                        ; previous token was negative number or minus
    ;; loop over all characters in statement
    (cl-loop for char across (concat statement " ")
             for column = 1 then (+ column 1)
             with line = 0
             do (let ((type (gethash char basic-parse-grammar-char-type-hash)))
                  ;; handle states differently
                  ;; quote or rem
                  (if (or (eq state :quote) (eq state :rem))
                      ;; if inside a quote or rem, all values get added to current token
                      (if (and (eq state :quote) (eq type :quote))
                          ;; another quote is the only way to end this state
                          (progn
                            (push (cons :quote current) tokens)
                            (setq current nil
                                  state :start))
                        (setq current (concat current (char-to-string char))))
                    ;; non-quote, non-rem
                    (progn
                      ;; if inside text string, either add to it or push token
                      (when (eq state :text)
                        ;; letters and numbers are valid text
                        (if (or (eq type :letter) (eq type :number))
                            (setq current (concat current (char-to-string char)))
                          ;; else add current text token, and continue
                          (let ((string-type nil))
                            ;; if text is followed by a dollar sign, add it and set the type to string
                            (if (eq type :dollar)
                                (setq current (concat current "$")
                                      string-type :variable-string
                                      state :end)
                              ;; else make it a numeric variable
                              (setq string-type :variable-numeric
                                    state :start))
                            ;; if text is a keyword
                            (let ((key (gethash (downcase current) basic-parse-grammar-keyword-type-hash)))
                              (if key
                                  ;; if keyword is rem ignore rest of line
                                  (if (eq key :rem)
                                      (setq state :rem)
                                    ;; push keyword token
                                    (push (cons (gethash (downcase current) basic-parse-grammar-keyword-type-hash) nil) tokens))
                                ;; else, push text
                                (push (cons string-type current) tokens)))
                            (setq current nil))))
                      ;; if inside number string, either add to it or push token
                      (when (eq state :number)
                        ;; only numbers are valid
                        (if (eq type :number)
                            (setq current (concat current (char-to-string char)))
                          ;; else add current number token, and continue
                          (progn
                            ;; letters may not follow a number
                            (if (eq type :letter)
                                (push (cons :error (make-basic-error
                                                    :code :syntax-error
                                                    :line line
                                                    :column column)) tokens)
                              (push (cons :number (string-to-number current)) tokens))
                            (setq current nil
                                  state :start))))
                      ;; handle everything else
                      (when (eq state :start)
                        ;; current should be nil (internal error, if not)
                        (assert (eq current nil) (current) "Variable current not cleared")
                        ;; type should be valid
                        (if type
                            ;; ignore whitespace
                            (unless (eq type :whitespace)
                              (cond
                               ;; quote start
                               ((eq type :quote)
                                (setq current ""
                                      state :quote))
                               ;; text start
                               ((eq type :letter)
                                (setq current (char-to-string char)
                                      state :text))
                               ;; number start
                               ((eq type :number)
                                (setq current (char-to-string char)
                                      state :number)
                                (setq minus nil))
                               ;; just add any other types verbatim
                               (t
                                (push (cons type nil) tokens))))
                          ;; add error token for unknown symbol
                          (push (cons :error (make-basic-error
                                              :code :syntax-error
                                              :line line
                                              :column column)) tokens)))
                      ;; end of token, reset state
                      (when (eq state :end)
                        (setq state :start)))))
             finally (progn
                       (when (and current (plusp (length current)))
                         (setq current (substring current 0 (1- (length current)))))
                       (cl-case state
                         (:rem
                          (push (cons :rem current) tokens))
                         (:quote
                          (push (cons :error (make-basic-error
                                              :code :syntax-error
                                              :line line
                                              :column column)) tokens)))))
    ;; return tokens
    (nreverse tokens)))

;; parse buffer or region
(defun basic-parse (&optional start end)
  "Parse current buffer or region and return a list of tokens and possibly errors.

START is the starting buffer position, defaults to `point-min'.
END is the ending buffer position, defaults to `point-max'.

Format returned:

  (((LINE-NUMBER1 . (TOKEN1, TOKEN2, ...))
    (LINE-NUMBER2 . (TOKEN1, TOKEN2, ...))
    ...)
   ((ERROR-POSITION1 . ERROR-CODE1)
    (ERROR-POSITION2 . ERROR-CODE2)
    ...))

The error list is nil if there are no errors."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      ;; parse buffer and convert syntax to tokens
      (goto-char start)
      (goto-char (point-at-bol))
      ;; do not start on a partial line
      (while (< (point) start)
        (forward-line 1))
      (let (tokens                      ; token list
            errors)                     ; error list
        ;; loop through all lines
        (while (< (point) end)
          ;; parse one line at a time
          (let* ((line (buffer-substring (point-at-bol) (point-at-eol))) ; text of current line
                 (line-pos (point-at-bol)) ; point at start of line
                 (parsed (basic-parse-statement line))) ; parsed token list
            ;; if line is valid add to tokens
            (when parsed
              (if (eq (caar parsed) :number)
                  ;; if there is a line number, add to tokens
                  (progn
                    (push (cons (cdar parsed) (cdr parsed)) tokens)
                    ;; add any token errors
                    (dolist (token (cdr parsed))
                      (when (eq (car token) :error)
                        (push (cdr token) errors))))
                ;; else add missing line number to errors
                (push (make-basic-error
                       :code :missing-line-number
                       :line line-pos
                       :column 1) errors)))
            (when (< (point) end)
              (forward-line 1))))
        ;; return tokens and errors
        (list (nreverse tokens) (nreverse errors))))))

;;; Compiler

;; suppress compiler warnings for free variables
(defvar basic-line)
(defvar basic-line-number)
(defvar basic-line-numbers)
(defvar basic-errors)
(defvar basic-stack)
(defvar basic-next-stack)
(defvar basic-tokens)
(defvar basic-jumps)
(defvar basic-test-input)

;; variable not reserved
(defun basic-compile-internal-variable-not-reserved-word (variable)
  "Return non-nil if VARIABLE is not a reserved word."
  (not (gethash (downcase variable) basic-parse-grammar-keyword-type-hash)))

;; variable is a function
(defmacro basic-compile-internal-variable-is-function (variable)
  "Return non-nil if VARIABLE is a function."
  `(and (>= (length ,variable) 2)
        (string= (downcase (substring ,variable 0 2)) "fn")))

;; dim prefix
(defconst basic-dim-prefix
  "dim-"
  "Prefix value to distinguish non-dimension variable from same
  name dimension variable.")

;; make list of jump targets
(defun basic-compile-internal-jumps (tokens)
  "Return list of jump targets found in TOKENS."
  (let (jumps                           ; jump targets
        add-num)                        ; need to add next line number
    (dolist (line tokens)
      (when add-num
        (pushnew (car line) jumps)
        (setq add-num nil))
      (cl-do ((token (cdr line) (cdr token)))
          ((or (not token) (eq (caar token) :rem)))
        ;; direct jump targets
        (when (and
               (or
                (eq (caar token) :gosub)
                (eq (caar token) :goto)
                (eq (caar token) :then))
               (and
                (cadr token)
                (eq (caadr token) :number)))
          (pushnew (cdadr token) jumps))
        ;; indirect jump targets (adds next line number)
        (when (or (eq (caar token) :for)
                  (eq (caar token) :gosub)
                  (eq (caar token) :next)
                  (eq (caar token) :then))
          (setq add-num t))))
    (sort jumps #'<)))

;; make list of data items
(defun basic-compile-internal-data (tokens)
  "Return list of data items found in TOKENS."
  (let (data                            ; data items
        in-data)                        ; non-nil if in data line
    (dolist (line tokens)
      (setq in-data nil)
      (cl-do ((token (cdr line) (cdr token)))
          ((or (not token) (eq (caar token) :rem)))
        (if in-data
            (if (eq (caar token) :colon)
                (setq in-data nil)
              (when (not (eq (caar token) :comma))
                (push (cdar token) data)))
          (when (eq (caar token) :data)
            (setq in-data t)))))
    (nreverse data)))

;; push error
(defun basic-compile-internal-push-error (code &optional text)
  "Add an error to `basic-errors' and clear current `basic-line'.
\nThe error is generated from `basic-line-number', CODE, and
optional TEXT."
  (setq basic-line nil)
  (push (cons basic-line-number (cons code text)) basic-errors))

;; look
(defun basic-compile-internal-look (key &optional value)
  "Look ahead in `basic-line' for KEY, return VALUE or t if successful, nil if not."
  (if (and basic-line
           (or
            (eq (caar basic-line) key)
            (and (eq key :variable)
                 (member (caar basic-line) basic-parse-variable-types)))
           (or (not value) (string-equal (upcase (cdar basic-line)) (upcase value))))
      (let ((value (cdar basic-line)))
        (or value t))
    nil))

;; soft match
(defun basic-compile-internal-soft-match (key &optional value)
  "Match KEY in `basic-line' and consume it, return VALUE or t if successful, or nil if not."
  (if (and basic-line
           (or
            (eq (caar basic-line) key)
            (and (eq key :variable)
                 (member (caar basic-line) basic-parse-variable-types)))
           (or (not value) (string-equal (upcase (cdar basic-line)) (upcase value))))
      (let ((value (cdar basic-line)))
        (setq basic-line (cdr basic-line))
        (or value t))
    nil))

;; match
(defun basic-compile-internal-match (key &optional value)
  "Match KEY in `basic-line' and consume it, return VALUE if successful, or push an error if not."
  (let ((value (basic-compile-internal-soft-match key value)))
    (or value
        ;; (error "Expected '%S' at %S" (gethash key basic-parse-grammar-type-char-hash) basic-line-number)))
        (basic-compile-internal-push-error :expected-value (gethash key basic-parse-grammar-type-char-hash)))))

;; match number
(defun basic-compile-internal-match-number ()
  "Match either an integer or a floating point number."
  (let ((num (if (basic-compile-internal-look :period)
                 0
               (basic-compile-internal-match :number))))
    (if (basic-compile-internal-soft-match :period)
        (string-to-number (concat
                           (number-to-string num)
                           "."
                           (number-to-string (basic-compile-internal-match :number))))
      num)))

;; push stack
(defun basic-compile-internal-push-stack (value)
  "Push VALUE onto `basic-stack'."
  (push value basic-stack))

;; pop stack
(defun basic-compile-internal-pop-stack ()
  "Pop contents of `basic-stack', push an error if stack is empty."
  (or (pop basic-stack)
      (basic-compile-internal-push-error :missing-left-hand-value)))

;; compile a string expression
(defun basic-compile-internal-compile-string ()
  "Return compiled basic string expression on `basic-line'."
  (let ((str ""))
    (cond
     ;; quoted text
     ((basic-compile-internal-look :quote)
      (while (and basic-line (basic-compile-internal-look :quote))
        (setq str (concat str (basic-compile-internal-match :quote)))))
     ;; string variable
     ((basic-compile-internal-look :variable-string)
      (let ((var (basic-compile-internal-match :variable-string)))
        (cond
         ;; function variable
         ((basic-compile-internal-variable-is-function var)
          (basic-compile-internal-soft-match :left-paren)
          (let ((value (basic-compile-internal-compile-expression)))
            (basic-compile-internal-match :right-paren)
            (if value
                (setq str `(funcall (funcall var-get ,var) ,value))
              (basic-compile-internal-push-error :syntax-error))))
         ;; dim variable
         ((basic-compile-internal-soft-match :left-paren)
          (let (dims)
            (push (basic-compile-internal-compile-equation) dims)
            (while (basic-compile-internal-soft-match :comma)
              (push (basic-compile-internal-compile-equation) dims))
            (basic-compile-internal-match :right-paren)
            (if (basic-compile-internal-variable-not-reserved-word var)
                (setq str `(funcall dim-get ,var (quote ,(nreverse dims))))
              (basic-compile-internal-push-error :variable-reserved-word var))))
         ;; regular variable
         ((basic-compile-internal-variable-not-reserved-word var)
          (setq str `(funcall var-get ,var)))
         ;; reserved word
         (t
          (basic-compile-internal-push-error :variable-reserved-word var)))))
     (t
      (basic-compile-internal-push-error :syntax-error)))
    ;; ;; remove any connecting plus
    ;; (basic-compile-internal-soft-match :plus)
    ;; concatenate with more string values or return string
    (if (basic-compile-internal-soft-match :plus)
        `(concat ,str ,(basic-compile-internal-compile-expression))
      ;; return string value
      str)))

;; compile a function
(defun basic-compile-internal-compile-function ()
  "Compile a function."
  (cond
   ((basic-compile-internal-soft-match :abs)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(abs ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :asc)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(string-to-char ,(basic-compile-internal-compile-string)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :atn)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(atan ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :chr$)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(char-to-string ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :cos)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(cos ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :exp)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(exp ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :int)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(floor ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :left$)
    (basic-compile-internal-match :left-paren)
    (let ((str (basic-compile-internal-compile-string)))
      (basic-compile-internal-match :comma)
      (let ((num (basic-compile-internal-compile-expression)))
        ;; (basic-compile-internal-push-stack `(if (< (length ,str) 0)
        ;;                                         (error :argument-error num)
        ;;                                       (if (< (length ,str) ,num)
        ;;                                           ,str
        ;;                                         (substring ,str 0 ,num))))))
        (basic-compile-internal-push-stack `(if (< (length ,str) ,num)
                                                ,str
                                              (substring ,str 0 ,num)))))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :len)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(length ,(basic-compile-internal-compile-string)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :log)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(log ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :mid$)
    (basic-compile-internal-match :left-paren)
    (let ((str (basic-compile-internal-compile-string)))
      (basic-compile-internal-match :comma)
      (let ((num1 (basic-compile-internal-compile-expression))
            num2)
        (if (basic-compile-internal-soft-match :comma)
            (setq num2 (basic-compile-internal-compile-expression))
          (setq num2 (length str)))
        (basic-compile-internal-push-stack `(if (< (length ,str) ,num1)
                                                ""
                                              (if (< (length ,str) (+ ,num1 ,num2))
                                                  (substring ,str (1- ,num1))
                                                (substring ,str (1- ,num1) (+ ,num2 (1- ,num1))))))))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :rnd)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(funcall funct-rnd ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :right$)
    (basic-compile-internal-match :left-paren)
    (let ((str (basic-compile-internal-compile-string)))
      (basic-compile-internal-match :comma)
      (let ((num (basic-compile-internal-compile-expression)))
        (basic-compile-internal-push-stack `(if (< (length ,str) ,num)
                                                ,str
                                              (substring ,str (- (length ,str) ,num))))))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :sgn)
    (basic-compile-internal-match :left-paren)
    (let ((num (basic-compile-internal-compile-expression)))
      (basic-compile-internal-push-stack `(cond
                                           ((minusp ,num) -1)
                                           ((plusp ,num) 1)
                                           (t 0))))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :sin)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(sin ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :sqr)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(sqrt ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :str$)
    (basic-compile-internal-match :left-paren)
    (let ((num (basic-compile-internal-compile-expression)))
      ;;(basic-compile-internal-push-stack `(concat (if (minusp ,num) "" " ") (number-to-string ,num))))
      (basic-compile-internal-push-stack `(number-to-string ,num)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :tan)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(tan ,(basic-compile-internal-compile-equation)))
    (basic-compile-internal-match :right-paren)
    t)
   ((basic-compile-internal-soft-match :val)
    (basic-compile-internal-match :left-paren)
    (basic-compile-internal-push-stack `(string-to-number ,(basic-compile-internal-compile-string)))
    (basic-compile-internal-match :right-paren)
    t)
   (t
    nil)))

;; compile equation level loop
(defun basic-compile-internal-compile-equation-level-loop (level)
  "Compile equation using state levels in a stack with recursive calls."
  (cond
   ;; main entry point
   ((eq level 1)
    (basic-compile-internal-compile-equation-level-loop 2))
   ;; short circuit or
   ((eq level 2)
    (basic-compile-internal-compile-equation-level-loop 3)
    (while (basic-compile-internal-soft-match :or)
      (basic-compile-internal-push-stack `(or ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 2)))))
   ;; short circuit and
   ((eq level 3)
    (basic-compile-internal-compile-equation-level-loop 4)
    (while (basic-compile-internal-soft-match :and)
      (basic-compile-internal-push-stack `(and ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 3)))))
   ;; comparisons
   ((eq level 4)
    (basic-compile-internal-compile-equation-level-loop 5)
    (cond
     ;; equality
     ((basic-compile-internal-soft-match :equal)
      (basic-compile-internal-push-stack `(equal ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4))))
     ;; un-equality, less than, or less than or equal
     ((basic-compile-internal-soft-match :less-than)
      (if (basic-compile-internal-soft-match :greater-than)
          (basic-compile-internal-push-stack `(not (equal ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4))))
        (if (basic-compile-internal-soft-match :equal)
            (basic-compile-internal-push-stack `(<= ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4)))
          (basic-compile-internal-push-stack `(< ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4))))))
     ;; greater than or greater than or equal
     ((basic-compile-internal-soft-match :greater-than)
      (if (basic-compile-internal-soft-match :equal)
          (basic-compile-internal-push-stack `(>= ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4)))
        (basic-compile-internal-push-stack `(> ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 4)))))))
   ;; addition/subtraction
   ((eq level 5)
    (basic-compile-internal-compile-equation-level-loop 6)
    (cond
     ;; addition
     ((basic-compile-internal-soft-match :plus)
      ;; determine if adding numbers or concatenating strings
      (if (or
           (basic-compile-internal-look :quote)
           (basic-compile-internal-look :variable-string)
           (basic-compile-internal-look :chr$)
           (basic-compile-internal-look :left$)
           (basic-compile-internal-look :mid$)
           (basic-compile-internal-look :right$)
           (basic-compile-internal-look :str$))
          (basic-compile-internal-push-stack `(concat ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-expression)))
        (basic-compile-internal-push-stack `(+ ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 5)))))
     ;; subtraction
     ((and basic-stack (basic-compile-internal-soft-match :minus))
      (basic-compile-internal-push-stack `(- ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 5))))))
   ;; multiplication/division/modulo
   ((eq level 6)
    (basic-compile-internal-compile-equation-level-loop 7)
    (cond
     ;; multiplication
     ((basic-compile-internal-soft-match :asterisk)
      (basic-compile-internal-push-stack `(* ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 6))))
     ;; division
     ((basic-compile-internal-soft-match :slash)
      (basic-compile-internal-push-stack `(/ ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 6))))
     ;; modulo
     ((basic-compile-internal-soft-match :mod)
      (basic-compile-internal-push-stack `(mod ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 6))))))
   ;; factorial
   ((eq level 7)
    (basic-compile-internal-compile-equation-level-loop 8)
    (cond
     ;; factorial
     ((basic-compile-internal-soft-match :bang)
      (basic-compile-internal-push-stack `(! ,(basic-compile-internal-pop-stack) ,(basic-compile-internal-compile-equation 7))))))
   ;; parenthesis
   ((eq level 8)
    (cond
     ;; left parenthesis
     ((basic-compile-internal-soft-match :left-paren)
      (basic-compile-internal-push-stack (basic-compile-internal-compile-equation))
      (basic-compile-internal-match :right-paren))
     ;; number
     ((or (basic-compile-internal-look :number)
          (basic-compile-internal-look :period))
      (basic-compile-internal-push-stack (basic-compile-internal-match-number)))
     ;; numeric variable
     ((basic-compile-internal-look :variable-numeric)
      (let ((var (basic-compile-internal-match :variable-numeric)))
        (cond
         ;; function variable
         ((basic-compile-internal-variable-is-function var)
          (basic-compile-internal-soft-match :left-paren)
          (let ((value (basic-compile-internal-compile-expression)))
            (basic-compile-internal-match :right-paren)
            (if value
                (basic-compile-internal-push-stack `(funcall (funcall var-get ,var) ,value))
              (basic-compile-internal-push-error :syntax-error))))
         ;; dim variable
         ((basic-compile-internal-soft-match :left-paren)
          (let (dims)
            (push (basic-compile-internal-compile-equation) dims)
            (while (basic-compile-internal-soft-match :comma)
              (push (basic-compile-internal-compile-equation) dims))
            (basic-compile-internal-match :right-paren)
            (if (basic-compile-internal-variable-not-reserved-word var)
                (basic-compile-internal-push-stack `(funcall dim-get ,var (quote ,(nreverse dims))))
              (basic-compile-internal-push-error :variable-reserved-word var))))
         ;; regular variable
         ((basic-compile-internal-variable-not-reserved-word var)
          (basic-compile-internal-push-stack `(funcall var-get ,var)))
         ;; reserved word
         (t
          (basic-compile-internal-push-error :variable-reserved-word var)))))
     ;; negation
     ((and (basic-compile-internal-soft-match :minus) (not basic-stack))
      (basic-compile-internal-push-stack `(- ,(basic-compile-internal-compile-equation 8))))
     ;; quote
     ((basic-compile-internal-look :quote)
      (basic-compile-internal-push-stack (basic-compile-internal-compile-string)))
     ;; string variable
     ((basic-compile-internal-look :variable-string)
      (basic-compile-internal-push-stack (basic-compile-internal-compile-string)))
     ;; function
     ((basic-compile-internal-compile-function)
      nil)
     (t
      (basic-compile-internal-push-error :syntax-error))))
   (t
    (error "Invalid parsing level reached"))))

;; compile an equation
(defun basic-compile-internal-compile-equation (&optional level)
  "Create a stack and call compile loop."
  (let ((level (or level 1))
        basic-stack)
    (basic-compile-internal-compile-equation-level-loop level)
    ;; stack should only contain one value
    (if (= (length basic-stack) 1)
        (progn
          (setq basic-stack (car basic-stack))
          (when basic-debug-log
            (basic-debug-log (format "Stack: %S\n" basic-stack)))
          ;; return value of stack
          basic-stack)
      (basic-compile-internal-push-error :syntax-error))))

;; compile an expression
(defun basic-compile-internal-compile-expression ()
  "Compile a basic expression."
  (if (or
       (basic-compile-internal-look :quote)
       (basic-compile-internal-look :variable-string))
      (basic-compile-internal-compile-string)
    (basic-compile-internal-compile-equation)))

;; next jump line number
(defun basic-compile-internal-next-jump-line-number (line-number jumps)
  "Return the next line number jump point after LINE-NUMBER found in JUMPS."
  (car (cl-remove-if (lambda (x) (<= x line-number)) jumps)))

;; add a block of tokens
(defun basic-compile-internal-compile-block ()
  "Compile and return a block of tokens from `basic-tokens'."
  (let (code
        done)
    ;; loop through lines
    (while (and (not done)
                basic-tokens)
      (setq basic-line-number (caar basic-tokens)
            basic-line (cdar basic-tokens))
      ;; block ends when there are no more tokens, current line is a jump
      ;; target, or current line is the end
      (if (member basic-line-number basic-jumps)
          (progn
            ;; add jump to next line
            (push `(setq ln ,basic-line-number) code)
            (setq done t))
        (progn
          (setq basic-tokens (cdr basic-tokens))
          ;; loop through tokens in line
          (while basic-line
            ;; write lisp code for basic statements
            (cond
             ;; data
             ((basic-compile-internal-soft-match :data)
              (let ((values (list (basic-compile-internal-compile-expression))))
                (while basic-line
                  (basic-compile-internal-match :comma)
                  (push (basic-compile-internal-compile-expression) values))
                ;; ignored since data is setup at program initialization
                ;;(push `(funcall data-append (quote ,(nreverse values))) code)))
                ))
             ;; def
             ((basic-compile-internal-soft-match :def)
              (let* ((var-is-numeric (basic-compile-internal-look :variable-numeric))
                     (var (basic-compile-internal-match (if var-is-numeric :variable-numeric :variable-string))))
                (if (basic-compile-internal-variable-is-function var)
                    (progn
                      (basic-compile-internal-match :left-paren)
                      (let ((fnvar (basic-compile-internal-match (if var-is-numeric :variable-numeric :variable-string))))
                        (basic-compile-internal-match :right-paren)
                        (basic-compile-internal-match :equal)
                        (let ((value (basic-compile-internal-compile-expression)))
                          (if value
                              (push `(funcall var-set ,var (quote (lambda (x)
                                                                    (let (r)
                                                                      (setq fnvar (cons ,fnvar x))
                                                                      (setq r ,value)
                                                                      (setq fnvar (cons nil nil))
                                                                      r))))
                                    code)
                            (basic-compile-internal-push-error :syntax-error)))))
                  (basic-compile-internal-push-error :invalid-function-name))))
             ;; dim
             ((basic-compile-internal-soft-match :dim)
              (while basic-line
                (let* ((var-is-numeric (basic-compile-internal-look :variable-numeric))
                       (var (basic-compile-internal-match (if var-is-numeric :variable-numeric :variable-string)))
                       dims)
                  (basic-compile-internal-match :left-paren)
                  (push (basic-compile-internal-compile-expression) dims)
                  (while (basic-compile-internal-soft-match :comma)
                    (push (basic-compile-internal-compile-expression) dims))
                  (basic-compile-internal-match :right-paren)
                  (push `(let ((vector-size (* ,@dims))
                               (vector-init (if ,var-is-numeric 0 "")))
                           (funcall var-set ,(concat basic-dim-prefix var) (list (quote ,(nreverse dims))
                                                       (make-vector vector-size vector-init)))) code)
                  (when basic-line
                    (basic-compile-internal-match :comma)))))
             ;; end
             ((basic-compile-internal-soft-match :end)
              ;; end of program; end block
              (push `(setq done t) code)
              (setq done t))
             ;; error
             ((basic-compile-internal-soft-match :error)
              ;; should not be here if there were errors
              (error "Compiler called with parsing errors"))
             ;; for
             ((basic-compile-internal-soft-match :for)
              ;; expecting numeric variable assignment
              (let ((var (basic-compile-internal-match :variable-numeric)))
                (basic-compile-internal-match :equal)
                (let ((value1 (basic-compile-internal-compile-expression)))
                  (basic-compile-internal-match :to)
                  (let ((value2 (basic-compile-internal-compile-expression))
                        (step (if (basic-compile-internal-soft-match :step)
                                  (basic-compile-internal-compile-expression)
                                1))
                        (jump (basic-compile-internal-next-jump-line-number basic-line-number basic-jumps)))
                    (if (and value1 value2 step jump)
                        (progn
                          (push `(funcall var-set ,var ,value1) code)
                          (push (list jump var value1 value2 step) basic-next-stack))
                      (basic-compile-internal-push-error :syntax-error))))))
             ;; gosub/return
             ((basic-compile-internal-soft-match :gosub)
              ;; set jump location (TODO: can this be an equation?)
              (let ((goto (basic-compile-internal-soft-match :number))
                    (jump (basic-compile-internal-next-jump-line-number basic-line-number basic-jumps)))
                (push `(setq ln ,goto) code)
                ;; push next line number onto the jump stack
                (push `(funcall jump-push ,jump) code)
                (setq done t)))
             ((basic-compile-internal-soft-match :return)
              ;; get jump target; end block
              (push `(setq ln (funcall jump-pop)) code)
              (setq done t))
             ;; goto
             ((basic-compile-internal-soft-match :goto)
              ;; set jump location (TODO: can this be an equation?)
              (let ((goto (basic-compile-internal-soft-match :number)))
                (push `(setq ln ,goto) code)
                (setq done t)))
             ;; if/then
             ((basic-compile-internal-soft-match :if)
              ;; expecting truth equation
              (let ((value (basic-compile-internal-compile-equation)))
                (basic-compile-internal-match :then)
                (if (basic-compile-internal-look :number)
                    ;; a number after then is a goto
                    (let ((goto (basic-compile-internal-match :number))
                          (jump (basic-compile-internal-next-jump-line-number basic-line-number basic-jumps)))
                      (push `(if ,value
                                 (setq ln ,goto)
                               (setq ln ,jump)) code)
                      (setq done t))
                  ;; non-goto then
                  (let ((jump (basic-compile-internal-next-jump-line-number basic-line-number basic-jumps)))
                    (push `(setq ln ,jump) code)
                    (push `(when ,value ,@(basic-compile-internal-compile-block)) code)
                    (setq done t)))))
             ;; input
             ((basic-compile-internal-soft-match :input)
              (unless (basic-compile-internal-look :variable)
                ;; print different data types
                (while (and basic-line
                            (not (or (basic-compile-internal-look :semicolon)
                                     (basic-compile-internal-look :comma))))
                  ;; assume it is an expression
                  (push `(insert ,(basic-compile-internal-compile-expression)) code))
                ;; expecting ", VAR" or "; VAR" or just "VAR" at the end
                (or (basic-compile-internal-soft-match :semicolon)
                    (and (basic-compile-internal-match :comma)
                         (push `(funcall tab-stop) code))))
              ;; expecting variable(s)
              (unless (basic-compile-internal-look :variable)
                ;; force parsing error
                (basic-compile-internal-match :variable))
              (while (basic-compile-internal-look :variable)
                (cond
                 ((basic-compile-internal-look :variable-numeric)
                  (let ((var (basic-compile-internal-match :variable-numeric)))
                    (push `(let (input)
                             (while (not input)
                               (if (and basic-test-mode basic-test-input)
                                   (setq input (pop basic-test-input))
                                 (setq input (read-from-minibuffer "? " "" nil t 'minibuffer-history "0"))))
                             (insert (format "%s\n" input))
                             (funcall var-set ,var input))
                          code)))
                 ((basic-compile-internal-look :variable-string)
                  (let ((var (basic-compile-internal-match :variable-string)))
                    (push `(let (input)
                             (while (not input)
                               (if (and basic-test-mode basic-test-input)
                                   (setq input (pop basic-test-input))
                                 (setq input (format "%s" (read-from-minibuffer "? " "" nil nil 'minibuffer-history)))))
                             (insert (format "%s\n" input))
                             (funcall var-set ,var input))
                          code)))
                 (t
                  ;; force parsing error
                  (basic-compile-internal-match :variable)))
                (and (basic-compile-internal-soft-match :comma)
                     (push `(funcall tab-stop) code))))
             ;; next
             ((basic-compile-internal-soft-match :next)
              (basic-compile-internal-soft-match :variable-numeric)
              (let ((values (pop basic-next-stack)))
                (if values
                    (let* ((goto (nth 0 values))
                           (var (nth 1 values))
                           (value1 (nth 2 values))
                           (value2 (nth 3 values))
                           (step (nth 4 values))
                           (jump (basic-compile-internal-next-jump-line-number goto basic-jumps)))
                      (push `(funcall var-set ,var (+ (funcall var-get ,var) ,step)) code)
                      (push `(if (minusp ,step)
                                 (if (>= (funcall var-get ,var) ,value2)
                                     (setq ln ,goto)
                                   (setq ln ,jump))
                               (if (<= (funcall var-get ,var) ,value2)
                                   (setq ln ,goto)
                                 (setq ln ,jump)))
                            code)
                      (setq done t))
                  (basic-compile-internal-push-error :next-without-for))))
             ;; on/goto/gosub
             ((basic-compile-internal-soft-match :on)
              (let ((on (basic-compile-internal-compile-equation))
                    (is-goto (basic-compile-internal-look :goto))
                    (is-gosub (basic-compile-internal-look :gosub))
                    (jump (basic-compile-internal-next-jump-line-number basic-line-number basic-jumps)))
                (if is-goto
                    (basic-compile-internal-match :goto)
                  (basic-compile-internal-match :gosub))
                (let (targets)
                  (push (basic-compile-internal-compile-expression) targets)
                  (while (basic-compile-internal-soft-match :comma)
                    (push (basic-compile-internal-compile-expression) targets))
                  (push `(if (and (plusp ,on) (<= ,on (length ,targets)))
                             (setq ln (nth (1- ,on) ,targets))
                           (setq ln ,jump))
                        code)
                  (when is-gosub
                    ;; push next line number onto the jump stack
                    (push `(funcall jump-push ,jump) code))
                  (setq done t))))
             ;; print
             ((basic-compile-internal-soft-match :print)
              (let (separator-ending)
                ;; print different data types
                (while basic-line
                  (setq separator-ending nil)
                  (if (or (basic-compile-internal-soft-match :semicolon)
                          (and (basic-compile-internal-soft-match :comma)
                               (push `(funcall tab-stop) code)))
                      ;; separator; do nothing
                      (setq separator-ending t)
                    ;; check for tab function
                    (if (basic-compile-internal-soft-match :tab)
                        (progn
                          (basic-compile-internal-match :left-paren)
                          (push `(funcall funct-tab ,(basic-compile-internal-compile-equation)) code)
                          (basic-compile-internal-match :right-paren))
                      ;; otherwise, assume it is an expression
                      (push `(insert (format "%s" ,(basic-compile-internal-compile-expression))) code))))
                ;; after every print, add a carriage return (unless ending with a separator) and draw the screen
                (when (not separator-ending)
                  (push '(newline) code))
                (push '(sit-for 0) code)))
             ;; read
             ((basic-compile-internal-soft-match :read)
              (while basic-line
                ;; FIXME: does not handle dim variables
                (cond
                 ((basic-compile-internal-look :variable-numeric)
                  (let ((var (basic-compile-internal-match :variable-numeric)))
                    (push `(funcall var-set ,var (funcall data-read)) code)))
                 ((basic-compile-internal-look :variable-string)
                  (let ((var (basic-compile-internal-match :variable-string)))
                    (push `(funcall var-set ,var (funcall data-read)) code)))
                 (t
                  (basic-compile-internal-push-error :syntax-error)))
                (basic-compile-internal-soft-match :comma)))
             ;; rem
             ((basic-compile-internal-soft-match :rem)
              ;; do nothing; clear rest of line
              (setq basic-line nil))
             ;; restore
             ((basic-compile-internal-soft-match :restore)
              (push `(setq data-position 0) code))
             ;; stop
             ((basic-compile-internal-soft-match :stop)
              ;; stop program (same as end); end block
              (push `(setq done t) code)
              (setq done t))
             ;; let (optional statement)
             ((or
               (basic-compile-internal-soft-match :let)
               t)
              ;; expecting variable assignment
              (cond
               ((or
                (basic-compile-internal-look :variable-numeric)
                (basic-compile-internal-look :variable-string))
                (let* ((var-is-numeric (basic-compile-internal-look :variable-numeric))
                       (var (basic-compile-internal-match (if var-is-numeric :variable-numeric :variable-string))))
                  (cond
                   ;; dim variable
                   ((basic-compile-internal-soft-match :left-paren)
                    (let (dims)
                      (push (basic-compile-internal-compile-expression) dims)
                      (while (basic-compile-internal-soft-match :comma)
                        (push (basic-compile-internal-compile-expression) dims))
                      (basic-compile-internal-match :right-paren)
                      (basic-compile-internal-match :equal)
                      (if (basic-compile-internal-variable-not-reserved-word var)
                          (let ((value (basic-compile-internal-compile-expression)))
                            (if (and value dims)
                                (push `(funcall dim-set ,var (quote ,(nreverse dims)) ,value) code)
                              (basic-compile-internal-push-error :syntax-error)))
                        (basic-compile-internal-push-error :variable-reserved-word))))
                   ;; regular variable
                   (t
                    (basic-compile-internal-match :equal)
                    (if (basic-compile-internal-variable-not-reserved-word var)
                        (let ((value (basic-compile-internal-compile-expression)))
                          (if value
                              (push `(funcall var-set ,var ,value) code)
                            (basic-compile-internal-push-error :syntax-error)))
                      (basic-compile-internal-push-error :variable-reserved-word))))))
               (t
                ;; force parsing error
                (basic-compile-internal-match :variable))))
             )))))
    ;; return code
    (nreverse code)))

;; compile tokens into elisp code
(defun basic-compile (tokens)
  "Compile TOKENS and return a list of Emacs Lisp code and possibly errors.

Format returned:

  ((CODE)
   ((ERROR-LINE1 . ERROR-CODE1 . ERROR-TEXT1)
    (ERROR-LINE2 . ERROR-CODE2 . ERROR-TEXT2)
    ...))

The error list is nil if there are no errors."
  (let (code                            ; generated code
        basic-tokens                    ; processed tokens
        basic-errors                    ; errors
        basic-line                      ; current line tokens
        basic-line-number               ; current line number
        basic-line-numbers              ; all line numbers
        minibuffer-history)             ; input history

    ;; generate processed tokens
    ;; all colons are removed and dummy line numbers added
    (let (new-tokens)
      (cl-do ((lines tokens (cdr lines)))
          ((not lines))
        (let ((ln (caar lines))
              (tokens (cdar lines))
              (n 1)
              stack
              (is-rem (eq (caadar lines) :rem)))
          (cl-do ((token tokens (cdr token)))
              ((not token) (push (cons ln (nreverse stack)) new-tokens))
            (if (and (eq (caar token) :colon)
                     (not is-rem))
                (progn
                  (push (cons ln (nreverse stack)) new-tokens)
                  (setq ln (+ (floor ln) (* .001 n)))
                  (setq n (1+ n))
                  (setq stack nil))
              (push (car token) stack)))))
      (setq basic-tokens (nreverse new-tokens)))

    ;; debug output
    (when basic-debug-log
      (basic-debug-log "Modified Tokens:\n")
      (mapcar (lambda (x) (basic-debug-log (format "  %S" x))) basic-tokens)
      (basic-debug-log ""))

    ;; make sure last command is end
    (if basic-tokens
        (let* ((line (car (last basic-tokens)))
               (ln (+ (car line) 10))
               (token (caadr line)))
          (unless (eq token :end)
            (setq basic-tokens (append basic-tokens (list (list ln '(:end)))))))
      (setq basic-tokens (append basic-tokens (list (list 10 '(:end))))))

    ;; populate line numbers
    (setq basic-line-numbers (mapcar (lambda (x) (car x)) basic-tokens))

    ;; main section
    (let ((basic-jumps (basic-compile-internal-jumps basic-tokens)) ; list of jump targets
          (data (basic-compile-internal-data basic-tokens)) ; list of data items
          basic-next-stack                            ; for/next stack
          (start-line (caar basic-tokens)))           ; starting line number
      (when basic-debug-log
        (basic-debug-log (format "Jumps: %S\n" basic-jumps))
        (basic-debug-log (format "Data: %S\n" data)))
      ;; main block
      (setq code
            `(let (done
                   (ln ,start-line)
                   (variables (make-hash-table :test 'equal))
                   (fnvar (cons nil nil))
                   jump-stack
                   (data (quote ,data))
                   (data-position 0)
                   (rnd (random t)))
               ;; loop until done is set
               (while (not done)
                 ;; cond is used for jump locations (goto, gosub, and then targets)
                 (cond
                  ;; add a block for the starting line
                  ((= ln ,start-line)
                   ,@(basic-compile-internal-compile-block))
                  ;; continue to add blocks whenever a jump line is encountered
                  ,@(let (code)
                      (while basic-tokens
                        ;; remove line number from jumps to prevent an infinite loop
                        (setq basic-jumps (remove (caar basic-tokens) basic-jumps))
                        (push
                         `((= ln ,(caar basic-tokens))
                           ,@(basic-compile-internal-compile-block))
                         code))
                      (nreverse code))
                  (t
                   (error "Line number %s does not exist" ln))))))
      ;; wrap code in lambda/funcall and standard functions
      (setq code
            `(funcall
              (lambda ()
                ;; outer let block
                (let (
                      ;; function to set run-time variable values
                      ;; TODO: add data type check
                      (var-set
                       (lambda (var value)
                         (when (and (not ,(macroexpand '(basic-compile-internal-variable-is-function var)))
                                    (listp value))
                           (setq value (list (mapcar (lambda (x) (eval x)) (car value))
                                             (cadr value))))
                         (if (and (car fnvar) (string= var (car fnvar)))
                             (setf (cdr fnvar) value)
                           (setf (gethash var variables) value))))
                      ;; function to get run-time variable values
                      (var-get
                       (lambda (var)
                         (let ((value (if (and (car fnvar) (string= var (car fnvar)))
                                          (cdr fnvar)
                                        (gethash var variables))))
                           (if value
                               value
                             (if (string= (substring var (1- (length var)) (length var)) "$")
                                 ""
                               0)))))
                             ;; (error "Variable '%s' not declared" var)))))
                      ;; function to push a line number onto the jump/return stack
                      (jump-push
                       (lambda (num)
                         (push num jump-stack)))
                      ;; function to pop a line number from the jump/return stack
                      (jump-pop
                       (lambda ()
                         (let ((value (pop jump-stack)))
                           (if value
                               value
                             (error "Return without gosub")))))
                      ;; ;; function to append an element onto the data list
                      ;; (data-append
                      ;;  (lambda (values)
                      ;;    (setq data (append data values))))
                      ;; function to read next data element from the data list
                      (data-read
                       (lambda ()
                         (let ((value (nth data-position data)))
                           (setq data-position (1+ data-position))
                           (if value
                               value
                             (error "Out of data")))))
                      ;; function to set run-time dim variable values
                      (dim-set
                       (lambda (var index value)
                         (let* ((dim (funcall var-get (concat ,basic-dim-prefix var)))
                                (vector (cadr dim))
                                (vector-index (funcall dim-get-index dim index)))
                           (setf (aref vector vector-index) value))))
                      ;; function to get run-time dim variable values
                      (dim-get
                       (lambda (var index)
                         (let* ((dim (funcall var-get (concat ,basic-dim-prefix var)))
                                (vector (cadr dim))
                                (vector-index (funcall dim-get-index dim index)))
                           (aref vector vector-index))))
                      ;; function to get run-time dim vector-index from dim-index
                      (dim-get-index
                       (lambda (dim index)
                         (let* ((dims (car dim))
                                (vector (cadr dim))
                                (size (length dims)))
                           (if (and (listp dims)
                                    (vectorp vector))
                               (if (= size (length index))
                                   (cl-do ((i index (cdr index))
                                        (d dims (cdr dims))
                                        (v 1))
                                       ((not i) (1- v))
                                     (let ((iv (eval (car i))))
                                       (if (<= iv (car d))
                                           (setq v (* v iv))
                                         (error "Invalid dimension index: %s" index))))
                                 (error "Invalid dimension index: %s" index))
                             (error "Variable '%s' is not a dimension array" var)))))
                      ;; function to get random numbers
                      (funct-rnd
                       (lambda (num)
                         (cond
                          ((minusp num)
                           (setq rnd (random (number-to-string num))))
                          ((plusp num)
                           (setq rnd (random))))
                         (when (minusp rnd)
                           (setq rnd (* -1 rnd)))
                         (while (>= rnd 1)
                           (setq rnd (/ rnd 10.0)))
                         rnd))
                      ;; function to tab over when printing
                      (funct-tab
                       (lambda (num)
                         (if (< num 1)
                             (error "Invalid TAB value: %s" num)
                           (when (> (- (point) (point-at-bol)) num) (newline))
                           (insert (spaces-string (- num (- (point) (point-at-bol)) 1))))))
                      ;; function to move to the next tab-stop
                      (tab-stop
                       (lambda ()
                         (insert (spaces-string (- 8 (mod (- (point) (point-at-bol)) 8))))))
                      ;; end of standard functions
                      )
                  ;; generated code
                  ,code))))
      ;; debug log
      (when basic-debug-log
        (basic-debug-log "Code:\n")
        (basic-debug-log (with-output-to-string (pp code))))
      ;; return code and errors
      (cons code (nreverse basic-errors)))))

;; compile and run buffer
;;;###autoload
(defun basic-run (&optional buffer)
  "Compile and run a BASIC program in BUFFER.
\nBUFFER defaults to `current-buffer'."
  (interactive)
  (when basic-debug-log
    (basic-debug-clear))
  (let ((buffer (or buffer (current-buffer)))
        output-buffer)
    (save-current-buffer
      (set-buffer buffer)
      ;; parse buffer
      (let* ((parsed (basic-parse))       ; parsed tokens and errors
             (tokens (car parsed))        ; token list
             (errors (cadr parsed))       ; error list
             (output-buffer (get-buffer-create basic-output-buffer-name))) ; output buffer
        ;; debug output
        (when basic-debug-log
          (basic-debug-log "Tokens:\n")
          (mapc (lambda (x) (basic-debug-log (format "  %S" x))) tokens)
          (basic-debug-log "")
          (when errors
            (basic-debug-log "Errors:")
            (mapc (lambda (x) (basic-debug-log (format "  %S" x))) errors)
            (basic-debug-log "")))
        ;; setup output buffer
        (delete-other-windows)
        (split-window-vertically)
        (other-window 1)
        (set-buffer output-buffer)
        (switch-to-buffer output-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (if errors
            ;; if errors found, output them
            (progn
              (insert "Parser Errors:\n")
              (mapc (lambda (x)
                      (insert (format "  %s\n" (basic-print-error x))))
                    errors))
          ;; else, continue
          (progn
            ;; compile basic token hash
            (let* ((result (basic-compile tokens))
                   (code (car result))
                   (errors (cdr result)))
              (if errors
                  ;; if errors found, output them
                  (progn
                    (insert "Compiler Errors:\n")
                    (mapc (lambda (x)
                            (insert (format "  %s\t%s%s\n"
                                            (car x)
                                            (basic-error-code-string (cadr x))
                                            (if (cddr x)
                                                (concat ": " (cddr x))
                                              ""))))
                          errors))
                ;; else, run the generated code
                (progn
                  ;; (insert "Code:\n")
                  ;; (mapc (lambda (x) (insert (format "  %S\n" x))) code)
                  ;; run code
                  (eval code))))))
        ;; make output buffer read-only
        (setq buffer-read-only t)))))

;; used for writing tests
(defun basic-run-test (&rest input)
  "Version of `basic-run' for use with test code.

INPUT is used to respond to input prompts."
  (let ((basic-test-mode t)
        (basic-test-input (car input)))
    (basic-run)))

(provide 'basic)

;;; basic.el ends here

;; Local Variables:
;; byte-compile-warnings: (not mapcar)
;; End:
