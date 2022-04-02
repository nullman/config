;;; tsql.el --- customizes sql.el for T-SQL
;;
;;; Copyright (C) 2005 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2005-01-01
;; Version:  1.0
;; Keywords: SQL TSQL mode
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
;; sql-indent indents SQL statements and was specifically written to indent
;; Microsoft Transact SQL code. Indentation of the current line is based upon
;; indentation of previous lines and looking backward through the code for
;; indentation hints.
;;
;; Lines will be indented to the same indentation level of the previous line
;; unless the previous line is a begin, is part of a DML statement, or the
;; current line is an end statement. If the previous line is a begin
;; statement, the current line will be indented to the right compared to the
;; begin statement. If the previous line is part of a DML statement, the
;; statement is indented so that all portions of the statement line up to the
;; same tab stop. If the current line is an end statement, then it will be
;; outdented compared to the previous line.
;;
;; Code inspection is performed via one of several regular expressions. These
;; regular expressions are defined in customization variables.
;;
;;; Installation:
;;
;; This library should be loaded once SQL mode has finished loading. One way
;; to accomplish this is to add the following to your .emacs file:
;;
;; (eval-after-load "sql"
;;   '(load-library "tsql"))
;;
;; Once the library is loaded, all SQL mode buffers from that point forward
;; will use this indentation function.
;;
;;; Inspiration:
;;
;; The inspiration for this code came from "sql-indent" by Alex Schroeder
;; (http://www.emacswiki.org/cgi-bin/wiki.pl?SqlIndent). Also, an excellent
;; tutorial on indenation in "An Emacs language mode creation tutorial"
;; (http://two-wugs.net/emacs/mode-tutorial.html).

;;; Code:

(require 'sql)

;;; Custom

;;(defcustom tsql-tab
;;  "4"
;;  "SQL indentation size."
;;  :group 'SQL)

;;; Font-Lock

(defvar sql-mode-tsql-font-lock-keywords nil
  "T-SQL keywords used by font-lock.")

(unless sql-mode-tsql-font-lock-keywords
  (debug)
  (let ((tsql-keywords (eval-when-compile
                         (concat "\\b"
                                 (regexp-opt '(
"add" "alter" "authorization" "backup" "begin" "break" "browse" "bulk" "cascade"
"case" "checkpoint" "close" "clustered" "coalesce" "collate" "column" "commit"
"compute" "constraint" "contains" "containstable" "continue" "convert" "cross"
"current_date" "current_time" "current_timestamp" "current_user" "cursor"
"database" "dbcc" "deallocate" "declare" "deny" "disk" "distributed" "double"
"drop" "dummy" "dump" "else" "end" "errlvl" "escape" "except" "exec" "execute"
"exit" "fetch" "file" "fillfactor" "foreign" "freetext" "freetexttable" "full"
"function" "goto" "holdlock" "identity" "identity_insert" "identitycol" "if"
"index" "inner" "intersect" "key" "kill" "lineno" "load" "national" "nocheck"
"nonclustered" "nullif" "off" "offsets" "open" "opendatasource" "openquery"
"openrowset" "openxml" "over" "percent" "plan" "precision" "primary" "print"
"proc" "procedure" "public" "raiserror" "read" "readtext" "reconfigure"
"references" "replication" "restore" "restrict" "return" "revoke" "rollback"
"rowcount" "rowguidcol" "rule" "save" "schema" "session_user" "setuser"
"shutdown" "some" "statistics" "system_user" "textsize" "then" "tran"
"transaction" "trigger" "truncate" "tsequal" "updatetext" "use" "varying"
"waitfor" "when" "while" "writetext"
) t) "\\b")))
        (tsql-reserved-words (eval-when-compile
                               (concat "\\b"
                                       (regexp-opt '(
"all" "and" "any" "as" "asc" "between" "by" "check" "create" "current" "default"
"delete" "desc" "distinct" "exists" "for" "from" "grant" "group" "having" "in"
"inner" "insert" "into" "is" "join" "left" "like" "not" "null" "of" "on"
"option" "or" "order" "outer" "public" "right" "select" "set" "table" "to" "top"
"union" "unique" "update" "user" "values" "view" "where" "with"
) t) "\\b")))
        (tsql-types (eval-when-compile
                      (concat "\\b"
                              (regexp-opt '(
"bigint" "binary" "bit" "char" "character" "cursor" "datetime" "dec" "decimal"
"float" "image" "int" "integer" "money" "nchar" "ntext" "numeric" "nvarchar"
"real" "smalldatetime" "smallint" "smallmoney" "sql_variant" "table" "text"
"timestamp" "tinyint" "uniqueidentifier" "varbinary" "varchar"
) t) "\\b")))
        (tsql-builtin-functions (eval-when-compile
                                  (concat "\\b"
                                          (regexp-opt '(
"@@total_errors" "@@total_read" "@@total_write" "@@trancount" "@@version" "abs"
"acos" "app_name" "ascii" "asin" "atan" "atn2" "case" "cast" "ceiling" "char"
"charindex" "coalesce" "collationproperty" "columnproperty" "col_length"
"col_name" "convert" "cos" "cot" "current_timestamp" "current_user"
"cursor_status" "databaseproperty" "databasepropertyex" "datalength" "dateadd"
"datediff" "datename" "datepart" "day" "db_id" "db_name" "degrees" "difference"
"exp" "filegroupproperty" "filegroup_id" "filegroup_name" "fileproperty"
"file_id" "file_name" "floor" "formatmessage" "fulltextcatalogproperty"
"fulltextserviceproperty" "getansinull" "getdate" "getutcdate" "has_dbaccess"
"host_id" "host_name" "identity" "ident_current" "ident_incr" "ident_seed"
"indexkey_property" "indexproperty" "index_col" "isdate" "isnull" "isnumeric"
"is_member" "is_srvrolemember" "left" "len" "log" "log10" "lower" "ltrim"
"month" "nchar" "newid" "nullif" "objectproperty" "object_id" "object_name"
"parsename" "patindex" "patindex" "permissions" "pi" "power" "quotename"
"radians" "rand" "replace" "replicate" "reverse" "right" "round" "rowcount_big"
"rtrim" "scope_identity" "serverproperty" "sessionproperty" "session_user"
"sign" "sin" "soundex" "space" "sql_variant_property" "sqrt" "square"
"stats_date" "str" "stuff" "substring" "suser_sid" "suser_sname" "system_user"
"tan" "textptr" "textvalid" "typeproperty" "unicode" "upper" "user" "user_id"
"user_name" "year" "fn_helpcollations" "fn_listextendedproperty"
"fn_servershareddrives" "fn_trace_geteventinfo" "fn_trace_getfilterinfo"
"fn_trace_getinfo" "fn_trace_gettable" "fn_virtualfilestats"
"fn_virtualfilestats"
) t) "\\b"))))
    (setq sql-mode-tsql-font-lock-keywords
          (list (cons tsql-keywords 'font-lock-function-name-face)
                (cons tsql-reserved-words 'font-lock-keyword-face)
                (cons tsql-types 'font-lock-type-face)
                (cons tsql-builtin-functions 'font-lock-builtin-face)))))

;;;###autoload
(defun sql-highlight-tsql-keywords ()
  "Highlight T-SQL keywords.
Set `font-lock-keywords' appropriately."
  (interactive)
  (setq font-lock-keywords sql-mode-tsql-font-lock-keywords)
  (font-lock-fontify-buffer))

;;; SQL Menu

;; (easy-menu-define
;;   sql-mode-menu sql-mode-map
;;   "Menu for `sql-mode'."
;;   '("SQL"
;;     ["Send Paragraph" sql-send-paragraph (and (buffer-live-p sql-buffer)
;;                                               (get-buffer-process sql-buffer))]
;;     ["Send Region" sql-send-region (and (or (and (boundp 'mark-active); Emacs
;;                                                  mark-active)
;;                                             (mark)); XEmacs
;;                                         (buffer-live-p sql-buffer)
;;                                        (get-buffer-process sql-buffer))]
;;     ["Send Buffer" sql-send-buffer (and (buffer-live-p sql-buffer)
;;                                         (get-buffer-process sql-buffer))]
;;     ["Show SQLi buffer" sql-show-sqli-buffer t]
;;     ["Set SQLi buffer" sql-set-sqli-buffer t]
;;     ["Pop to SQLi buffer after send"
;;      sql-toggle-pop-to-buffer-after-send-region
;;      :style toggle
;;      :selected sql-pop-to-buffer-after-send-region]
;;     ("Highlighting"
;;      ["T-SQL keywords" sql-highlight-tsql-keywords t]
;;      ["ANSI SQL keywords" sql-highlight-ansi-keywords t]
;;      ["Oracle keywords" sql-highlight-oracle-keywords t]
;;      ["Postgres keywords" sql-highlight-postgres-keywords t])))

(easy-menu-add-item sql-mode-map '("menu-bar" "SQL")
                    '("Highlighting"
                      ["T-SQL keywords" sql-highlight-tsql-keywords t]
                      ["ANSI SQL keywords" sql-highlight-ansi-keywords t]
                      ["Oracle keywords" sql-highlight-oracle-keywords t]
                      ["Postgres keywords" sql-highlight-postgres-keywords t]))

;;(define-key sql-mode-menu [Highlighting]
;;  (append '(menu-item "T-SQL keyword" (setq sql-highlight-tsql-keywords t))))

;;(setq sql-highlight-tsql-keywords t)
(setq sql-mode-font-lock-keywords sql-mode-tsql-font-lock-keywords)

;;; Syntax Table

(defvar sql-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; double-dash starts comment
    (if (string-match "XEmacs\\|Lucid" emacs-version)
        (modify-syntax-entry ?- ". 56" table)
      (modify-syntax-entry ?- ". 12b" table))
    ;; newline and formfeed end coments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    ;; single quotes (') quotes delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; underline and at symbols are word constituents
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?@ "w" table)
    table)
  "Syntax table used in `sql-mode' and `sql-interactive-mode'.")

;;; Uppercasing Keywords

(defvar sql-mode-keyword-upcase-p t
  "Whether or not to upcase SQL keywords.")

(defvar sql-mode-keyword-upcase-face
  (list 'font-lock-function-name-face
        'font-lock-keyword-face
        'font-lock-type-face
        'font-lock-builtin-face)
  "Font-lock faces that are considered keywords.
These will be uppercased if `sql-mode-keyword-upcase-p' is true.")

(defconst sql-mode-keyword-space-regexp
  "[\\t ]"
  "Regular expression that matches white-space.")

;;;###autoload
(defun sql-mode-keyword-upcase (beg end)
  "Upcase SQL keywords in range."
  (interactive "r")
  (save-excursion
    (let (face
          face-list)
      ;; goto start of range
      (goto-char beg)
      ;; find first word
      (while (and
              (< (point) end)
              (looking-at sql-mode-keyword-space-regexp))
        (forward-char))
      ;; loop from start to end of range
      (while (< (point) end)
        ;; get font-lock face of character at point
        (setq face (get-char-property (point) 'face))
        ;; if face is not nil check for keyword face
        (when face
          ;; get list of keyword faces
          (setq face-list sql-mode-keyword-upcase-face)
          ;; loop through each face in list looking for a match
          (while face-list
            (if (equal face (car face-list))
                ;; if current face is a match...
                (progn
                  ;; uppercase the word
                  (upcase-word 1)
                  ;; clear the list to end the while loop
                  (setq face-list ()))
              ;; else cdr the list
              (setq face-list (cdr face-list)))))
        ;; move to next word and continue
        (forward-word)
        (while (and
                (< (point) end)
                (looking-at sql-mode-keyword-space-regexp))
          (forward-char))))))

;;; Indenting

(defconst sql-indent-blank-regexp
  "^[\\t ]*$"
  "Regular expression that matches a blank line.")

(defconst sql-indent-begin-regexp
  ;;"^[ \t]*begin"
  "^[\\t ]*\\bbegin\\b"
  "Regular expression that matches a begin block.")

(defconst sql-indent-end-regexp
  ;;"^[ \t]*\\(end[ ]*[^)]\\|commit tran\\)"
  "^[\\t ]*\\b\\(end[\\t ]*$\\|commit tran\\)\\b"
  "Regular expression that matches an end block.
Does not match the end of an inline case statement.")

(defconst sql-indent-if-else-regexp
  ;;"^[\\t ]*\\(if\\|else\\)"
  "^[\\t ]*\\b\\(if\\|else\\)\\b"
  "Regular expression that matches an if or else statement.")

(defconst sql-indent-comment-regexp
  "^[\\t ]*\\(\\-\\-\\|\\/\\*\\)"
  "Regular expression that matches a comment begin or line.")

(defconst sql-indent-comment-line-regexp
  "^[\\t ]*\\-\\-"
  "Regular expression that matches a comment line.")

(defconst sql-indent-comment-begin-regexp
  "^.*\\/\\*"
  "Regular expression that matches a comment begin.")

(defconst sql-indent-comment-end-regexp
  "^.*\\*\\/"
  "Regular expression that matches a comment end.")

(defconst sql-indent-asterisk-regexp
  "^[\\t ]*\\*"
  "Regular expression that matches a line starting with `*'.")

(defconst sql-indent-statement-regexp
  (eval-when-compile
    (concat "^[\\t ]*\\b"
            (regexp-opt '(
"add" "alter" "as" "authorization" "backup" "break" "browse" "bulk" "cascade"
"checkpoint" "close" "clustered" "coalesce" "collate" "column" "commit"
"compute" "constraint" "contains" "containstable" "continue" "convert" "create" "cross"
"current_date" "current_time" "current_timestamp" "current_user" "cursor"
"database" "dbcc" "deallocate" "declare" "delete" "deny" "disk" "distributed"
"drop" "dummy" "dump" "else" "errlvl" "escape" "except" "exec" "execute"
"exit" "fetch" "file" "for" "freetext" "freetexttable" "full"
"go" "goto" "grant" "holdlock" "identity_insert" "identitycol" "if"
"insert" "intersect" "key" "kill" "lineno" "load" "national"
"offsets" "open" "opendatasource" "openquery"
"openrowset" "openxml" "over" "plan" "print"
"public" "raiserror" "read" "reconfigure"
"references" "replication" "restore" "restrict" "return" "revoke" "rollback"
"rowguidcol" "rule" "save" "schema" "select" "session_user" "set" "setuser"
"shutdown" "some" "statistics" "system_user" "textsize"
"truncate" "tsequal" "union" "update" "use" "varying"
"waitfor" "while"
) t) "\\b"))
  "Regular expression that matches the beginning of SQL statements.")

(defconst sql-indent-dml-regexp
  (eval-when-compile
    (concat "^[\\t ]*\\b"
            (regexp-opt '("delete" "insert" "select" "update") t) "\\b"))
  "Regular expression that matches the beginning of DML SQL statements.
Ones that start with `select', `insert', `update', or `delete'.")

(defconst sql-indent-select-regexp
  "^[\\t ]*\\bselect\\b"
  "Regular expression that matches the beginning of a select statement.")

(defconst sql-indent-insert-regexp
  "^[\\t ]*\\binsert\\b"
  "Regular expression that matches an insert statement.")

(defconst sql-indent-insert-values-regexp
  "^[\\t ]*\\binsert\\b.*\\bvalues\\b"
  "Regular expression that matches an insert/values statement.")

(defconst sql-indent-cursor-regexp
  "^[\\t ]*\\bdeclare\\b.*\\bcursor\\b[\\t ]*\\bfor\\b[\\t ]*$"
  "Regular expression that matches a declare cursor statement.")

;;;###autoload
(defun sql-indent-line-get-info ()
  "Get info about statement on current line.

Return a list containing the following:

  type:     Type of SQL statement
  keyword:  Starting SQL keyword (lowercased)
  indent:   Column number of indentation

Move point to start of statement. If in a comment block, will
move point to the start of the comment block. You may call it
again after doing `forward-word -1' to get info on the previous
statement.

Possible types are:

  bob:                   Beginning of block
  comment-line:          -- type of comment
  comment-block-begin:   /* */ type of comment (first line)
  comment-block-end:     /* */ type of comment (last line)
  comment-block-middle:  /* */ type of comment (a middle line)
  blank:                 Blank line
  begin:                 Begin statement (block begin)
  end:                   End statement (block end)
  if-else:               If or else statement
  comment:               (Should never happend; should get a more specific type, above)
  statement:             Other sql statement
  statement-select:      An sql statement that may be followed by a select
  continue:              Continuation of an sql statement"
  (interactive)                         ; remove this after debugging

  ;; init variables
  (let ((type nil)
        (keyword nil)
        (indent nil)
        (case-fold-search t))

    ;; goto beginning of line
    (beginning-of-line)

    ;; if at beginning of buffer, set type and indent
    (if (bobp)
        (progn
          (setq type 'bob)
          (setq indent 0))

      ;; else continue
      (progn

        ;; check for comment sections
        ;; goto indent
        (goto-char (+ (point-at-bol) (current-indentation)))

        ;; if in a comment, then determine what type
        (when (eq (get-char-property (point) 'face) 'font-lock-comment-face)
          (save-excursion
            ;; goto beginning of line
            (beginning-of-line)
            (cond
             ;; if at comment line, then set type
             ((looking-at sql-indent-comment-line-regexp)
              (setq type 'comment-line)
              ;; comment lines can be in the middle of comment blocks
              ;; since we can have multiple comment lines, we need to go
              ;; up until we either are not in a comment block or we see
              ;; the comment block begin
              (save-excursion
                (forward-line -1)
                (end-of-line)
                (while (and (not (bobp))
                            (eq (get-char-property (point) 'face)
                                'font-lock-comment-face))
                  (forward-char -1))
                ;; if we see a comment block begin, then we were in a comment block
                (when (looking-at sql-indent-comment-begin-regexp)
                  (setq type 'comment-block-middle))))
             ;; else if at comment block begin, then set type
             ((looking-at sql-indent-comment-begin-regexp)
              (setq type 'comment-block-begin))
             ;; else if at comment block end, then set type
             ((looking-at sql-indent-comment-end-regexp)
              (setq type 'comment-block-end))
             ;; else must be at a comment block middle, set type
             (t (setq type 'comment-block-middle)))))

        ;; if in a comment block, move to beginning of block
        (when type
          ;; move up until out of comment block
          (while (and (not (bobp))
                      (eq (get-char-property (point) 'face) 'font-lock-comment-face))
            (forward-line -1)
            (beginning-of-line)
            (unless (bobp)
              (goto-char (+ (point-at-bol) (current-indentation)))))
          ;; move down one unless at top of block and in comment block
          (unless (and (bobp)
                       (eq (get-char-property (point) 'face) 'font-lock-comment-face))
            (forward-line 1)))

        ;; if not in a comment block, then find line type
        (unless type
          ;; goto beginning of line
          (beginning-of-line)
          (cond
           ;; if at blank line, then set type
           ((looking-at sql-indent-blank-regexp)
            (setq type 'blank))
           ;; if at begin statement, then set type
           ((looking-at sql-indent-begin-regexp)
            (setq type 'begin))
           ;; if at end statement, then set type
           ((looking-at sql-indent-end-regexp)
            (setq type 'end))
           ;; if at if/else statement, then set type
           ((looking-at sql-indent-if-else-regexp)
            (setq type 'if-else))
           ;; if at comment, then set type
           ((looking-at sql-indent-comment-regexp)
            (setq type 'comment))
           ;; if at regular statement, then set type
           ((looking-at sql-indent-statement-regexp)
            (setq type 'statement)
            ;; look for special statements that may be followed by select
            ;; if at an insert with no values, set type
            (when (and
                   (looking-at sql-indent-insert-regexp)
                   (not (looking-at sql-indent-insert-values-regexp)))
              (setq type 'statement-select))
            ;; if at a declare cursor, set type
            (when (looking-at sql-indent-cursor-regexp)
              (setq type 'statement-select)))
           ;; else assume we are at a continuation statement, set type
           (t (setq type 'continue))))))

    ;; type is set at this point

    ;; goto indent
    (goto-char (+ (point-at-bol) (current-indentation)))

    ;; set keyword
    (when (stringp (thing-at-point 'word))
      (setq keyword (downcase (thing-at-point 'word))))

    ;; set indent if not set already
    (unless indent
      (setq indent (current-indentation)))
    (list type keyword indent)))

(defun sql-indent-line-get-info-root ()
  "Call sql-indent-line-get-info until a non-continue type line is reached.
\nTypes `continue' and `statement-select' are skipped."
  (let (line-info)
    (while (or (not line-info)
               (eq (car line-info) 'continue)
               (eq (car line-info) 'statement-select))
      (setq line-info (sql-indent-line-get-info)))))

(defun sql-indent-line-is-comment (type)
  "Test if type is a comment symbol."
  (or
   (eq type 'comment)
   (eq type 'comment-line)
   (eq type 'comment-block-begin)
   (eq type 'comment-block-end)
   (eq type 'comment-block-middle)))

(defun sql-indent-line-is-statement (type)
  "Test if type is a statement symbol."
  (or
   (eq type 'statement)
   (eq type 'statement-select)))

;;;###autoload
(defun sql-indent-line ()
  "Indent current SQL line."
  (interactive)
  (debug)

  ;; upcase keywords
  (when sql-mode-keyword-upcase-p
    (sql-mode-keyword-upcase (point-at-bol) (point-at-eol)))

  ;; init variables
  (let ((line-info nil)
        (line-type nil)
        (line-keyword nil)
        (line-indent nil)
        (prev-line-info nil)
        (prev-line-type nil)
        (prev-line-keyword nil)
        (prev-line-indent nil)
        (indent nil)
        (check-if-else t))

    ;; save position
    (save-excursion

      ;; get current line info
      (setq line-info (sql-indent-line-get-info))
      (setq line-type (car line-info))
      (setq line-keyword (car (cdr line-info)))
      (setq line-indent (car (cdr (cdr line-info))))

      ;; if we are at the beginning of the buffer, set indent
      (when (eq line-type 'bob)
        (setq indent line-indent))

      ;; if we are in a comment block, set indent to +3
      ;; unless line starts with *, then set to + 1
      (when (or (eq line-type 'comment-block-middle)
                (eq line-type 'comment-block-end))
        (setq indent (+ line-indent 3))
        (if (looking-at sql-indent-asterisk-regexp)
            (setq indent (- indent 2))))

      (unless indent
        ;; find the previous statement
        ;; go up lines until we find something (or hit the top)
        (while (and (not (bobp)) (not indent))

          ;; get previous line info
          (forward-line -1)
          (setq prev-line-info (sql-indent-line-get-info))
          (setq prev-line-type (car prev-line-info))
          (setq prev-line-keyword (car (cdr prev-line-info)))
          (setq prev-line-indent (car (cdr (cdr prev-line-info))))

          ;; if we are at the beginning of the buffer, set indent
          (when (eq prev-line-type 'bob)
            (setq indent prev-line-indent))

          ;; if prev is a begin statement, then set indent accordingly
          (when (eq prev-line-type 'begin)
            (setq indent (+ prev-line-indent tab-width))
            ;; do not check for if-else indent
            (setq check-if-else nil))

          ;; if at end statement, then set indent accordingly
          ;; loop until we get the real current indent
          (when (and (eq line-type 'end)
                     (not (eq prev-line-type 'continue))
                     (not (eq prev-line-type 'statement-select)))
            ;; if prev is a begin block, then set indent to current
            ;; else set indent to current - tab
            (if (eq prev-line-type 'begin)
                (setq indent prev-line-indent)
              (setq indent (- prev-line-indent tab-width)))
            ;; do not check for if-else indent
            (setq check-if-else nil))

          ;; if prev is an if/else statement, then set accordingly
          (when (eq prev-line-type 'if-else)
            ;; if at begin block, then set indent to current
            ;; else set indent to current + tab
            (if (eq line-type 'begin)
                (setq indent prev-line-indent)
              (setq indent (+ prev-line-indent tab-width))))

          ;; if prev is a regular statement, comment, or end and indent is not set
          ;; then set accordingly
          (when (and (not indent)
                     (or
                      (sql-indent-line-is-statement prev-line-type)
                      (sql-indent-line-is-comment prev-line-type)
                      (eq prev-line-type 'end)))
            ;; set indent to current
            (setq indent prev-line-indent)
            ;; if prev is a statement that can be followed with a select,
            ;; and it is, then add a half tab to the indent
            (when (and
                   (eq prev-line-type 'statement-select)
                   (eq line-type 'statement)
                   (string= line-keyword "select"))
              ;; add half tab to indent
              (setq indent (+ indent (/ tab-width 2)))
              ;; do not check for if-else indent
              (setq check-if-else nil)))
          ;; else assume we are at a continuation statement and continue
          ))

      ;; check for if/else with no begin/end block
      (while (and (not (bobp))
                  (not (eq line-type 'continue))
                  (not (eq line-type 'statement-select))
                  (not (eq prev-line-type 'begin))
                  (not (eq prev-line-type 'end))
                  (not (eq prev-line-type 'if-else))
                  check-if-else)
        ;; look for an if/else two statements previous
        ;; prev-line is already at previous line, so go up one more
        ;; go up lines until we find something (or hit the top)

        ;; get previous line info
        (forward-line -1)
        (setq prev-line-info (sql-indent-line-get-info))
        (setq prev-line-type (car prev-line-info))
        (setq prev-line-keyword (car (cdr prev-line-info)))
        (setq prev-line-indent (car (cdr (cdr prev-line-info))))

        ;; if we are at the beginning of the buffer, then exit loop
        (when (eq prev-line-type 'bob)
          (setq check-if-else nil))

        ;; if at a begin block, then exit loop
        (when (eq prev-line-type 'begin)
          (setq check-if-else nil))

        ;; if at an end block, then exit loop
        (when (eq prev-line-type 'end)
          (setq check-if-else nil))

        ;; if at an if/else statement, then set indent to current, and exit loop
        (when (eq prev-line-type 'if-else)
          (setq indent (current-indentation))
          (setq check-if-else nil))

        ;; if at a statement, then exit loop
        (when (sql-indent-line-is-statement prev-line-type)
          (setq check-if-else nil))

        ;; otherwise we are at a comment, blank, or continue
        ;; loop and get next line up
        )

      ;; if at beginning of buffer and not indented, then do not indent
      (when (and (bobp) (not indent))
        (setq indent 0))

      ;; if at a continuation line, then add half a tab to the indent
      (when (eq line-type 'continue)
        (setq indent (+ indent (/ tab-width 2)))))

    ;; if we figured out the indentation, then set it accordingly
    ;; else, set it to 0
    (if (and indent (>= indent 0))
        (indent-line-to indent)
      (indent-line-to 0))))

(defun sql-indent-line-old ()
  "Indent current SQL line."
  (interactive)

  ;; upcase keywords
  (when sql-mode-keyword-upcase-p
    (sql-mode-keyword-upcase (point-at-bol) (point-at-eol)))

  ;; goto beginning of line
  (beginning-of-line)

  ;; if at beginning of buffer, set indent to 0
  (if (bobp)
      (indent-line-to 0)

    ;; else...
    ;; init variables
    (let ((not-indented t)
          (cur-indent nil)
          (cur-line-is-begin nil)
          (cur-line-is-end nil)
          (cur-line-is-statement nil)
          (cur-line-is-continuation nil)
          (cur-line-is-blank nil)
          (cur-line-is-comment nil)
          (cur-line-is-select nil)
          (prev-line-is-dml nil)
          (prev-line-is-insert nil)
          (prev-line-is-cursor nil)
          (check-if-else t)
          (in-comment nil))

      ;; determine if in the middle of a comment
      (save-excursion
        ;; if at end of comment and there is no begin of comment,
        ;; then go up one and continue
        (when (and
               (and (not (bobp))
                    (looking-at sql-indent-comment-end-regexp)
                    (not (looking-at sql-indent-comment-begin-regexp))))
          (forward-line -1))
        ;; go up lines until we find the top, a comment end, or a comment begin
        (while (and
                (and (not (bobp))
                     (not cur-line-is-comment))
                (not (looking-at sql-indent-comment-end-regexp)))
          (forward-line -1)
          ;; if we find a line with a comment begin and no end,
          ;; then we are in a comment block
          ;; set indent to current
          (when (and (looking-at sql-indent-comment-begin-regexp)
                     (not (looking-at sql-indent-comment-end-regexp)))
            (setq cur-line-is-comment t)
            (setq cur-indent (current-indentation))
            (setq not-indented nil))))

      ;; if not in a comment block then find line type
      (when not-indented
        (cond
         ;; if at blank line, then set flag
         ((looking-at sql-indent-blank-regexp)
          (setq cur-line-is-blank t))
         ;; if at begin statement, then set flag
         ((looking-at sql-indent-begin-regexp)
          (setq cur-line-is-begin t))
         ;; if at end statement, then set flag
         ((looking-at sql-indent-end-regexp)
          (setq cur-line-is-end t))
         ;; if at regular statement (or comment), then set flag
         ((or
           (looking-at sql-indent-statement-regexp)
           (looking-at sql-indent-comment-regexp))
          (setq cur-line-is-statement t)
          ;; if at a select statement, set flag
          (when (looking-at sql-indent-select-regexp)
            (setq cur-line-is-select t)))
         ;; else assume we are at a continuation statement and set flag
         ;; do not check for if-else indent
         (t (setq cur-line-is-continuation t)
            (setq check-if-else nil))))

      ;; find the previous statement
      ;; go up lines until we find something (or hit the top)
      (save-excursion
        (while (and (not (bobp)) not-indented)
          (forward-line -1)

          ;; if in comment block, then continue until at comment begin
          (while (and
                  (and in-comment
                       (not (bobp)))
                  (not (looking-at sql-indent-comment-begin-regexp)))
            (forward-line -1))
          (setq in-comment nil)

          ;; if in comment block, then set flag
          (if (and
               (looking-at sql-indent-comment-end-regexp)
               (not (looking-at sql-indent-comment-begin-regexp)))
              (setq in-comment t)
            ;; if at blank line, then continue
            (unless (looking-at sql-indent-blank-regexp)
              ;; if at begin statement, then set indent accordingly
              (if (looking-at sql-indent-begin-regexp)
                  (progn
                    ;; if at end block, then set indent to current
                    ;; else set indent to current + tab
                    (if cur-line-is-end
                        (setq cur-indent (current-indentation))
                      (setq cur-indent (+ (current-indentation) tab-width)))
                    (setq not-indented nil)
                    ;; do not check for if-else indent
                    (setq check-if-else nil))
                ;; if at end statement, then set indent accordingly
                (if (looking-at sql-indent-end-regexp)
                    (progn
                      ;; if at end block, then set indent to current - tab
                      ;; else set indent to current
                      (if cur-line-is-end
                          (setq cur-indent (- (current-indentation) tab-width))
                        (setq cur-indent (current-indentation)))
                      (setq not-indented nil))
                  ;; if at if/else statement, then set accordingly
                  (if (looking-at sql-indent-if-else-regexp)
                      (progn
                        ;; if at begin block, then set indent to current
                        ;; else set indent to current + tab
                        (if cur-line-is-begin
                            (setq cur-indent (current-indentation))
                          (setq cur-indent (+ (current-indentation) tab-width)))
                        (setq not-indented nil))
                    ;; if at regular statement (or comment), then set accordingly
                    (if (or
                         (looking-at sql-indent-statement-regexp)
                         (looking-at sql-indent-comment-regexp))
                        (progn
                          ;; if at end block, then set indent to current - tab
                          ;; else set indent to current
                          (if cur-line-is-end
                              (setq cur-indent (- (current-indentation) tab-width))
                            (setq cur-indent (current-indentation)))
                          ;; if at a dml statement, set flag
                          (when (looking-at sql-indent-dml-regexp)
                            (setq prev-line-is-dml t))
                          ;; if at an insert with no values, set flag
                          (when (and
                                 (looking-at sql-indent-insert-regexp)
                                 (not (looking-at sql-indent-insert-values-regexp)))
                            (setq prev-line-is-insert t))
                          ;; if at a declare cursor, set flag
                          (when (looking-at sql-indent-cursor-regexp)
                            (setq prev-line-is-cursor t))
                          (setq not-indented nil))
                      ;; else assume we are at a continuation statement and continue
                      ))))))

          ;; if at beginning of buffer and not indented, then do not indent
          (when (and (bobp) not-indented)
            (setq cur-indent 0)
            (setq not-indented nil)))

        ;; now check for if/else with no begin/end block
        ;; find the previous statement
        ;; go up lines until we find something
        ;; skip if we are on a continuation line
        (when check-if-else
          (setq not-indented t)
          (save-excursion
            (while (and not-indented (not (bobp)))
              (forward-line -1)
              ;; if at blank line, then continue
              (unless (looking-at sql-indent-blank-regexp)
                ;; if at begin block, then leave indent alone
                (if (looking-at sql-indent-begin-regexp)
                    (setq not-indented nil)
                  ;; if at end block, then leave indent alone
                  (if (looking-at sql-indent-end-regexp)
                      (setq not-indented nil)
                    ;; if at if-else statement, then set indent to current
                    (if (looking-at sql-indent-if-else-regexp)
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                      ;; if at regular statement, then leave indent alone
                      (if (looking-at sql-indent-statement-regexp)
                          (setq not-indented nil)
                        ;; else assume we are at a continuation statement
                        ;; and continue
                        ())))))))))

      ;; certain dml statements are really continuation lines
      ;; check for insert/select
      (when (and cur-line-is-select prev-line-is-insert)
        (setq cur-line-is-continuation t))
      ;; check for cursor/select
      (when (and cur-line-is-select prev-line-is-cursor)
        (setq cur-line-is-continuation t))

      ;; if at a continuation line, then add half a tab to the indent
      (when (and cur-indent cur-line-is-continuation)
        (setq cur-indent (+ cur-indent (/ tab-width 2))))

      ;; if in comment block, set indent to + 3,
      ;; unless line starts with *, then set to + 1
      (when cur-line-is-comment
        (setq cur-indent (+ cur-indent 3))
        (when (looking-at sql-indent-asterisk-regexp)
          (setq cur-indent (- cur-indent 2))))

      ;; if we figured out the indentation, then set it accordingly
      ;; else, set it to 0
      (if cur-indent
          (if (< cur-indent 0)
              (indent-line-to 0)
            (indent-line-to cur-indent))
        (indent-line-to 0)))))

;;; SQL Mode Hook

;;;###autoload
(defun indent-newline-and-indent ()
  "Indent current line, then add a newline at the end, then indent the new line."
  (interactive)
  (save-excursion
    (indent-for-tab-command))
  (newline-and-indent))

(defun local-sql-mode-hook ()
  ;; setup syntax table
  (set-syntax-table sql-mode-syntax-table)

  ;; setup indenting
  (set (make-local-variable 'indent-line-function) 'sql-indent-line)

  ;; set key bindings
  ;; set return to `indent-newline-and-indent', so the
  ;; current line gets reevaluated after it is entered
  (local-set-key (kbd "<return>") 'indent-newline-and-indent)
  (local-set-key (kbd "C-m") 'indent-newline-and-indent))

(add-hook 'sql-mode-hook 'local-sql-mode-hook)

(provide 'tsql)

;;; tsql.el ends here
