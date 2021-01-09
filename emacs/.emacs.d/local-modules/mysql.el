;;; mysql.el --- customizes sql.el for MySQL
;;
;;; Copyright (C) 2005 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2005-01-01
;; Version:  1.0
;; Keywords: SQL MySQL mode
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
;; mysql adds proper font faces and intenting to sql buffers.
;;
;;; Installation:
;;
;; This library should be loaded once SQL mode has finished loading.  One way
;; to accomplish this is to add the following to your .emacs file:
;;
;; (eval-after-load "sql"
;;   '(load-library "mysql"))
;;
;; Once the library is loaded, all SQL mode buffers from that point forward
;; will use this indentation function.
;;
;;; Inspiration:
;;
;; The inspiration for this code came from "sql-indent" by Alex Schroeder
;; (http://www.emacswiki.org/cgi-bin/wiki.pl?SqlIndent).  Also, an excellent
;; tutorial on indenation in "An Emacs language mode creation tutorial"
;; (http://two-wugs.net/emacs/mode-tutorial.html).

;;; Code:

(require 'sql)

;;; Custom

;;(defcustom mysql-tab
;;  "4"
;;  "SQL indentation size."
;;  :group 'SQL)

;;; Font-Lock

;; ;; original:
;; (defvar sql-mode-mysql-font-lock-keywords
;;   (let ((mysql-funcs (sql-keywords-re
;; "ascii" "avg" "bdmpolyfromtext" "bdmpolyfromwkb" "bdpolyfromtext"
;; "bdpolyfromwkb" "benchmark" "bin" "bit_and" "bit_length" "bit_or"
;; "bit_xor" "both" "cast" "char_length" "character_length" "coalesce"
;; "concat" "concat_ws" "connection_id" "conv" "convert" "count"
;; "curdate" "current_date" "current_time" "current_timestamp" "curtime"
;; "elt" "encrypt" "export_set" "field" "find_in_set" "found_rows" "from"
;; "geomcollfromtext" "geomcollfromwkb" "geometrycollectionfromtext"
;; "geometrycollectionfromwkb" "geometryfromtext" "geometryfromwkb"
;; "geomfromtext" "geomfromwkb" "get_lock" "group_concat" "hex" "ifnull"
;; "instr" "interval" "isnull" "last_insert_id" "lcase" "leading"
;; "length" "linefromtext" "linefromwkb" "linestringfromtext"
;; "linestringfromwkb" "load_file" "locate" "lower" "lpad" "ltrim"
;; "make_set" "master_pos_wait" "max" "mid" "min" "mlinefromtext"
;; "mlinefromwkb" "mpointfromtext" "mpointfromwkb" "mpolyfromtext"
;; "mpolyfromwkb" "multilinestringfromtext" "multilinestringfromwkb"
;; "multipointfromtext" "multipointfromwkb" "multipolygonfromtext"
;; "multipolygonfromwkb" "now" "nullif" "oct" "octet_length" "ord"
;; "pointfromtext" "pointfromwkb" "polyfromtext" "polyfromwkb"
;; "polygonfromtext" "polygonfromwkb" "position" "quote" "rand"
;; "release_lock" "repeat" "replace" "reverse" "rpad" "rtrim" "soundex"
;; "space" "std" "stddev" "substring" "substring_index" "sum" "sysdate"
;; "trailing" "trim" "ucase" "unix_timestamp" "upper" "user" "variance"
;; ))

;;  (mysql-keywords (sql-keywords-re
;; "action" "add" "after" "against" "all" "alter" "and" "as" "asc"
;; "auto_increment" "avg_row_length" "bdb" "between" "by" "cascade"
;; "case" "change" "character" "check" "checksum" "close" "collate"
;; "collation" "column" "columns" "comment" "committed" "concurrent"
;; "constraint" "create" "cross" "data" "database" "default"
;; "delay_key_write" "delayed" "delete" "desc" "directory" "disable"
;; "distinct" "distinctrow" "do" "drop" "dumpfile" "duplicate" "else"
;; "enable" "enclosed" "end" "escaped" "exists" "fields" "first" "for"
;; "force" "foreign" "from" "full" "fulltext" "global" "group" "handler"
;; "having" "heap" "high_priority" "if" "ignore" "in" "index" "infile"
;; "inner" "insert" "insert_method" "into" "is" "isam" "isolation" "join"
;; "key" "keys" "last" "left" "level" "like" "limit" "lines" "load"
;; "local" "lock" "low_priority" "match" "max_rows" "merge" "min_rows"
;; "mode" "modify" "mrg_myisam" "myisam" "natural" "next" "no" "not"
;; "null" "offset" "oj" "on" "open" "optionally" "or" "order" "outer"
;; "outfile" "pack_keys" "partial" "password" "prev" "primary"
;; "procedure" "quick" "raid0" "raid_type" "read" "references" "rename"
;; "repeatable" "restrict" "right" "rollback" "rollup" "row_format"
;; "savepoint" "select" "separator" "serializable" "session" "set"
;; "share" "show" "sql_big_result" "sql_buffer_result" "sql_cache"
;; "sql_calc_found_rows" "sql_no_cache" "sql_small_result" "starting"
;; "straight_join" "striped" "table" "tables" "temporary" "terminated"
;; "then" "to" "transaction" "truncate" "type" "uncommitted" "union"
;; "unique" "unlock" "update" "use" "using" "values" "when" "where"
;; "with" "write" "xor"
;; ))

;;  (mysql-types (sql-keywords-re
;; "bigint" "binary" "bit" "blob" "bool" "boolean" "char" "curve" "date"
;; "datetime" "dec" "decimal" "double" "enum" "fixed" "float" "geometry"
;; "geometrycollection" "int" "integer" "line" "linearring" "linestring"
;; "longblob" "longtext" "mediumblob" "mediumint" "mediumtext"
;; "multicurve" "multilinestring" "multipoint" "multipolygon"
;; "multisurface" "national" "numeric" "point" "polygon" "precision"
;; "real" "smallint" "surface" "text" "time" "timestamp" "tinyblob"
;; "tinyint" "tinytext" "unsigned" "varchar" "year" "year2" "year4"
;; "zerofill"
;; )))

;;     `((,mysql-funcs    . font-lock-builtin-face)
;;       (,mysql-keywords . font-lock-keyword-face)
;;       (,mysql-types    . font-lock-type-face)))

;;   "MySQL SQL keywords used by font-lock.

;; This variable is used by `sql-mode' and `sql-interactive-mode'.  The
;; regular expressions are created during compilation by calling the
;; function `regexp-opt'.  Therefore, take a look at the source before
;; you define your own sql-mode-mysql-font-lock-keywords.")

(defvar sql-mode-mysql-font-lock-keywords nil
  "MySQL keywords used by font-lock.")

;; all mysql reserved words taken from the table found here:
;; http://dev.mysql.com/doc/refman/5.0/en/reserved-words.html
;; "add" "all" "alter" "analyze" "and" "as" "asc" "asensitive" "before"
;; "between" "bigint" "binary" "blob" "both" "by" "call" "cascade" "case"
;; "change" "char" "character" "check" "collate" "column" "condition"
;; "connection" "constraint" "continue" "convert" "create" "cross"
;; "current_date" "current_time" "current_timestamp" "current_user" "cursor"
;; "database" "databases" "day_hour" "day_microsecond" "day_minute"
;; "day_second" "dec" "decimal" "declare" "default" "delayed" "delete" "desc"
;; "describe" "deterministic" "distinct" "distinctrow" "div" "double" "drop"
;; "dual" "each" "else" "elseif" "enclosed" "escaped" "exists" "exit"
;; "explain" "false" "fetch" "float" "float4" "float8" "for" "force" "foreign"
;; "from" "fulltext" "goto" "grant" "group" "having" "high_priority"
;; "hour_microsecond" "hour_minute" "hour_second" "if" "ignore" "in" "index"
;; "infile" "inner" "inout" "insensitive" "insert" "int" "int1" "int2" "int3"
;; "int4" "int8" "integer" "interval" "into" "is" "iterate" "join" "key"
;; "keys" "kill" "label" "leading" "leave" "leave" "left" "like" "limit"
;; "lines" "load" "localtime" "localtimestamp" "lock" "long" "longblob"
;; "longtext" "loop" "low_priority" "match" "mediumblob" "mediumint"
;; "mediumtext" "middleint" "minute_microsecond" "minute_second" "mod"
;; "modifies" "natural" "no_write_to_binlog" "not" "null" "numeric" "on"
;; "optimize" "option" "optionally" "or" "order" "out" "outer" "outfile"
;; "precision" "primary" "procedure" "purge" "raid0" "read" "reads" "real"
;; "references" "regexp" "release" "rename" "repeat" "replace" "require"
;; "restrict" "return" "revoke" "right" "rlike" "schema" "schemas"
;; "second_microsecond" "select" "sensitive" "separator" "set" "show"
;; "smallint" "soname" "spatial" "specific" "sql" "sql_big_result"
;; "sql_calc_found_rows" "sql_small_result" "sqlexception" "sqlstate"
;; "sqlwarning" "ssl" "starting" "straight_join" "table" "terminated" "then"
;; "tinyblob" "tinyint" "tinytext" "to" "trailing" "trigger" "true" "undo"
;; "union" "unique" "unlock" "unsigned" "update" "upgrade" "usage" "use"
;; "using" "utc_date" "utc_time" "utc_timestamp" "values" "varbinary"
;; "varchar" "varcharacter" "varying" "when" "where" "while" "with" "write"
;; "x509" "xor" "year_month" "zerofill"

(eval-when-compile
  (setq sql-mode-mysql-font-lock-keywords
        (list
         (sql-font-lock-keywords-builder 'font-lock-type-face nil
"ada" "asensitive" "assignment" "asymmetric" "atomic" "between"
"bigint" "binary" "bit" "blob" "char" "character" "date" "datetime" "dec"
"decimal" "double" "enum" "float" "float4" "float8" "int" "int1" "int2" "int3"
"int4" "int8" "integer" "long" "longblob" "longtext" "mediumblob" "mediumint"
"mediumtext" "numeric" "precision" "real" "smallint" "time" "timestamp"
"tinyblob" "tinyint" "tinytext" "unsigned" "varbinary" "varchar"
"varcharacter" "year" "zerofill"
)
         (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"all" "alter" "and" "as" "asc" "between" "by" "check" "create" "cross"
"declare" "default" "delete" "desc" "distinct" "drop" "exists" "for" "from"
"grant" "group" "having" "in" "inner" "insert" "into" "is" "join" "left"
"like" "not" "null" "on" "option" "or" "order" "outer" "right" "select" "set"
"table" "to" "top" "union" "unique" "update" "values" "view" "where" "with"
)
         (sql-font-lock-keywords-builder 'font-lock-function-name-face nil
"add" "after" "analyze" "asensitive" "before" "both" "call" "cascade" "btree"
"change" "collate" "column" "condition" "connection" "constraint" "continue"
"convert" "cursor" "database" "databases" "delayed" "describe" "deterministic"
"distinctrow" "div" "dual" "each" "else" "elseif" "enclosed" "escaped" "exit"
"explain" "false" "fetch" "force" "foreign" "fulltext" "goto" "high_priority"
"if" "ignore" "index" "infile" "inout" "insensitive" "interval" "iterate"
"key" "keys" "kill" "label" "leading" "leave" "leave" "limit" "lines" "load"
"lock" "loop" "match" "mod" "modifies" "natural" "optimize" "optionally" "out"
"outfile" "primary" "procedure" "purge" "raid0" "read" "reads" "real"
"references" "regexp" "release" "rename" "repeat" "replace" "require"
"restrict" "return" "revoke" "rlike" "schema" "schemas" "sensitive"
"separator" "show" "soname" "spatial" "specific" "sql" "sqlexception"
"sqlstate" "sqlwarning" "ssl" "starting" "straight_join" "terminated" "then"
"trailing" "trigger" "true" "undo" "unlock" "unsigned" "upgrade" "usage" "use"
"using" "varying" "when" "while" "write" "x509" "xor"
)
         (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"case" "current_date" "current_time" "current_timestamp" "current_user"
"day_hour" "day_microsecond" "day_minute" "day_second" "hour_microsecond"
"hour_minute" "hour_second" "localtime" "localtimestamp" "low_priority"
"minute_microsecond" "minute_second" "no_write_to_binlog" "second_microsecond"
"sql_big_result" "sql_calc_found_rows" "sql_small_result" "utc_date"
"utc_time" "utc_timestamp" "year_month"
))))

;;;###autoload
(defun sql-highlight-mysql-keywords ()
  "Highlight MySQL keywords.

Set `font-lock-keywords' appropriately."
  (interactive)
  (setq font-lock-keywords sql-mode-mysql-font-lock-keywords)
  (font-lock-fontify-buffer))

;;; SQL Menu

;; (easy-menu-define
;;   sql-mode-menu sql-mode-map
;;   "Menu for `sql-mode'."
;;   '("SQL"
;;     ["Send Paragraph" sql-send-paragraph (and (buffer-live-p sql-buffer)
;;                                               (get-buffer-process sql-buffer))]
;;     ["Send Region" sql-send-region (and (or (and (boundp 'mark-active) ; Emacs
;;                                                  mark-active)
;;                                             (mark)) ; XEmacs
;;                                         (buffer-live-p sql-buffer)
;;                                         (get-buffer-process sql-buffer))]
;;     ["Send Buffer" sql-send-buffer (and (buffer-live-p sql-buffer)
;;                                         (get-buffer-process sql-buffer))]
;;     ["Show SQLi buffer" sql-show-sqli-buffer t]
;;     ["Set SQLi buffer" sql-set-sqli-buffer t]
;;     ["Pop to SQLi buffer after send"
;;      sql-toggle-pop-to-buffer-after-send-region
;;      :style toggle
;;      :selected sql-pop-to-buffer-after-send-region]
;;     ("Highlighting"
;;      ["MySQL keywords" sql-highlight-mysql-keywords t]
;;      ["T-SQL keywords" sql-highlight-tsql-keywords t]
;;      ["ANSI SQL keywords" sql-highlight-ansi-keywords t]
;;      ["Oracle keywords" sql-highlight-oracle-keywords t]
;;      ["Postgres keywords" sql-highlight-postgres-keywords t])))

(easy-menu-add-item sql-mode-map '("menu-bar" "SQL")
                    '("Highlighting"
                      ["MySQL keywords" sql-highlight-mysql-keywords t]
                      ["T-SQL keywords" sql-highlight-tsql-keywords t]
                      ["ANSI SQL keywords" sql-highlight-ansi-keywords t]
                      ["Oracle keywords" sql-highlight-oracle-keywords t]
                      ["Postgres keywords" sql-highlight-postgres-keywords t]))

;;(define-key sql-mode-menu [Highlighting]
;;  (append '(menu-item "MySQL keyword" (setq sql-highlight-mysql-keywords t))))

;;(setq sql-highlight-mysql-keywords t)
(setq sql-mode-font-lock-keywords sql-mode-mysql-font-lock-keywords)

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
  (list 'font-lock-type-face
        'font-lock-keyword-face
        'font-lock-function-name-face
        'font-lock-builtin-face)
  "Font-lock faces that are considered keywords.
\nThese will be uppercased if `sql-mode-keyword-upcase-p' is
true.")

(defconst sql-mode-keyword-space-regexp
  ;;"[\\t ]"
  "\\s-"
  "Regular expression that matches a single white-space character.")

;;;###autoload
(defun sql-mode-keyword-upcase (beg end)
  "Upcase SQL keywords in range."
  (interactive "*r")
  (save-excursion
    (let (face
          face-list)
      ;; goto start of range
      (goto-char beg)
      ;; loop from start to end of range
      (while (< (point) end)
        ;; find next word
        (while (and
                (< (point) end)
                (looking-at sql-mode-keyword-space-regexp))
          (forward-char))
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
                  ;; uppercase the word (without moving point)
                  (save-excursion
                    (upcase-word 1))
                  ;; clear the list to end the while loop
                  (setq face-list ()))
              ;; else cdr the list
              (setq face-list (cdr face-list)))))
        ;; move to next word and continue
        (forward-word 1)))))

;;; Indenting

(defconst sql-indent-blank-regexp
  ;;"^[\t ]*$"
  "^\\s-*$"
  "Regular expression that matches a blank line.")

(defconst sql-indent-begin-regexp
  ;;"^[\t ]*\\bbegin\\b"
  "^\\s-*\\bbegin\\b"
  "Regular expression that matches a begin block.")

(defconst sql-indent-end-regexp
  ;;"^[\t ]*\\b\\(end[\t ]*$\\|commit tran\\)\\b"
  "^\\s-*\\b\\(end\\s-*$\\|commit tran\\)\\b"
  "Regular expression that matches an end block.
\nDoes not match the end of an inline case statement.")

(defconst sql-indent-if-else-regexp
  ;;"^[\t ]*\\b\\(if\\|else\\)\\b"
  "^\\s-*\\b\\(if\\|else\\)\\b"
  "Regular expression that matches an if or else statement.")

(defconst sql-indent-comment-regexp
  ;;"^[\t ]*\\(\\-\\-\\|\\/\\*\\)"
  "^\\s-*\\(\\-\\-\\|\\/\\*\\)"
  "Regular expression that matches a comment begin or line.")

(defconst sql-indent-comment-line-regexp
  ;;"^[\t ]*\\-\\-"
  "^\\s-*\\-\\-"
  "Regular expression that matches a comment line.")

(defconst sql-indent-comment-begin-regexp
  "^.*\\/\\*"
  "Regular expression that matches a comment begin.")

(defconst sql-indent-comment-end-regexp
  "^.*\\*\\/"
  "Regular expression that matches a comment end.")

(defconst sql-indent-asterisk-regexp
  ;;"^[\t ]*\\*"
  "^\\s-*\\*"
  "Regular expression that matches a line starting with `*'.")

(defconst sql-indent-statement-regexp
  (eval-when-compile
    ;;(concat "^[\t ]*\\b"
    (concat "^\\s-*\\b"
            (regexp-opt '(
"add" "alter" "as" "authorization" "backup" "break" "browse" "bulk" "cascade"
"checkpoint" "close" "clustered" "coalesce" "collate" "column" "commit"
"compute" "constraint" "contains" "containstable" "continue" "convert"
"create" "cross" "current_date" "current_time" "current_timestamp"
"current_user" "cursor" "database" "dbcc" "deallocate" "declare" "delete"
"deny" "disk" "distributed" "drop" "dummy" "dump" "else" "errlvl" "escape"
"except" "exec" "execute" "exit" "fetch" "file" "for" "freetext"
"freetexttable" "full" "go" "goto" "grant" "holdlock" "identity_insert"
"identitycol" "if" "insert" "intersect" "key" "kill" "lineno" "load"
"national" "offsets" "open" "opendatasource" "openquery" "openrowset"
"openxml" "over" "plan" "print" "public" "raiserror" "read" "reconfigure"
"references" "replication" "restore" "restrict" "return" "revoke" "rollback"
"rowguidcol" "rule" "save" "schema" "select" "session_user" "set" "setuser"
"shutdown" "some" "statistics" "system_user" "textsize" "truncate" "tsequal"
"union" "update" "use" "varying" "waitfor" "while"
) t) "\\b"))
  "Regular expression that matches the beginning of SQL statements.")

(defconst sql-indent-dml-regexp
  (eval-when-compile
    ;;(concat "^[\t ]*\\b"
    (concat "^\\s-*\\b"
            (regexp-opt '("delete" "insert" "select" "update") t) "\\b"))
  "Regular expression that matches the beginning of DML SQL statements.
\nOnes that start with `select', `insert', `update', or `delete'.")

(defconst sql-indent-select-regexp
  ;;"^[\t ]*\\bselect\\b"
  "^\\s-*\\bselect\\b"
  "Regular expression that matches the beginning of a select statement.")

(defconst sql-indent-insert-regexp
  ;;"^[\t ]*\\binsert\\b"
  "^\\s-*\\binsert\\b"
  "Regular expression that matches an insert statement.")

(defconst sql-indent-insert-values-regexp
  ;;"^[\t ]*\\binsert\\b.*\\bvalues\\b"
  "^\\s-*\\binsert\\b.*\\bvalues\\b"
  "Regular expression that matches an insert/values statement.")

(defconst sql-indent-cursor-regexp
  ;;"^[\t ]*\\bdeclare\\b.*\\bcursor\\b[\t ]*\\bfor\\b[\t ]*$"
  "^\\s-*\\bdeclare\\b.*\\bcursor\\b\\s-*\\bfor\\b\\s-*$"
  "Regular expression that matches a declare cursor statement.")

;;;###autoload
(defun sql-indent-line-get-info ()
  "Get info about statement on current line.

Returns a list containing the following:
  type:     type of SQL statement
  keyword:  starting SQL keyword (lowercased)
  indent:   column number of indentation

Moves point to start of statement.  If in a comment block, will
move point to the start of the comment block.

You may call it again after doing `forward-word -1' to get info
on the previous statement.

Possible types are:
  bob:                   beginning of block
  comment-line:          -- type of comment
  comment-block-begin:   /* */ type of comment (first line)
  comment-block-end:     /* */ type of comment (last line)
  comment-block-middle:  /* */ type of comment (a middle line)
  blank:                 blank line
  begin:                 begin statement (block begin)
  end:                   end statement (block end)
  if-else:               if or else statement
  comment:               (should never happend; should get a more specific type, above)
  statement:             other sql statement
  statement-select:      an sql statement that may be followed by a select
  continue:              continuation of an sql statement"
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
                ;; if we see a comment block begin, then we were in a
                ;; comment block
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
                      (eq (get-char-property (point) 'face)
                          'font-lock-comment-face))
            (forward-line -1)
            (beginning-of-line)
            (unless (bobp)
              (goto-char (+ (point-at-bol) (current-indentation)))))
          ;; move down one unless at top of block and in comment block
          (unless (and (bobp)
                       (eq (get-char-property (point) 'face)
                           'font-lock-comment-face))
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
            ;; look for special statements that may be
            ;; followed by select if at an insert with no
            ;; values, set type
            (when (and
                   (looking-at sql-indent-insert-regexp)
                   (not (looking-at sql-indent-insert-values-regexp)))
              (setq type 'statement-select))
            ;; if at a declare cursor, set type
            (when (looking-at sql-indent-cursor-regexp)
              (setq type 'statement-select)))
           ;; else assume we are at a continuation statement;
           ;; set type
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

  ;; upcase keywords
  (when sql-mode-keyword-upcase-p
      (sql-mode-keyword-upcase (point-at-bol) (point-at-eol)))

  ;; init variables
  (let (line-info
        line-type
        line-keyword
        line-indent
        prev-line-info
        prev-line-type
        prev-line-keyword
        prev-line-indent
        first-prev-line-indent
        indent
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

      ;; if we are in a comment block, set indent to +3, unless line starts
      ;; with *, then set to + 1
      (when (or (eq line-type 'comment-block-middle)
                (eq line-type 'comment-block-end))
        (setq indent (+ line-indent 3))
        (when (looking-at sql-indent-asterisk-regexp)
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
          ;; store first previous line indent
          (unless first-prev-line-indent
            (setq first-prev-line-indent prev-line-indent))

          ;; if we are at the beginning of the buffer, set indent
          (when (eq prev-line-type 'bob)
            (setq indent prev-line-indent))

          ;; if prev is a begin statement, then set indent accordingly
          (when (eq prev-line-type 'begin)
            (setq indent (+ prev-line-indent tab-width))
            ;; do not check for if-else indent
            (setq check-if-else nil))

          ;; if at end statement, then set indent accordingly; loop until we
          ;; get the real current indent
          (when (and (eq line-type 'end)
                     (not (eq prev-line-type 'continue))
                     (not (eq prev-line-type 'statement-select)))
            ;; if prev is a begin block, then set indent to current;
            ;; else set indent to current - tab
            (if (eq prev-line-type 'begin)
                (setq indent prev-line-indent)
              (setq indent (- prev-line-indent tab-width)))
            ;; do not check for if-else indent
            (setq check-if-else nil))

          ;; if prev is an if/else statement, then set accordingly
          (when (eq prev-line-type 'if-else)
            ;; if at begin block, then set indent to current; else set
            ;; indent to current + tab
            (if (eq line-type 'begin)
                (setq indent prev-line-indent)
              (setq indent (+ prev-line-indent tab-width))))

          ;; if prev is a regular statement, comment, or end and indent is
          ;; not set then set accordingly
          (when (and (not indent)
                     (or
                      (sql-indent-line-is-statement prev-line-type)
                      (sql-indent-line-is-comment prev-line-type)
                      (eq prev-line-type 'end)))
            ;; set indent to current
            (setq indent prev-line-indent)
            ;; if prev is a statement that can be followed with a
            ;; select, and it is, then add a half tab to the indent
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
        ;; look for an if/else two statements previous; prev-line is already
        ;; at previous line, so go up one more; go up lines until we find
        ;; something (or hit the top)

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

        ;; if at an if/else statement, then set indent to current, and exit
        ;; loop
        (when (eq prev-line-type 'if-else)
          (setq indent (current-indentation))
          (setq check-if-else nil))

        ;; if at a statement, then exit loop
        (when (sql-indent-line-is-statement prev-line-type)
          (setq check-if-else nil))

        ;; otherwise we are at a comment, blank, or continue loop and get next
        ;; line up
        )

      ;; if at beginning of buffer and not indented, then do not indent
      (when (and (bobp) (not indent))
        (setq indent 0))

      ;; if at a continuation line, then add half a tab to the indent
      (when (eq line-type 'continue)
        (setq indent (+ indent (/ tab-width 2))))

      ;; if at a blank line, then use the first previous line indent
      (when (eq line-type 'blank)
        (setq indent first-prev-line-indent)))

    ;; if we figured out the indentation, then set it accordingly; else set it
    ;; to 0
    (if (and indent (>= indent 0))
        (indent-line-to indent)
      (indent-line-to 0))))

;;; SQL Mode Hook

;;;###autoload
(defun indent-newline-and-indent ()
  "Indent current line, then add a newline at the end, then indent the new line."
  (interactive)
  (save-excursion
    (indent-for-tab-command))
  (newline-and-indent))

(defun mysql-mode-hook ()
  ;; setup syntax table
  (set-syntax-table sql-mode-syntax-table)

  ;; setup font locking
  (sql-add-product-keywords 'mysql sql-mode-mysql-font-lock-keywords 'set)

  ;; setup indenting
  (set (make-local-variable 'indent-line-function) 'sql-indent-line)

  ;; set key bindings
  ;; set return to `indent-newline-and-indent', so the current line
  ;; gets reevaluated after it is entered
  (local-set-key (kbd "<return>") 'indent-newline-and-indent)
  (local-set-key (kbd "C-m") 'indent-newline-and-indent))

(add-hook 'sql-mode-hook 'mysql-mode-hook)

(provide 'mysql)

;;; mysql.el ends here
