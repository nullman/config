;;; bitwarden-test.el --- Test Suite for bitwarden.el
;;
;;; Copyright (C) 2008-2014 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2008-03-29
;; Version:  0.1
;; Keywords: bitwarden mode password test
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
;; Tests `bitwarden.el' functionality.
;;
;;; Installation:
;;
;; Load `bitwarden-test.el':
;;
;;   (load "bitwarden-test.el")
;;
;;; Usage:
;;
;; Run tests:
;;
;;   (eval-buffer)

;;; Code:

;; bitwarden
(require 'bitwarden)

;; assert
(defun bitwarden-test (program output &rest input)
  "Run PROGRAM through the BITWARDEN compiler and compare its output to OUTPUT.
\nAny optional INPUT parameters will be used to fill input prompts."
  (let ((bitwarden-debug-log t))
    (save-excursion
      (with-temp-buffer
        (insert program)
        (bitwarden-run-test input))
      (set-buffer "*bitwarden-output*")
      (let ((buffer-str (buffer-substring-no-properties (point-min) (point-max))))
        (when (> (length buffer-str) (length output))
          (setq output (concat output "\n")))
        (assert (string= buffer-str output)))
      )))
    ;; (switch-to-buffer "*bitwarden-debug*")
    ;; (goto-char (point-min))))

;;; Unit Tests: Statements

;; remarks
(bitwarden-test
 "
10 REM Remark Test
20 REM `1234567890-=
30 REM ~!@#$%^&*()_+
40 REM []\\;',./
50 REM {}|:\"<>?
"
 "")

;; math
(bitwarden-test
 "
10 REM BITWARDEN Test: Math
20 PRINT \"BITWARDEN Test: Math\": PRINT
30 A = -1 * 10 + 6 * 10
40 PRINT A
50 A = 10 * -1 + 6 * 10
60 PRINT A
70 A = -5 + 6 * 20 / 2 - 5
80 PRINT A
90 A = ((-1 + 6) * 20) / (5 - 3)
100 PRINT A
"
 "BITWARDEN Test: Math

50
50
50
50
")

;; string concatenation
(bitwarden-test
 "
10 REM BITWARDEN Test: Strings
20 PRINT \"BITWARDEN Test: Strings\": PRINT
30 S$ = \"1\"
40 PRINT S$
50 T$ = \"2\"
60 PRINT S$ + T$
70 S$ = S$ + T$ + \"3\"
80 PRINT S$
"
 "BITWARDEN Test: Strings

1
12
123
")

;; undeclared variables
(bitwarden-test
 "
10 REM BITWARDEN Test: Undeclared Vars
20 PRINT \"BITWARDEN Test: Undeclared Vars\": PRINT
30 A = A + 1
40 PRINT A
50 S$ = S$ + \"1\"
60 PRINT S$
"
 "BITWARDEN Test: Undeclared Vars

1
1
")

;; data/read/restore
(bitwarden-test
 "
10 REM BITWARDEN Test: DATA/READ/RESTORE
20 PRINT \"BITWARDEN Test: DATA/READ/RESTORE\": PRINT
30 DATA 1, 2, 3
40 FOR I = 1 TO 3
50   READ A
60   PRINT \"#\" I \" = \" A
70 NEXT
80 FOR I = 4 TO 6
90    READ A
100   PRINT \"#\" I \" = \" A
110 NEXT
120 RESTORE
130 FOR I = 1 TO 6
140   READ A
150   PRINT \"#\" I \" = \" A
160 NEXT
170 DATA 4, 5, 6
"
 "BITWARDEN Test: DATA/READ/RESTORE

#1 = 1
#2 = 2
#3 = 3
#4 = 4
#5 = 5
#6 = 6
#1 = 1
#2 = 2
#3 = 3
#4 = 4
#5 = 5
#6 = 6
")

;; def
(bitwarden-test
 "
10 REM BITWARDEN Test: DEF
20 PRINT \"BITWARDEN Test: DEF\": PRINT
30 DEF FNA(X)=X+1
40 PRINT STR$(FNA(1)) + \" + \" + STR$(FNA(2)) + \" = \" + STR$(FNA(1)+FNA(2))
50 DEF FNB(Z)=2*Z+1
60 FOR I = 1 TO 5
70   Z=INT(10+FNB(I)-.5*I)
80 PRINT Z;
90 NEXT I
100 PRINT
"
 "BITWARDEN Test: DEF

2 + 3 = 5
1214151718
")

;; dim
(bitwarden-test
 "
10 REM BITWARDEN Test: DIM
20 PRINT \"BITWARDEN Test: DIM\": PRINT
30 DIM A(10), B(10,10), C(10,10,10), S$(10), T$(10,10)
100 FOR I = 1 TO 10
110   A(I) = I
120 NEXT
130 FOR I = 6 TO 10
140   PRINT A(I-5)
150 NEXT
160 A = 100
170 PRINT A
180 PRINT A(10)
200 FOR I = 1 TO 10
210   S$(I) = CHR$(64+I)
220 NEXT
230 FOR I = 6 TO 10
240   PRINT S$(I-5)
250 NEXT
260 S$ = \"Z\"
270 PRINT S$
280 PRINT S$(10)
"
 "BITWARDEN Test: DIM

1
2
3
4
5
100
10
A
B
C
D
E
Z
J
")

;; end
(bitwarden-test
 "
10 REM BITWARDEN Test: END
20 PRINT \"BITWARDEN Test: END\": PRINT
30 END
40 PRINT \"Error\"
"
 "BITWARDEN Test: END

")

;; for/next
(bitwarden-test
 "
10 REM BITWARDEN Test: FOR/NEXT
20 PRINT \"BITWARDEN Test: FOR/NEXT\": PRINT
30 LET C = 0
40 FOR I = 1 TO 100
50   C = C + I
60 NEXT
70 PRINT C
80 FOR I = 0 TO -50 STEP -1
90   C = C + I
100 NEXT I
110 PRINT C
"
 "BITWARDEN Test: FOR/NEXT

5050
3775
")

;; goto
(bitwarden-test
 "
10 REM BITWARDEN Test: GOTO
20 PRINT \"BITWARDEN Test: GOTO\": PRINT
20 GOTO 70
30 PRINT \"Error\"
40 PRINT \"Test 2\"
50 END
60 PRINT \"Error\"
70 PRINT \"Test 1\"
80 GOTO 40
90 PRINT \"Error\"
"
 "BITWARDEN Test: GOTO

Test 1
Test 2")

;; gosub/return
(bitwarden-test
 "
10 REM BITWARDEN Test: GOSUB/RETURN
20 PRINT \"BITWARDEN Test: GOSUB/RETURN\": PRINT
30 PRINT \"Pass 1\"
40 GOSUB 80
50 PRINT \"Pass 2\"
60 GOSUB 140
70 END
80 PRINT \"Test 1\"
90 GOSUB 140
100 RETURN
110 PRINT \"Test 3\"
120 GOSUB 170
130 RETURN
140 PRINT \"Test 2\"
150 GOSUB 110
160 RETURN
170 PRINT \"Test 4\"
180 RETURN
190 END
"
 "BITWARDEN Test: GOSUB/RETURN

Pass 1
Test 1
Test 2
Test 3
Test 4
Pass 2
Test 2
Test 3
Test 4
")

;; input
(bitwarden-test
 "
10 REM BITWARDEN Test: INPUT
20 PRINT \"BITWARDEN Test: INPUT\": PRINT
30 INPUT \"Input number: \", N
40 INPUT \"Input string: \"; S$
50 PRINT \"Number = \", N, \", String = \", \"'\" + S$ + \"'\"
"
 "BITWARDEN Test: INPUT

Input number:   1
Input string: test
Number =        1       , String =      'test'
"
 1 "test")

;; let
(bitwarden-test
 "
10 REM BITWARDEN Test: LET
20 PRINT \"BITWARDEN Test: LET\": PRINT
30 LET A = 1
40 B = 2
50 PRINT \"A = \"; A; \", B = \"; B
"
 "BITWARDEN Test: LET

A = 1, B = 2
")

;; next (see for/next)

;; print
(bitwarden-test
 "
10 REM BITWARDEN Test: PRINT
20 PRINT \"BITWARDEN Test: PRINT\": PRINT
30 LET A = 1
40 LET B = 2
50 LET C$ = \"test\"
60 LET D$ = \"more\"
70 PRINT \"Print test, \"; A; \", \"; B;
80 PRINT \", \"; C$;
90 PRINT \", \"; D$
100 PRINT \"Print test 2, \" + STR$(A) + \", \" + STR$(B);
110 PRINT \", \"+C$;
120 PRINT \", \"+D$
130 PRINT \"Tab test:\"
140 PRINT TAB(5); \"*\"; TAB(10); \"*\"; TAB(15); \"*\"; TAB(20); \"*\"; TAB(25); \"*\"; TAB(30); \"*\"
150 PRINT \"123456789012345678901234567890\"
"
 "BITWARDEN Test: PRINT

Print test, 1, 2, test, more
Print test 2, 1, 2, test, more
Tab test:
    *    *    *    *    *    *
123456789012345678901234567890
")

;; read (see data/read/restore)

;; rem
(bitwarden-test
 "
10 REM BITWARDEN Test: REM
20 PRINT \"BITWARDEN Test: REM\": PRINT
30 REM REM Test
"
 "BITWARDEN Test: REM
")

;; restore (see data/read/restore)

;; return (see gosub/return)

;; stop
(bitwarden-test
 "
10 REM BITWARDEN Test: STOP
20 PRINT \"BITWARDEN Test: STOP\": PRINT
30 STOP
40 PRINT \"Error\"
"
 "BITWARDEN Test: STOP
")

;;; Unit Tests: Reserved Words

;; reserved words
(bitwarden-test
 "
10 REM BITWARDEN Test: Reserved Words
20 PRINT \"BITWARDEN Test: Reserved Words\": PRINT
30 FOR I = 0 TO 100 STEP 2
40   IF I MOD 10 <> 0 THEN 70
50   IF (I <= 20 OR I >= 80) AND I <> 100 THEN 70
60   PRINT I
70 NEXT I
"
 "BITWARDEN Test: Reserved Words

30
40
50
60
70
100
")

;;; Unit Tests: Functions

;; reserved words
(bitwarden-test
 "
10 REM BITWARDEN Test: Functions
20 PRINT \"BITWARDEN Test: Functions\": PRINT
30 PRINT \"ABS(10) = \";  ABS(10)
31 PRINT \"ABS(0) = \";  ABS(0)
32 PRINT \"ABS(-10) = \";  ABS(-10)
40 PRINT \"ASC('A') = \";  ASC(\"A\")
41 PRINT \"ASC('Z') = \";  ASC(\"Z\")
50 PRINT \"ATN(10) = \";  ATN(10)
51 PRINT \"ATN(0) = \";  ATN(0)
52 PRINT \"ATN(-10) = \";  ATN(-10)
60 PRINT \"CHR$(65) = \";  CHR$(65)
61 PRINT \"CHR$(90) = \";  CHR$(90)
70 PRINT \"COS(10) = \";  COS(10)
71 PRINT \"COS(0) = \";  COS(0)
72 PRINT \"COS(-10) = \";  COS(-10)
80 PRINT \"EXP(10) = \";  EXP(10)
81 PRINT \"EXP(0) = \";  EXP(0)
82 PRINT \"EXP(-10) = \";  EXP(-10)
90 PRINT \"INT(10.7) = \";  INT(10.7)
91 PRINT \"INT(0) = \";  INT(0)
92 PRINT \"INT(-10.2) = \";  INT(-10.2)
100 PRINT \"LEFT$('California', 2) = \";  LEFT$(\"California\", 2)
101 PRINT \"LEFT$('Minnesota', 3) = \"; LEFT$(\"Minnesota\", 3)
110 PRINT \"LEN('California') = \"; LEN(\"California\")
111 PRINT \"LEN('Minnesota') = \"; LEN(\"Minnesota\")
120 PRINT \"LOG(10) = \"; LOG(10)
121 PRINT \"LOG(0) = \"; LOG(0)
122 PRINT \"LOG(-10) = \"; LOG(-10)
130 PRINT \"MID$('California', 1) = \"; MID$(\"California\", 1)
130 PRINT \"MID$('California', 2, 3) = \"; MID$(\"California\", 2, 3)
130 PRINT \"MID$('California', 10) = \"; MID$(\"California\", 10)
130 PRINT \"MID$('California', 12) =\"; MID$(\"California\", 12)
131 PRINT \"MID$('Minnesota', 2) = \"; MID$(\"Minnesota\", 2)
131 PRINT \"MID$('Minnesota', 4, 5) = \"; MID$(\"Minnesota\", 4, 5)
131 PRINT \"MID$('Minnesota', 8) = \"; MID$(\"Minnesota\", 8)
131 PRINT \"MID$('Minnesota', 10) =\"; MID$(\"Minnesota\", 10)
140 PRINT \"RND(-10) = \"; RND(-10)
141 PRINT \"RND(0) = \"; RND(0)
150 PRINT \"RIGHT$('California', 2) = \"; RIGHT$(\"California\", 2)
151 PRINT \"RIGHT$('Minnesota', 3) = \"; RIGHT$(\"Minnesota\", 3)
160 PRINT \"SGN(10) = \"; SGN(10)
161 PRINT \"SGN(0) = \"; SGN(0)
162 PRINT \"SGN(-10) = \"; SGN(-10)
170 PRINT \"SIN(10) = \"; SIN(10)
171 PRINT \"SIN(0) = \"; SIN(0)
172 PRINT \"SIN(-10) = \"; SIN(-10)
180 PRINT \"SQR(10) = \"; SQR(10)
181 PRINT \"SQR(0) = \"; SQR(0)
182 PRINT \"SQR(-10) = \"; SQR(-10)
190 PRINT \"STR$(10) = \"; STR$(10)
191 PRINT \"STR$(0) = \"; STR$(0)
192 PRINT \"STR$(-10) = \"; STR$(-10)
200 PRINT \"TAB(1) =\"; TAB(1); \"Tabbed 1\"
201 PRINT \"TAB(5) =\"; TAB(5); \"Tabbed 5\"
202 PRINT \"TAB(10) =\"; TAB(10); \"Tabbed 10\"
203 PRINT \"TAB(20) = \"; TAB(20); \"Tabbed 20\"
210 PRINT \"TAN(10) = \"; TAN(10)
211 PRINT \"TAN(0) = \"; TAN(0)
212 PRINT \"TAN(-10) = \"; TAN(-10)
220 PRINT \"VAL('10') = \"; VAL(\"10\")
221 PRINT \"VAL('0') = \"; VAL(\"0\")
222 PRINT \"VAL('-10') = \"; VAL(\"-10\")
"
 "BITWARDEN Test: Functions

ABS(10) = 10
ABS(0) = 0
ABS(-10) = 10
ASC('A') = 65
ASC('Z') = 90
ATN(10) = 1.4711276743037347
ATN(0) = 0.0
ATN(-10) = -1.4711276743037347
CHR$(65) = A
CHR$(90) = Z
COS(10) = -0.8390715290764524
COS(0) = 1.0
COS(-10) = -0.8390715290764524
EXP(10) = 22026.465794806718
EXP(0) = 1.0
EXP(-10) = 4.5399929762484854e-05
INT(10.7) = 10
INT(0) = 0
INT(-10.2) = -11
LEFT$('California', 2) = Ca
LEFT$('Minnesota', 3) = Min
LEN('California') = 10
LEN('Minnesota') = 9
LOG(10) = 2.302585092994046
LOG(0) = -1.0e+INF
LOG(-10) = 0.0e+NaN
MID$('California', 1) = California
MID$('California', 2, 3) = ali
MID$('California', 10) = a
MID$('California', 12) =
MID$('Minnesota', 2) = innesota
MID$('Minnesota', 4, 5) = nesot
MID$('Minnesota', 8) = ta
MID$('Minnesota', 10) =
RND(-10) = 0.18235988178869306
RND(0) = 0.18235988178869306
RIGHT$('California', 2) = ia
RIGHT$('Minnesota', 3) = ota
SGN(10) = 1
SGN(0) = 0
SGN(-10) = -1
SIN(10) = -0.5440211108893698
SIN(0) = 0.0
SIN(-10) = 0.5440211108893698
SQR(10) = 3.1622776601683795
SQR(0) = 0.0
SQR(-10) = -0.0e+NaN
STR$(10) = 10
STR$(0) = 0
STR$(-10) = -10
TAB(1) =
Tabbed 1
TAB(5) =
    Tabbed 5
TAB(10) =Tabbed 10
TAB(20) =          Tabbed 20
TAN(10) = 0.6483608274590866
TAN(0) = 0.0
TAN(-10) = -0.6483608274590866
VAL('10') = 10
VAL('0') = 0
VAL('-10') = -10
")

;;; Functional Tests

;; test 1
(bitwarden-test
 "
10 REM BITWARDEN Test 1
20 PRINT \"BITWARDEN Test 1\": PRINT
30 PRINT \"Hello World\"
40 END
"
 "BITWARDEN Test 1

Hello World
")

;; test 2
(bitwarden-test
 "
10 REM BITWARDEN Test 2
20 PRINT \"BITWARDEN Test 2\": PRINT
30 PRINT \"Summing the values between 1 and 100\"
40 LET total = 0
50 FOR I = 1 TO 100
60   LET total = total + I
70 NEXT I
80 PRINT \"The total of all digits between 1 and 100 is \" total
90 END
"
 "BITWARDEN Test 2

Summing the values between 1 and 100
The total of all digits between 1 and 100 is 5050
")

;; test 3
(bitwarden-test
 "
10 REM BITWARDEN Test 3
20 PRINT \"BITWARDEN Test 3\": PRINT
30 INPUT \"What's your age? \"; age
40 PRINT \"You are \" age * 7 \" in dog years!\"
50 END
"
 "BITWARDEN Test 3

What's your age? 35
You are 245 in dog years!
"
 35)

;; test 4
(bitwarden-test
 "
10 REM BITWARDEN Test 4
20 PRINT \"BITWARDEN Test 4\": PRINT
30 A = 1: FOR I = A TO A+2 : PRINT I : NEXT
40 END
"
 "BITWARDEN Test 4

1
2
3
")

;; done
(bitwarden-test
 "
10 REM BITWARDEN Tests Completely Successfully
20 PRINT \"BITWARDEN Tests Completely Successfully\"
30 END
"
 "BITWARDEN Tests Completely Successfully
")

;;; bitwarden-test.el ends here
