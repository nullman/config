;;; brainfuck-test.el --- Test Suite for brainfuck.el
;;
;;; Copyright (C) 2016 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2016-03-19
;; Version:  1.0
;; Keywords: brainfuck compiler interpreter mode test
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
;; Tests `brainfuck.el' functionality.
;;
;;; Installation:
;;
;; Load `brainfuck-test.el':
;;
;;   (load "brainfuck-test.el")
;;
;;; Usage:
;;
;; Run tests:
;;
;;   (eval-buffer)

;;; Code:

;; brainfuck
(require 'brainfuck)

;; assert
(defun brainfuck-test (program output &optional input)
  "Run PROGRAM through the BRAINFUCK interpreter and compiler and compare their outputs to OUTPUT.
\nOptional INPUT parameter will be used for the input stream."
  (cl-labels ((test-output (output)
                           (set-buffer "*brainfuck-output*")
                           (let ((buffer-str (buffer-substring-no-properties (point-min) (point-max))))
                             (when (> (length buffer-str) (length output))
                               (setq output (concat output "\n")))
                             (assert (string= buffer-str output))
                             (delete-window))))
    (save-excursion
      ;; interpreter test
      (with-temp-buffer
        (insert program)
        (brainfuck-interpret-run input)
        (test-output output))
      ;; compiler test
      (with-temp-buffer
        (insert program)
        (brainfuck-run input)
        (test-output output)))))

;;; Unit Tests: Parser

;; parse valid statement
(assert (equal '(:greater-than :less-than :plus :minus :period :comma :left-bracket :right-bracket)
               (brainfuck-parse-statement "><+-.,[]")))

;; parse invalid statement with extra jump forward
(assert (condition-case err
            (equal '(:na)
                   (brainfuck-parse-statement "><+-.,[[]"))
          ('error t)))

;; parse invalid statement with extra jump back
(assert (condition-case err
            (equal '(:na)
                   (brainfuck-parse-statement "><+-.,[]]"))
          ('error t)))

;;; Unit Tests: Interpret

(assert (equal "A" (brainfuck-interpret (brainfuck-parse-statement "+++++[>+++++++++++++[>+<-]<-]>>."))))
(assert (equal "ABC" (brainfuck-interpret (brainfuck-parse-statement "+++++[>+++++++++++++[>+<-]<-]>>.+.+."))))
(assert (equal "XYZ" (brainfuck-interpret (brainfuck-parse-statement ",.+.+.") "X")))
(assert (equal "BCD" (brainfuck-interpret (brainfuck-parse-statement ",+.>,+.>,+.") "ABC")))
(assert (equal "Hello World!\n" (brainfuck-interpret (brainfuck-parse-statement "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))))

;;; Unit Tests: Compile
(assert (equal "A" (funcall (brainfuck-compile (brainfuck-parse-statement "+++++[>+++++++++++++[>+<-]<-]>>.")))))
(assert (equal "ABC" (funcall (brainfuck-compile (brainfuck-parse-statement "+++++[>+++++++++++++[>+<-]<-]>>.+.+.")))))
(assert (equal "XYZ" (funcall (brainfuck-compile (brainfuck-parse-statement ",.+.+.")) "X")))
(assert (equal "BCD" (funcall (brainfuck-compile (brainfuck-parse-statement ",+.>,+.>,+.")) "ABC")))
(assert (equal "Hello World!\n" (funcall (brainfuck-compile (brainfuck-parse-statement "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")))))

;;; Functional Tests

;; hello world
;; source: https://en.wikipedia.org/wiki/Brainfuck#Hello_World.21
(brainfuck-test
 "
[
    This program prints 'Hello World!' and a newline to the screen, its length
    is 106 active command characters. [It is not the shortest.]

    This loop is a 'comment loop', a simple way of adding a comment to a BF
    program such that you don't have to worry about any command characters.
    Any '.', ',', '+', '-', '<' and '>' characters are simply ignored, the '['
    and ']' characters just have to be balanced. This loop and the commands
    it contains are ignored because the current cell defaults to a value of 0;
    the 0 value causes this loop to be skipped.
]

+++++ +++               Set Cell #0 to 8
[
    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
    [                   as the cell will be cleared by the loop
        >++             Add 2 to Cell #2
        >+++            Add 3 to Cell #3
        >+++            Add 3 to Cell #4
        >+              Add 1 to Cell #5
        <<<<-           Decrement the loop counter in Cell #1
    ]                   Loop till Cell #1 is zero; number of iterations is 4
    >+                  Add 1 to Cell #2
    >+                  Add 1 to Cell #3
    >-                  Subtract 1 from Cell #4
    >>+                 Add 1 to Cell #6
    [<]                 Move back to the first zero cell you find; this will
                        be Cell #1 which was cleared by the previous loop
    <-                  Decrement the loop Counter in Cell #0
]                       Loop till Cell #0 is zero; number of iterations is 8

The result of this is:
Cell No :   0   1   2   3   4   5   6
Contents:   0   0  72 104  88  32   8
Pointer :   ^

>>.                     Cell #2 has value 72 which is 'H'
>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
+++++++..+++.           Likewise for 'llo' from Cell #3
>>.                     Cell #5 is 32 for the space
<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
<.                      Cell #3 was set to 'o' from the end of 'Hello'
+++.------.--------.    Cell #3 for 'rl' and 'd'
>>+.                    Add 1 to Cell #5 gives us an exclamation point
>++.                    And finally a newline from Cell #6
"
 "Hello World!
")

;; rot-13
;; source: https://en.wikipedia.org/wiki/Brainfuck#ROT13
(brainfuck-test
 "
[
    This program enciphers its input with the ROT13 cipher. To do this, it
    must map characters A-M (ASCII 65-77) to N-Z (78-90), and vice versa.
    Also it must map a-m (97-109) to n-z (110-122) and vice versa. It must
    map all other characters to themselves ; it reads characters one at a time
    and outputs their enciphered equivalents until it reads an EOF (here
    assumed to be represented as either -1 or 'no change'), at which point the
    program terminates.

    The basic approach used is as follows. Calling the input character x,
    divide x-1 by 32, keeping quotient and remainder. Unless the quotient is
    2 or 3, just output x, having kept a copy of it during the division. If
    the quotient is 2 or 3, divide the remainder ((x-1) modulo 32) by 13; if
    the quotient here is 0, output x+13; if 1, output x-13; if 2, output x.

    Regarding the division algorithm, when dividing y by z to get a quotient q
    and remainder r, there is an outer loop which sets q and r first to the
    quotient and remainder of 1/z, then to those of 2/z, and so on; after it
    has executed y times, this outer loop terminates, leaving q and r set to
    the quotient and remainder of y/z. (The dividend y is used as a
    diminishing counter that controls how many times this loop is executed.)
    Within the loop, there is code to increment r and decrement y, which is
    usually sufficient; however, every zth time through the outer loop, it is
    necessary to zero r and increment q. This is done with a diminishing
    counter set to the divisor z; each time through the outer loop, this
    counter is decremented, and when it reaches zero, it is refilled by moving
    the value from r back into it.
]

-,+[                         Read first character and start outer character reading loop
    -[                       Skip forward if character is 0
        >>++++[>++++++++<-]  Set up divisor (32) for division loop
                               (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
        <+<-[                Set up dividend (x minus 1) and enter division loop
            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
            <<<<<-           Decrement dividend
        ]                    End division loop
    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
    >--[-[<->+++[-]]]<[        Zero that flag unless quotient was 2 or 3; zero quotient; check flag
        ++++++++++++<[       If flag then set up divisor (13) for second division loop
                               (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
            >-[>+>>]         Reduce divisor; Normal case: increase remainder
            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
            <<<<<-           Decrease dividend
        ]                    End division loop
        >>[<+>-]             Add remainder back to divisor to get a useful 13
        >[                   Skip forward if quotient was 0
            -[               Decrement quotient and skip forward if quotient was 1
                -<<[-]>>     Zero quotient and divisor if quotient was 2
            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
    <[-]                     Clear remainder from first division if second division was skipped
    <.[-]                    Output ROT13ed character from copy and clear it
    <-,+                     Read next character
]                            End character reading loop
"
 "NOPQRSTUVWXYZABCDEFGHIJKLM"
 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;;; brainfuck-test.el ends here
