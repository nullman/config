;;; blackjack.el --- Blackjack Game
;;
;;; Copyright (C) 2014 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2014-11-24
;; Version:  0.1
;; Keywords: blackjack game
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
;; Blackjack card game for GNU Emacs.
;;
;;; Installation:
;;
;; Put `blackjack.el' where you keep your elisp files and add the following
;; to your .emacs file:
;;
;;   (require 'blackjack)
;;   (autoload 'blackjack "blackjack" "Blackjack card game for GNU Emacs." t)
;;
;;; Usage:
;;
;; Run the game with the `blackjack' command (M-x blackjack RET).
;;
;; Press 'h' for in-game help.

;;; Code:

;; customize group
(defgroup blackjack nil
  "Blackjack game."
  :prefix "blackjack-"
  :group 'games)

;; buffer name
(defcustom blackjack-buffer-name
  "*Blackjack*"
  "Buffer name for game output."
  :type 'string
  :group 'blackjack)

;; help buffer name
(defcustom blackjack-help-buffer-name
  "*Blackjack Help*"
  "Buffer name for game help."
  :type 'string
  :group 'blackjack)

;; cards
;; .-----.  .-----.  .-----.  .-----.
;; |A .  |  |A _  |  |A_ _ |  |A .  |
;; | /.\ |  | ( ) |  |( v )|  | / \ |
;; |(_._)|  |(_'_)|  | \ / |  | \ / |
;; |  | A|  |  | A|  |  ' A|  |  ' A|
;; `-----'  `-----'  `-----'  `-----'

;; card width
(defconst blackjack-card-width
  9
  "Card width, including two spaces in front.")

;; card suits
(defconst blackjack-suits
  '((1 "S" "Spades" (" . " " /.\ " "(_._)" " | "))
    (2 "C" "Clubs" (" _ " " ( ) " "(_'_)" " | "))
    (3 "H" "Hearts" ("_ _" "( V )" " \ / " " ' "))
    (4 "D" "Diamonds" (" . " " / \ " " \ / " " ' ")))
  "Card suit information.")

;; card faces
(defconst blackjack-faces
  '((1 "A" "Ace")
    (2 "2" "Two")
    (3 "3" "Three")
    (4 "4" "Four")
    (5 "5" "Five")
    (6 "6" "Six")
    (7 "7" "Seven")
    (8 "8" "Eight")
    (9 "9" "Nine")
    (10 "T" "Ten")
    (11 "J" "Jack")
    (12 "Q" "Queen")
    (13 "K" "King"))
  "Card face information.")

;; cards
(defconst blackjack-cards
  `,(let (cards)
      (dolist (x blackjack-suits)
        (dolist (y blackjack-faces)
          (push (append x y) cards)))
      (nreverse cards))
  "Information about the cards in a deck.")

;; ;; array version of cards
;; (defconst blackjack-cards
;;   `,(let ((cards (make-vector 52 nil))
;;           (i -1))
;;       (dolist (x blackjack-suits)
;;         (dolist (y blackjack-faces)
;;           (aset cards (cl-incf i) (append x y))))
;;       cards)
;;   "Information about the cards in a deck.")

;; insert card goto pos
(defun blackjack-insert-card-goto-pos (pos)
  "Move point to POS on current line."
  (goto-char (point-at-bol))
  (while (< (- (point) (point-at-bol)) pos)
    (forward-char 1)
    (when (= (point) (point-at-bol))
      (forward-char -1)
      (insert " "))))

;; insert card clear row
(defun blackjack-insert-card-clear-row ()
  "Clear one card width in current row at current position."
  (kill-region (point)
               (if (> (+ (point) blackjack-card-width) (point-at-eol))
                   (point-at-eol)
                 (+ (point) blackjack-card-width))))

;; insert card
(defun blackjack-insert-card (pos card)
  "Insert CARD at POS of current line, replacing any existing
card at that location."
  (save-excursion
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  .-----.")
    (forward-line 1)
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  |" (nth 5 card) (nth 0 (nth 3 card)) " |")
    (forward-line 1)
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  |" (nth 1 (nth 3 card)) "|")
    (forward-line 1)
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  |" (nth 2 (nth 3 card)) "|")
    (forward-line 1)
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  |" (nth 3 (nth 3 card)) (nth 5 card) "|")
    (forward-line 1)
    (blackjack-insert-card-goto-pos pos)
    (blackjack-insert-card-clear-row)
    (insert "  `-----'")))

;; test cards
(defun blackjack-test-cards ()
  "Display cards."
  (switch-to-buffer (get-buffer-create blackjack-buffer-name))
  ;; setup buffer
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq case-fold-search nil
        truncate-lines nil
        show-trailing-whitespace nil
        fill-column (1- (window-width)))
  (buffer-disable-undo (current-buffer))
  (let ((pos 0)
        (i 0))
    (dolist (c blackjack-cards)
      (when (zerop (mod (cl-incf i) 5))
        ))))

;; play a game
;;;###autoload
(defun blackjack ()
  "Play Blackjack game."
  (interactive)
  ;; switch to blackjack buffer
  (switch-to-buffer (get-buffer-create blackjack-buffer-name))
  ;; setup buffer
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq case-fold-search nil
        truncate-lines nil
        show-trailing-whitespace nil
        fill-column (1- (window-width)))
  (buffer-disable-undo (current-buffer))
  (insert "Blackjack")
  (dotimes (i 16)
    (newline))
  ;; initialize local variables


  (setq
   ;; grid
   blackjack-local-grid nil
   ;; grid buffer positions
   blackjack-local-grid-buffer-pos nil
   ;; monsters info
   blackjack-local-monsters nil
   ;; hero info
   blackjack-local-hero nil
   ;; hero moves
   blackjack-local-hero-moves nil
   ;; hero maximum rationality
   blackjack-local-hero-max-rationality nil
   ;; turn
   blackjack-local-turn 0
   ;; whitespace between cells
   blackjack-local-grid-space (if (and (>= (window-width) (1- (* blackjack-grid-width 2)))
                                        (>= (window-height) (1- (* blackjack-grid-height 2))))
                                   2
                                 1)
   ;; left margin
   blackjack-local-grid-offset-width (if (= blackjack-local-grid-space 2)
                                          (/ (- (1- (window-width)) (1- (* blackjack-grid-width 2))) 2)
                                        (/ (- (1- (window-width)) blackjack-grid-width) 2))
   ;; right margin
   blackjack-local-grid-offset-height (if (= blackjack-local-grid-space 2)
                                           (/ (- (1- (window-height)) (1- (* blackjack-grid-height 2))) 2)
                                         (/ (- (1- (window-height)) blackjack-grid-height) 2)))
  ;; initialize
  (blackjack-initialize-grid blackjack-grid-width blackjack-grid-height blackjack-monsters
                              blackjack-hero-rationality blackjack-hero-moves)
  ;; draw screen
  (blackjack-draw-screen))

;; print help screen
;;;###autoload
(defun blackjack-help ()
  "Print help screen."
  (interactive)
  (let ((inhibit-read-only t))
    (switch-to-buffer (get-buffer-create blackjack-help-buffer-name) t)
    (erase-buffer)
    (insert "\"Blackjack is a rogue-like game where you're the number zero and
your goal, in zero's naturally nihilistic fashion, is to wipe out
all the other numbers!  But you have to use mathematical
operations in order to make them equal zero...\"
          -- Ed Kolis (Author of the original version.)


STORY

You are the number zero.

Being the number zero, you are a nihilist.

This means you want to wipe out all the other impure, barbaric
numbers from the face of the Cartesian plane.


THE GAME WORLD

The game world is very simple: a 20 by 20 grid.

There are no obstacles - just you (the zero) and the
monsters (the other digits from 1 to 9).


THE HERO

As stated above, you are the number zero.

You start with 100 / 100 rationality points which are your HP.

When you are attacked by an adjacent monster, you lose some
rationality.

Should your rationality reach zero or below, you lose the game.

You are slower than all the monsters (you move once every ten
game ticks; see THE MONSTERS for details on monster speeds).

Thus it is important to avoid letting other monsters see you
until you're done dealing with the ones that can already see you.

You have no melee attacks; instead, you have four special attacks
which affect all adjacent monsters with an arithmetic operation.

See COMMANDS for details on commands.


THE MONSTERS

Monsters are the other digits from 1 to 9.

Monsters that can see you will seek after you and attack you.

Monsters that cannot see you will mill about randomly.

Monsters' sight range is equal to their number.

Thus, a one can see you only if you're standing right next to it,
but a nine can see you about halfway across the map.

If a monster can see you, it is displayed in red; otherwise it is
displayed in yellow.

\(The zero and any empty tiles are displayed in brown if they are
in sight range of any monster, and white if they are not.)

Monsters' attack power is XdX, where X is the number of the
monster.

Thus, a one does 1d1 (1) damage per hit, a five does 5d5 (5 to
25) damage per hit, and a nine does 9d9 (9 to 81) damage per hit.

However, lower numbered monsters are pests which move very
quickly.

While the hero moves every 10 game ticks, monsters move and
attack according to their number, so a 5 is twice as fast as the
hero, and a 1 is ten times as fast.



COMMANDS

All commands in Blackjack are performed using the numeric
keypad.

The following commands are available:

1, 2, 3, 4, 6, 7, 8, 9: Move in various directions. (Note that
you cannot use 5 or . to sit still and wait for the monsters to
come to you\; you have to at least side-step.)

+: Attack all adjacent monsters with Addition. (See COMBAT for
more details).

-: Attack all adjacent monsters with Subtraction. (See COMBAT for
 more details).

*: Attack all adjacent monsters with Multiplication. (See COMBAT
 for more details).

/: Attack all adjacent monsters with Division. (See COMBAT for
more details).

n: Start a new game.

q: Quit game.

h: Show this help dialog.


COMBAT

The hero has four attacks with which to attack all adjacent
monsters.

The goal is to transform the numbers so that they equal zero.

Whenever a monster reaches zero, it disappears and the hero
regains half the rationality (HP) that have been lost in
combat (rounded down).

Thus, if the hero is at 75 / 100 HP, and defeats a monster, the
hero's HP will increase by 12 to 87 / 100.

The attacks are as follows (note that if a number reaches 10 or
above, the number is taken \"modulo 10\", that is, all but the last
digit are removed):

Addition (triggered by + key on numpad): Adds three to adjacent
numbers. Not generally useful, but can weaken an eight or nine,
or defeat a seven.

Subtraction (triggered by - key on numpad): Subtracts one from
adjacent numbers. Useful for dealing with small numbers\; will
take too long to wear down larger ones.

Multiplication (triggered by * key on numpad): Multiplies
adjacent numbers by themselves. Does nothing to a five or six,
but is devastating to a nine.

Division (triggered by / key on numpad): Divides adjacent numbers
by two, rounding up. Useful for quickly weakening large numbers.

Monster to player combat is explained in the sections THE HERO
and THE MONSTERS.")
    (goto-char (point-min))))

(provide 'blackjack)

;;; blackjack.el ends here
