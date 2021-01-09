;;; decimation.el --- Decimation Game
;;
;;; Original C# version Copyright (C) 2009 Ed Kolis
;;; Emacs version Copyright (C) 2009 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2009-08-05
;; Version:  1.0
;; Keywords: decimation game
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This is a port of Ed Kolis' game Decimation that he wrote for the 7DRL
;; competion.  More information and the original C# code can be found on his
;; website:
;;
;;   http://edkolis.exofire.net/decimation.php
;;
;; > Decimation is my entry for the 2009 7DRL Competition.  It's a roguelike
;; > game where you're the number zero and your goal, in zero's naturally
;; > nihilistic fashion, is to wipe out all the other numbers!  But you have
;; > to use mathematical operations in order to make them equal zero...
;;
;;; Installation:
;;
;; Put `decimation.el' where you keep your elisp files and add the following
;; to your .emacs file:
;;
;;   (require 'decimation)
;;   (autoload 'decimation "decimation" "Decimation game for GNU Emacs." t)
;;
;;; Usage:
;;
;; Run the game with the `decimation' command (M-x decimation RET).
;;
;; Press 'h' for in-game help.

;;; Code:

;; customize group
(defgroup decimation nil
  "Decimation game."
  :prefix "decimation-"
  :group 'games)

;; buffer name
(defcustom decimation-buffer-name
  "*Decimation*"
  "Buffer name for game output."
  :type 'string
  :group 'decimation)

;; help buffer name
(defcustom decimation-help-buffer-name
  "*Decimation Help*"
  "Buffer name for game help."
  :type 'string
  :group 'decimation)

;; grid width
(defcustom decimation-grid-width
  20
  "Number of X squares in game grid."
  :type 'number
  :group 'decimation)

;; grid height
(defcustom decimation-grid-height
  20
  "Number of Y squares in game grid."
  :type 'number
  :group 'decimation)

;; monsters
(defcustom decimation-monsters
  9
  "Number of monsters to start with."
  :type 'number
  :group 'decimation)

;; hero rationality (hit points)
(defcustom decimation-hero-rationality
  100
  "Number of rationality points (hit points) the hero starts with."
  :type 'number
  :group 'decimation)

;; hero moves
(defcustom decimation-hero-moves
  10
  "Number of turns before the hero gets to move."
  :type 'number
  :group 'decimation)

;; suppress compiler warnings for free variables
(defvar decimation-local-hero)
(defvar decimation-local-hero-max-rationality)
(defvar decimation-local-hero-moves)
(defvar decimation-local-grid)
(defvar decimation-local-grid-buffer-pos)
(defvar decimation-local-grid-height)
(defvar decimation-local-grid-offset-height)
(defvar decimation-local-grid-offset-width)
(defvar decimation-local-grid-size)
(defvar decimation-local-grid-space)
(defvar decimation-local-grid-width)
(defvar decimation-local-monsters)
(defvar decimation-local-turn)

;; characters
(defconst decimation-grid-floor-char ?.)
(defconst decimation-grid-hero-char ?0)

;; faces
(defconst decimation-face-floor nil)
(defconst decimation-face-hero 'font-lock-function-name-face)
(defconst decimation-face-monster 'font-lock-constant-face)
(defconst decimation-face-monster-can-see 'font-lock-comment-face)
(defconst decimation-face-banner 'font-lock-keyword-face)

;; decimation mode map
(defvar decimation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up] (lambda () (interactive) (decimation-hero-move 0 -1)))
    (define-key map [down] (lambda () (interactive) (decimation-hero-move 0 1)))
    (define-key map [left] (lambda () (interactive) (decimation-hero-move -1 0)))
    (define-key map [right] (lambda () (interactive) (decimation-hero-move 1 0)))
    (define-key map "1" (lambda () (interactive) (decimation-hero-move -1 1)))
    (define-key map "2" (lambda () (interactive) (decimation-hero-move 0 1)))
    (define-key map "3" (lambda () (interactive) (decimation-hero-move 1 1)))
    (define-key map "4" (lambda () (interactive) (decimation-hero-move -1 0)))
    (define-key map "5" (lambda () (interactive) (decimation-hero-move 0 0)))
    (define-key map "6" (lambda () (interactive) (decimation-hero-move 1 0)))
    (define-key map "7" (lambda () (interactive) (decimation-hero-move -1 -1)))
    (define-key map "8" (lambda () (interactive) (decimation-hero-move 0 -1)))
    (define-key map "9" (lambda () (interactive) (decimation-hero-move 1 -1)))
    (define-key map "+" #'decimation-hero-addition)
    (define-key map "-" #'decimation-hero-subtraction)
    (define-key map "*" #'decimation-hero-multiplication)
    (define-key map "/" #'decimation-hero-devision)
    (define-key map "h" #'decimation-help)
    (define-key map "n" #'decimation)
    ;;(define-key map "q" (lambda () (interactive) (when (y-or-n-p "Quit game? ") (kill-buffer nil))))
    (define-key map "q" (lambda () (interactive) (kill-buffer nil)))
    map))

;; decimation mode
(defun decimation-mode ()
  "Major mode for playing Decimation."
  ;;(interactive)
  (kill-all-local-variables)
  (setq major-mode 'decimation-mode)
  (setq mode-name "Decimation")
  (use-local-map decimation-mode-map)
  (setq buffer-read-only t))

;; convert grid position to x/y coordinates
(defun decimation-grid-pos-to-xy (pos)
  "Return X/Y coordinates on grid for POS.
\nReturn nil if position is invalid."
  (if (or (not pos)
          (< pos 0)
          (>= pos (length decimation-local-grid)))
      nil
    (let* ((y (truncate (/ pos decimation-local-grid-width)))
           (x (- pos (* y decimation-local-grid-width))))
      (list x y))))

;; convert x/y coordinates to grid position
(defun decimation-xy-to-grid-pos (x y)
  "Return grid position from X/Y coordinates.
\nReturn nil if coordinates are invalid."
  (if (or (< x 0)
          (< y 0)
          (>= x decimation-local-grid-width)
          (>= y decimation-local-grid-height))
      nil
    (+ (* y decimation-local-grid-width) x)))

;; draw char
(defun decimation-draw-char (pos char)
  "Insert CHAR at tile position POS on the game grid,
overwriting the old character.
\nColor properties should already be applied to CHAR."
  (let* ((inhibit-read-only t))
    (goto-char (aref decimation-local-grid-buffer-pos pos))
    (delete-char 1)
    (insert char)
    (goto-char (point-max))))

;; convert digit to character
(defun decimation-digit-to-char (digit)
  "Return ASCII character code of DIGIT."
  (progn
    (assert (numberp digit))
    (assert (and (>= digit 0) (<= digit 9)))
    (+ digit 48)))

;; in bounds
(defun decimation-grid-pos-in-bounds (pos grid)
  "Return t if POS is in GRID boundaries, nil otherwise."
  (if (or (not pos)
          (< pos 0)
          (>= pos (length grid)))
      nil
    t))

;; output status
(defun decimation-output-status ()
  "Output status (hero's rationality and turn counter) to the screen."
  (let ((inhibit-read-only t)
        (rationality (first decimation-local-hero)))
    (when (< rationality 0)
      (setq rationality 0))
    (goto-char (point-max))
    ;; output rationality
    (forward-line -3)
    (goto-char (point-at-bol))
    (kill-region (point) (point-at-eol))
    (indent-to decimation-local-grid-offset-width)
    (insert (format "Rationality: %d" rationality))
    ;; output turn
    (forward-line 1)
    (goto-char (point-at-bol))
    (kill-region (point) (point-at-eol))
    (indent-to decimation-local-grid-offset-width)
    (insert (format "Turn: %d" (/ decimation-local-turn 10)))
    (goto-char (point-min))))

;; check hero status
(defun decimation-hero-status ()
  "Return t if hero is alive and some monsters are still left, nil otherwise."
  (sit-for 0)
  (if (and (> (first decimation-local-hero) 0)
           (> (length decimation-local-monsters) 0))
      t
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (goto-char (point-at-bol))
      (kill-region (point) (point-at-eol))
      (indent-to decimation-local-grid-offset-width)
      (if (= (length decimation-local-monsters) 0)
          (progn
            (insert (propertize "YOU WIN" 'face decimation-face-banner))
            (message "Hiro has killed all monsters, you win"))
        (progn
          (insert (propertize "YOU LOOSE" 'face decimation-face-banner))
          (message "Hero has died, you loose")))
      (insert "  (press 'n' to start a new game)")
      (goto-char (point-min))
      nil)))

;; set monster
(defun decimation-set-monster (monster new-pos new-type can-see)
  "Update a specific MONSTER with a new position POS and an optional NEW-TYPE.
\nIf NEW-POS is nil, the monster is removed (killed).
If NEW-TYPE is nil, the monster type is not changed.
If CAN-SEE is t, the monster's color is changed."
  (let (new-monsters
        (mnum (first monster))
        (old-type (second monster))
        (old-pos (third monster))
        (new-type (or new-type (second monster))))
    ;; loop through all current monsters
    (dolist (monster decimation-local-monsters)
      ;; if monster found
      (if (= (first monster) mnum)
          ;; replace with new position
          (if new-pos
              (progn
                (push (list mnum new-type new-pos) new-monsters)
                ;; re-draw monster
                (decimation-draw-char old-pos (propertize
                                               (char-to-string decimation-grid-floor-char)
                                               'face decimation-face-floor))
                (if can-see
                    (decimation-draw-char new-pos (propertize
                                                   (char-to-string (decimation-digit-to-char new-type))
                                                   'face decimation-face-monster-can-see))
                  (decimation-draw-char new-pos (propertize
                                                 (char-to-string (decimation-digit-to-char new-type))
                                                 'face decimation-face-monster))))
            ;; othwise, monster is dead, so remove from grid
            (decimation-draw-char old-pos (propertize
                                           (char-to-string decimation-grid-floor-char)
                                           'face decimation-face-floor)))
        ;; otherwise, just add it to new list
        (push monster new-monsters)))
    ;; update master monster list
    ;;(message "Monsters: %s" (reverse new-monsters))
    (setq decimation-local-monsters (nreverse new-monsters))))

;; kill monster
(defun decimation-kill-monster (monster)
  "Remove MONSTER from monster list and award the hero with some rationality points."
  ;; remove monster from monster list
  (decimation-set-monster monster nil nil nil)
  ;; award hero some rationality points
  (let* ((rationality (/ (+ (first decimation-local-hero) decimation-local-hero-max-rationality) 2))
         (increase (- rationality (first decimation-local-hero))))
    (setq decimation-local-hero (list rationality (second decimation-local-hero)))
    (message "You killed a monster and received %s rationality back" increase)))

;; generate initial game grid
(defun decimation-initialize-grid (width height monster-count rationality moves)
  "Initialize Decimation game grid.
\nWIDTH is the size of the grid in the horizontal, defaults to `decimation-grid-width'.
HEIGHT is the size of the grid in the vertical, defaults to `decimation-grid-height'.
MONSTER-COUNT is the number of random monsters to create, defaults to `decimation-monsters'.
RATIONALITY is the hero's maximum hit points, defaults to `decimation-rationality'."
  (let (grid-free)
    (setq
     ;; grid width
     decimation-local-grid-width width
     ;; grid height
     decimation-local-grid-height height
     ;; grid size
     decimation-local-grid-size (* width height)
     ;; initialize grid with floor characters
     decimation-local-grid (make-vector decimation-local-grid-size decimation-grid-floor-char)
     ;; clear monsters
     decimation-local-monsters nil
     ;; clear hero
     decimation-local-hero nil
     ;; set hero max rationality
     decimation-local-hero-max-rationality rationality
     ;; set hero moves
     decimation-local-hero-moves moves)
    ;; make sure grid is large enough
    (when (< decimation-local-grid-size (1+ monster-count))
      (error "Grid size must be large enough to hold the monsters and the hero"))
    ;; initialize free grid tiles
    (dotimes (x decimation-local-grid-size)
      (push x grid-free))
    ;; place monsters
    (dotimes (x monster-count)
      (let ((type (1+ (mod x 9)))
            (pos (nth (random (length grid-free)) grid-free)))
        ;;(message "monster: %s, type: %s, position: %s" x type pos)
        ;; add monster to monsters
        (push (list x type pos) decimation-local-monsters)
        ;; put monster in grid
        (aset decimation-local-grid pos (decimation-digit-to-char type))
        ;; remove grid tile
        (delq pos grid-free)))
    (setq decimation-local-monsters (nreverse decimation-local-monsters))
    ;; place hero
    (let ((pos (nth (random (length grid-free)) grid-free)))
      ;; set hero info
      (setq decimation-local-hero (list rationality pos))
      ;; put hero in grid
      (aset decimation-local-grid pos decimation-grid-hero-char))))

;; move hero
(defun decimation-hero-move (dx dy)
  "Move hero DX in the X axis and DY in the Y axis."
  (let* ((hero-pos (second decimation-local-hero))
         (hero-xy (decimation-grid-pos-to-xy hero-pos))
         (new-pos (decimation-xy-to-grid-pos
                   (+ (first hero-xy) dx)
                   (+ (second hero-xy) dy))))
    (when new-pos
      (cond
       ;; check hero status
       ((not (decimation-hero-status)))
       ;; cannot move off the grid
       ((not (decimation-grid-pos-in-bounds new-pos decimation-local-grid))
        (message "Cannot move hero out of bounds"))
       ;; cannot move onto monster tile
       ((member new-pos (mapcar 'third decimation-local-monsters))
        (message "Cannot move hero onto a monster"))
       ;; move hero
       (t
        (aset decimation-local-grid hero-pos decimation-grid-floor-char)
        (aset decimation-local-grid new-pos decimation-grid-hero-char)
        (decimation-draw-char hero-pos (propertize
                                        (char-to-string decimation-grid-floor-char)
                                        'face decimation-face-floor))
        (decimation-draw-char new-pos (propertize
                                       (char-to-string decimation-grid-hero-char)
                                       'face decimation-face-hero))))
      (setq decimation-local-hero (list (first decimation-local-hero) new-pos))
      ;; move monsters
      (decimation-move-monsters))))

;; monster can see
(defun decimation-monster-can-see (monster hero-pos)
  "Return t if monster can see hero, nil otherwise."
  (let ((monster-xy (decimation-grid-pos-to-xy (third monster)))
        (hero-xy (decimation-grid-pos-to-xy hero-pos)))
    (if (> (max (abs (- (first monster-xy) (first hero-xy)))
                (abs (- (second monster-xy) (second hero-xy))))
           (second monster))
        nil
      t)))

;; move monsters
(defun decimation-move-monsters ()
  "Move monsters according to game logic."
  ;; loop until hero gets another move
  (let ((move decimation-local-hero-moves))
    (while (and (> move 0)
                (decimation-hero-status))
      (setq move (1- move))
      (let (moving-monsters
            (hero-pos (second decimation-local-hero)))
        ;; determine which monsters move this turn
        (dolist (monster decimation-local-monsters)
          (when (= (mod decimation-local-turn (second monster)) 0)
            (push monster moving-monsters)))
        ;; move eligible monsters
        (dolist (monster moving-monsters)
          (let ((x (first (decimation-grid-pos-to-xy (third monster))))
                (y (second (decimation-grid-pos-to-xy (third monster)))))
            (if (decimation-monster-can-see monster hero-pos)
                ;; if monster can see hero, then it moves towards him
                (let* ((monster-xy (decimation-grid-pos-to-xy hero-pos))
                       (dx (- x (first monster-xy)))
                       (dy (- y (second monster-xy)))
                       (nx (cond
                            ((= dx 0) x)
                            ((> dx 0) (1- x))
                            (t (1+ x))))
                       (ny (cond
                            ((= dy 0) y)
                            ((> dy 0) (1- y))
                            (t (1+ y)))))
                  (decimation-move-monster monster (decimation-xy-to-grid-pos nx ny) t))
              ;; otherwise randomly move monster
              (let (dx
                    dy
                    pos)
                (while (or (not pos)
                           (not (decimation-grid-pos-in-bounds pos decimation-local-grid))
                           (and (= dx 0) (= dy 0)))
                  (setq dx (1- (random 3))
                        dy (1- (random 3))
                        pos (decimation-xy-to-grid-pos (+ x dx) (+ y dy))))
                (decimation-move-monster monster (decimation-xy-to-grid-pos (+ x dx) (+ y dy)) nil)))))
        ;; increment turn counter
        (setq decimation-local-turn (1+ decimation-local-turn))
        ;; output status
        (decimation-output-status)
        ;; check hero status
        (decimation-hero-status)))))

;; move monster and attack
(defun decimation-move-monster (monster pos can-see)
  "Move monster to POS if it is empty.  Attack hero if he is at POS.
\nIf CAN-SEE is non-nil, then monster can see hero."
  (cond
   ;; monster attacks hero
   ((= (second decimation-local-hero) pos)
    ;; move monster in place to make sure color is set correctly
    (decimation-set-monster monster (third monster) nil t)
    (let* ((type (second monster))
           (damage 0))
      (dotimes (x type)
        (setq damage (+ damage (random type) 1)))
      (message "Monster %d hits you for %d damage" type damage)
      (setq decimation-local-hero (list (- (first decimation-local-hero) damage) (second decimation-local-hero)))))
   ;; move monster
   ((not (member pos (mapcar 'third decimation-local-monsters)))
    (decimation-set-monster monster pos nil can-see))))

;; monsters next to hero
(defun decimation-monsters-next-to-hero (monsters hero-pos)
  "Return list of monsters that are next to the hero."
  (let ((monsters-next)
        (hero-xy (decimation-grid-pos-to-xy hero-pos)))
    (dolist (monster monsters)
      (let ((monster-xy (decimation-grid-pos-to-xy (third monster))))
        (when (<= (max (abs (- (first monster-xy) (first hero-xy)))
                       (abs (- (second monster-xy) (second hero-xy))))
                  1)
          (push monster monsters-next))))
    ;;(message "Monsters next to hero: %s" (reverse monsters-next))
    (nreverse monsters-next)))

;; hero attack
(defun decimation-hero-attack (funct)
  "Hero performs an attack, applying FUNCT to monster type."
  ;; check hero status
  (when (decimation-hero-status)
    (let ((monster-targets (decimation-monsters-next-to-hero decimation-local-monsters (second decimation-local-hero))))
      (if (> (length monster-targets) 0)
          (progn
            (dolist (monster monster-targets)
              (let* ((type (second monster))
                     (pos (third monster))
                     (new-type (funcall funct type)))
                (if (= new-type 0)
                    (decimation-kill-monster monster)
                  (decimation-set-monster monster pos new-type t))))
            ;; output status
            (decimation-output-status)
            ;; move monsters
            (decimation-move-monsters))
        (message "There are no monsters close enough to attack")))))

;; hero add
(defun decimation-hero-addition ()
  "Hero attacks with addition."
  (interactive)
  (decimation-hero-attack (lambda (x) (% (+ x 3) 10))))

;; hero subtract
(defun decimation-hero-subtraction ()
  "Hero attacks with subtraction."
  (interactive)
  (decimation-hero-attack (lambda (x) (% (1- x) 10))))

;; hero multiplication
(defun decimation-hero-multiplication ()
  "Hero attacks with multiplication."
  (interactive)
  (decimation-hero-attack (lambda (x) (% (* x x) 10))))

;; hero devision
(defun decimation-hero-devision ()
  "Hero attacks with devision."
  (interactive)
  (decimation-hero-attack (lambda (x) (% (ceiling (/ x 2.0)) 10))))

;; draw screen
(defun decimation-draw-screen ()
  "Draw game screen from entries in GRID."
  (switch-to-buffer (get-buffer-create decimation-buffer-name))
  (setq decimation-local-grid-buffer-pos (make-vector decimation-local-grid-size nil))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (x decimation-local-grid-offset-height)
      (newline))
    (dotimes (y decimation-local-grid-height)
      (indent-to decimation-local-grid-offset-width)
      (dotimes (x decimation-local-grid-width)
        (let* ((pos (+ (* y decimation-local-grid-width) x))
               (char (aref decimation-local-grid pos)))
          (aset decimation-local-grid-buffer-pos pos (point))
          (cond
           ((= char decimation-grid-floor-char)
            (insert (propertize (char-to-string char) 'face decimation-face-floor)))
           ((= char decimation-grid-hero-char)
            (insert (propertize (char-to-string char) 'face decimation-face-hero)))
           (t
            (insert (propertize (char-to-string char) 'face decimation-face-monster))))
          (when (= decimation-local-grid-space 2)
            (insert " "))))
      (newline))
    (newline 5)
    (decimation-output-status))
  ;; move monsters in place to make sure color is set correctly
  (dolist (monster decimation-local-monsters)
    (when (decimation-monster-can-see monster (second decimation-local-hero))
      (decimation-set-monster monster (third monster) nil t))))

;; play a game
;;;###autoload
(defun decimation ()
  "Play Decimiation game."
  (interactive)
  ;; switch to decimation buffer
  (switch-to-buffer (get-buffer-create decimation-buffer-name))
  ;; switch to major mode and initialize
  (decimation-mode)
  (setq case-fold-search nil
        truncate-lines nil
        show-trailing-whitespace nil
        fill-column (1- (window-width)))
  (buffer-disable-undo (current-buffer))
  ;; initialize local variables
  (setq
   ;; grid
   decimation-local-grid nil
   ;; grid buffer positions
   decimation-local-grid-buffer-pos nil
   ;; monsters info
   decimation-local-monsters nil
   ;; hero info
   decimation-local-hero nil
   ;; hero moves
   decimation-local-hero-moves nil
   ;; hero maximum rationality
   decimation-local-hero-max-rationality nil
   ;; turn
   decimation-local-turn 0
   ;; whitespace between cells
   decimation-local-grid-space (if (and (>= (window-width) (1- (* decimation-grid-width 2)))
                                        (>= (window-height) (1- (* decimation-grid-height 2))))
                                   2
                                 1)
   ;; left margin
   decimation-local-grid-offset-width (if (= decimation-local-grid-space 2)
                                          (/ (- (1- (window-width)) (1- (* decimation-grid-width 2))) 2)
                                        (/ (- (1- (window-width)) decimation-grid-width) 2))
   ;; right margin
   decimation-local-grid-offset-height (if (= decimation-local-grid-space 2)
                                           (/ (- (1- (window-height)) (1- (* decimation-grid-height 2))) 2)
                                         (/ (- (1- (window-height)) decimation-grid-height) 2)))
  ;; initialize
  (decimation-initialize-grid decimation-grid-width decimation-grid-height decimation-monsters
                              decimation-hero-rationality decimation-hero-moves)
  ;; draw screen
  (decimation-draw-screen))

;; print help screen
;; this is taken from the original README by Ed Kolis
;;;###autoload
(defun decimation-help ()
  "Print help screen."
  (interactive)
  (let ((inhibit-read-only t))
    (switch-to-buffer (get-buffer-create decimation-help-buffer-name) t)
    (erase-buffer)
    (insert "\"Decimation is a rogue-like game where you're the number zero and
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

All commands in Decimation are performed using the numeric
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

(provide 'decimation)

;;; decimation.el ends here
