;;; maze.el --- Maze Generator
;;
;;; Copyright (C) 2013 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2013-06-25
;; Version:  0.1
;; Keywords: maze generator
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
;;
;;; Installation:
;;
;; Put `maze.el' where you keep your elisp files and add something like the
;; following to your .emacs file:
;;
;;   (autoload 'maze "maze" "Maze generator." t)
;;
;;; Usage:
;;
;; (maze)

;;; Code:

;; original C code found here: http://homepages.cwi.nl/~tromp/maze.html

;; char M[3];           /* holds the 2 characters printed for each cell */
;; int H,                       /* height of the maze */
;;     C,                       /* current cell */
;;     E,                       /* temporary pointer used in the updating */
;;     L[40],R[40];        /* left and right pointers */

;; main()
;; {
;;   L[0] = scanf("%d",&H);             /* reads height and sets L[0] to 1 */
;;   for (E = 40; --E; L[E] = R[E] = E)
;;     printf("._");                    /* close top of maze */
;;   printf("\n|");
;;   while (--H)                           /* more rows to do */
;;   { for (C = 40; --C; printf(M))     /* visit cells from left to right */
;;     { if (C != (E=L[C-1]) && 6<<27<rand())   /* make right-connection ? */
;;       { R[E] = R[C];                 /* link E */
;;         L[R[C]] = E;                 /* to R[C] */
;;         R[C] = C-1;                  /* link C */
;;         L[C-1] = C;                  /* to C-1 */
;;         M[1] = '.';                  /* no wall to the right */
;;       }
;;       else M[1] = '|';                       /* wall to the right */
;;       if (C != (E=L[C]) && 6<<27<rand())     /* omit down-connection ? */
;;       { R[E] = R[C];                 /* link E */
;;         L[R[C]] = E;                 /* to R[C] */
;;         L[C] = C;                    /* link C */
;;         R[C] = C;                    /* to C */
;;         M[0] = '_';                  /* wall downward */
;;       }
;;       else M[0] = ' ';                       /* no wall downward */
;;     }
;;     printf("\n|");
;;   }
;;   M[0] = '_';                                /* close bottom of maze */
;;   for (C = 40; --C; printf(M))               /* bottom row */
;;   { if (C != (E=L[C-1]) && (C == R[C] || 6<<27<rand()))
;;     { L[R[E]=R[C]]=E;
;;       L[R[C]=C-1]=C;
;;       M[1] = '.';
;;     }
;;     else M[1] = '|';
;;     E = L[C];
;;     R[E] = R[C];
;;     L[R[C]] = E;
;;     L[C] = C;
;;     R[C] = C;
;;   }
;;   printf("\n");
;; }

;; maze output buffer name
(defconst maze-output-buffer-name
  "*maze-output*"
  "Buffer name to use for maze output.")

(defmacro maze-random-check ()
  "Return non-nil if random check passes, otherwise return nil."
  (< (lsh 6 27) (random 2147483648)))

;; maze
;;;###autoload
(defun maze (&optional width height)
  "Generate a maze of size WIDTH and HEIGHT."
  (interactive)
  (let ((output-buffer (get-buffer-create maze-output-buffer-name))) ; output buffer
    (save-current-buffer
      (set-buffer output-buffer)
      (erase-buffer)
      (let ((width (1+ (or width (/ (window-width) 2))))
            (height (1- (or height (- (window-height) 5)))))
        (let ((m (make-vector 2 " "))
              (left (make-vector width 0))
              (right (make-vector width 0)))
          ;; top
          (dotimes (x (1- width))
            (aset left x x)
            (aset right x x)
            (insert "._"))
          (insert ".\n|")
          ;; middle
          (aset right (1- width) width)
          (while (plusp height)
            (dotimes (x (1- width))
              (let ((e (aref left (1+ x))))
                (if (and (/= x e) (maze-random-check))
                    (progn
                      (aset right e (aref right x))
                      (aset left (aref right x) e)
                      (aset right x (1+ x))
                      (aset left (1+ x) x)
                      (aset m 1 "."))
                  (aset m 1 "|")))
              (let ((e (aref left x)))
                (if (and (/= x e) (maze-random-check))
                    (progn
                      (aset right e (aref right x))
                      (aset left (aref right x) e)
                      (aset left x x)
                      (aset right x x)
                      (aset m 0 "_"))
                  (aset m 0 " ")))
              (insert (cl-reduce #'concat m)))
            (insert "\n|")
            (setq height (1- height)))
          ;; bottom
          (aset m 0 "_")
          (dotimes (x (1- width))
            (let ((e (aref left (1+ x))))
              (if (and (/= x e) (or (= x (aref right x)) (maze-random-check)))
                  (progn
                    (aset right e (aref right x))
                    (aset left (aref right x) e)
                    (aset right x (1+ x))
                    (aset left (1+ x) x)
                    (aset m 1 "."))
                (aset m 1 "|"))
              (setq e (aref left x))
              (aset right e (aref right x))
              (aset left (aref right x) e)
              (aset left x x)
              (aset right x x))
            (insert (cl-reduce #'concat m)))
          (insert "\n"))))
    (switch-to-buffer output-buffer)))

(provide 'maze)

;;; maze.el ends here
