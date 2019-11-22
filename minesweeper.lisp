;;;; minesweeper.lisp

(in-package #:minesweeper)

;; Helpers
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read *query-io*))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

;; Tile Logic
(defstruct tile bomb open bomb-count id)
(defun tile-bomb-p (tile)
  (eq 't (tile-bomb tile)))
;; Coordinate Logic
(defstruct xy x y)

;; Board Logic
(defun make-counter ()
  (let ((counter 0))
  (lambda ()
    (setf counter (+ 1 counter)))))
(defvar tile-counter (make-counter))

(defun make-board (size)
  (make-array  (list size size) ))

(defun board-tile-init (b coord)
    (setf (aref b (xy-x coord) (xy-y coord))
	  (make-tile :open 'f
		     :bomb (if (> (random 100) 90) 't 'f))))

(defun board-with-tiles (b tile-fn)
  (destructuring-bind (n m) (array-dimensions b)
    (loop for x from 0 below n do
	 (loop for y from 0 below m do
	      (funcall tile-fn b (make-xy :x x :y y))))))
(defun board-assign-ids (board)
  (let ((tile-counter (make-counter)))
  (board-with-tiles board
		    (lambda (b coord)
		      (setf (tile-id
			     (board-tile-at b coord))
			     (funcall tile-counter))))))
(defun board-count-bombs (board)
  (board-with-tiles board
		    (lambda (b coord)
		      (setf
		       (tile-bomb-count
			(aref b (xy-x coord) (xy-y coord)))
			    (board-count-neighbor-bombs b coord)))))
(defun board-tile-at (board coord)
  (aref board (xy-x coord) (xy-y coord)))
(defun board-init (size)
  (let ((b (make-board size)))
    (board-with-tiles b #'board-tile-init)
    (board-count-bombs b)
    (board-assign-ids b)
    b))
(defun board-print-tile (board coord)
  (print (aref board (xy-x coord) (xy-y coord))))

(defun board-collect (b collect-fn)
 (destructuring-bind (n m) (array-dimensions b)
    (loop for x from 0 below n
	 collect (loop for y from 0 below m
		    collect (funcall collect-fn b (make-xy :x x :y y))))))

(defun board-filter-neighbors (b n-list)
  (when n-list
      (destructuring-bind (n m) (array-dimensions b)
	(let ((coord (car n-list)))
	  (if (board-coordinate-in-bounds-p b coord)
	      (cons coord (board-filter-neighbors b (cdr n-list)))
	      (board-filter-neighbors b (cdr n-list)))))))

(defun board-coordinate-in-bounds-p (b coord)
  (destructuring-bind (n m) (array-dimensions b)
    (and (< (xy-x coord) n) (>= (xy-x coord) 0) (>= (xy-y coord) 0) (< (xy-y coord) m))))

(defun board-tile-neighbors (b coord)
  (let* ((x (xy-x coord))
	 (y (xy-y coord))
	 (neighbor-coordinates (list
	    (make-xy :x x :y (+ y 1));N
	    (make-xy :x x :y (- y 1));S
	    (make-xy :x (+ x 1) :y y);E
	    (make-xy :x (- x 1) :y y);W
	    (make-xy :x (+ x 1) :y (+ y 1));NE
	    (make-xy :x (- x 1) :y (+ y 1));NW
	    (make-xy :x (- x 1) :y (- y 1));SW
	    (make-xy :x (+ x 1) :y (- y 1));SE
	    )))
    (board-filter-neighbors b neighbor-coordinates)))

(defun board-count-neighbor-bombs (b coord)
  (length (remove-if-not
	   (lambda (c) (eq 't (tile-bomb (board-tile-at b c))))
	  (board-tile-neighbors b coord))))

(defun tile-id-to-coord (board id)
  (car (flatten (board-collect
		 board
		 (lambda (b coord)
		   (if (eq id (tile-id (board-tile-at b coord)))
		       coord))))))

