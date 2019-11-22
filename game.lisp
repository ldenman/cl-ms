;;;; game.lisp
(in-package #:game)

(defun make-counter ()
  (let ((count 0))
    (lambda ()
      (setq count (+ 1 count)))))

(defun board-print-data (board)
  (minesweeper:board-collect board (lambda (b coord)
			 (cons coord (minesweeper:board-tile-at b coord)))))

(defun game-bombs-locations (board)
  (flatten (minesweeper:board-collect board
   (lambda (b coord)
     (let ((tile (minesweeper:board-tile-at b coord)))
       (if (eq 't (tile-bomb tile))
	   (cons coord tile)))))))

(defun game-print-debug (game)
  (let ((string ""))
    (dolist (row-data (board-print-data (game-board game)))
      (setf string (format t "~C ~C" #\linefeed #\linefeed  ))
      (dolist (column row-data)
	(setf string (format t "|~A ~A,~A ~A ~A" (determine-char column) (minesweeper:xy-x (car column)) (minesweeper:xy-y (car column)) (minesweeper:tile-bomb-count (cdr column)) #\linefeed  ))
	))
    (format t "~s ~C" string #\linefeed)))
(defun determine-char (data)
  (let ((tile (cdr data)))
    (cond
      ((and (eq 't (minesweeper:tile-open tile))
	    (eq 't (minesweeper:tile-bomb tile))) "!")
      ((eq 't (minesweeper:tile-open tile))
       (if (= 0 (minesweeper:tile-bomb-count tile))
	   "."
	   (write-to-string (minesweeper:tile-bomb-count tile))))
      (t "X"))))
(defun game-print (game)
  (let ((string ""))
    (dolist (row-data (board-print-data (game-board game)))
      (setf string (format t "~C ~C" #\linefeed #\linefeed  ))
      (dolist (column row-data)
	(setf string (format t "|~2,'0d: ~A" (minesweeper:tile-id (cdr column)) (determine-char column)  #\linefeed  ))
	))
    (format t "~a ~C" string #\linefeed)))
;; Game
(defstruct game board win turn-counter)

(defun collect-closed-tiles (game)
  (flatten
   (board-collect (game-board game)
		  (lambda (board coord)
		    (if (not (eq 't (minesweeper:tile-open (minesweeper:board-tile-at board coord))))
			(minesweeper:board-tile-at board coord))))))

(defun only-mines-left? (game)
  (every
   (lambda (tile)
     (eq (minesweeper:tile-bomb tile) 't))
   (collect-closed-tiles game)))

(defun game-finished-condition-p (game)
  (cond
    ((eq 'f (game-win game)) 't)
    ((only-mines-left? game)
     (progn
       (setf (game-win game) 't)
       't))
    (t 'f)))

(defvar *game*)

(defun determine-widget (x)
  (cond
    ((and (eq 't (minesweeper:tile-open x))
	  (eq 't (minesweeper:tile-bomb x)))
     (make-instance 'gtk-button
		    :label
		     "BOMB!"))
    ((and (eq 't (minesweeper:tile-open x)) (= 0 (minesweeper:tile-bomb-count x)))
     (make-instance 'gtk-button
		    :label
		    "-"))
    ((and (eq 't (minesweeper:tile-open x)))
     (make-instance 'gtk-button
		    :label
		    (write-to-string (minesweeper:tile-bomb-count x))))
    (t
     (make-instance 'gtk-button
		    :label
		    ""))))

(defun gui-table (game window)
  (let ((table (make-instance 'gtk-table
			      :n-rows 8
			      :n-columns 8
			      :row-spacing 0
			      :column-spacing 0
			      :homogeneous nil)))
    (dotimes (i 8)
      (dotimes (j 8)


	(let ((b  (determine-widget (board-tile-at
				     (game-board game)
				     (minesweeper:make-xy :x i :y j) ))))

	  (g-signal-connect b "clicked"
			    ((lambda (i j)
			       (lambda (button)
				 (declare (ignore button))
				 (evolve game window table
					 (minesweeper:make-xy :x i :y j)))) i j))
          (gtk-table-attach table 
                            b
                            i (+ i 1) j (+ j 1)))))
    table))


(defun gui-replace-table (game window table)
  (gtk-container-remove window table)
  (let ((table (gui-table game window)))
      (gtk-container-add window table)
      (gtk-widget-show-all window)))

(defun gui-win (game window table)
  (gtk-container-remove window table)
  (let ((table (gui-table game window))
	(vbox1 (make-instance 'gtk-box
				 :orientation :vertical
				 :homogeneous nil
				 :spacing 6))
	(label (make-instance 'gtk-label :label "YOU WIN")))
    (gtk-box-pack-start vbox1 label)
    (gtk-box-pack-start vbox1 table)
    (gtk-container-add window vbox1)
    (gtk-widget-show-all window)))

(defun gui-lose (game window table)
  (gtk-container-remove window table)
  (let ((table (gui-table game window))
	(vbox1 (make-instance 'gtk-box
				 :orientation :vertical
				 :homogeneous nil
				 :spacing 6))
	(label (make-instance 'gtk-label :label "YOU LOSE")))
    (gtk-box-pack-start vbox1 label)
    (gtk-box-pack-start vbox1 table)
    (gtk-container-add window vbox1)
    (gtk-widget-show-all window)))

(defun evolve (game window table coord)
  (let ((tile (game-tile-open2! game coord)))
  (check-and-set-win-flag game tile )
  (cond ((eq 'f (game-win game))
	 (progn
	 (gui-lose game window table)))
      ((eq 't (game-win game))
       (gui-win game window table))
      (t (gui-replace-table game window table)))))

(defun play-gui ()
  (let ((game (make-game :turn-counter (make-counter)
			 :board (minesweeper:board-init 8))))
    (setf *game* game)
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
				    :type :toplevel
				    :title "Minesweeper"
				    :border-width 12
				    :default-width 300))
	     (table (gui-table game window))

	     (quit (make-instance 'gtk-button
				  :label "Quit")))
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignore widget))
			    (leave-gtk-main)))
	(g-signal-connect quit "clicked"
			  (lambda (widget)
			    (declare (ignore widget))
			    (gtk-widget-destroy window)))

      
      (gtk-table-attach table quit 0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show-all window)))))

(defun play-game ()
  (let ((game (make-game :board (minesweeper:board-init 8))))
    (setf *game* game)
    (loop while (not (eq 't (game-finished-condition-p game))) do
	 (game-print game)
	 (setq id (minesweeper:prompt-read "Enter #: "))
	 (game-tile-open! game
			  (minesweeper:tile-id-to-coord
			   (game-board game) id)))
    (game-print game)
    (cond ((eq 'f (game-win game))
	   (print "You Lose"))
	  ((eq 't (game-win game))
	   (print "You win")))))

(defun check-and-set-win-flag (game tile)
    (cond
      ((eq 't (minesweeper:tile-bomb tile))
	 (setf (game-win game) 'f))
      (t (if (only-mines-left? game)
	     (setf (game-win game) 't)))))

(defun game-tile-open2! (game coord)
  (let* ((board (game-board game))
	 (tile (minesweeper:board-tile-at board coord)))
    (print (format t "Opening tile: ~A" coord))
    (setf (minesweeper:tile-open tile) 't)
    (if (and
	 (not (eq 't (minesweeper:tile-bomb tile)))
	 (= 0 (minesweeper:tile-bomb-count tile)))
	(dolist (n (minesweeper:board-tile-neighbors board coord))
	  (if (not (eq 't (minesweeper:tile-open (minesweeper:board-tile-at board n))))
	      (game-tile-open2! game n))))
    tile))


(defun game-tile-open! (game coord)
  (let* ((board (game-board game))
	 (tile (minesweeper:board-tile-at board coord)))
    (cond
      ((eq 't (minesweeper:tile-bomb tile))
       (progn
	 (setf (minesweeper:tile-open tile) 't)
	 (setf (game-win game) 'f)))
      (t
       (progn
	 (print (format t "Opening tile: ~A" coord))
	 (setf (minesweeper:tile-open tile) 't)
	 
	 (if (only-mines-left? game)
	     (setf (game-win game) 't))

	 (if (and 't
		  (= 0 (minesweeper:tile-bomb-count tile)))
	     (dolist (n (minesweeper:board-tile-neighbors board coord))
	       (if (and
		    (not (eq 't (minesweeper:tile-bomb tile)))
		    (not (eq 't (minesweeper:tile-open (minesweeper:board-tile-at board n)))))
		   (game-tile-open! game n)))))))))

