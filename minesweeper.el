(defun reset-slime ()
  (interactive)
  (slime-quit-lisp)
  (sit-for 1)
  (slime)
  (insert "(ql:quickload \"minesweeper\")")
  (comint-send-input)
  (insert "(in-package :game)")
  (comint-send-input)
  (insert "(play-gui)")
  (comint-send-input)
  )
