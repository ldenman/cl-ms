;;;; minesweeper.asd

(asdf:defsystem #:minesweeper
  :description "Describe minesweeper here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-cffi-gtk")
  :components ((:file "package")
               (:file "minesweeper")
	       (:file "game")))
