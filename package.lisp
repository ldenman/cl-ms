;;;; package.lisp

(defpackage #:minesweeper
  (:use #:cl)
  (:export #:flatten #:board-init #:board-collect #:board-tile-at #:board-tile-neighbors #:tile-open #:make-xy #:tile-bomb #:tile-bomb-count #:tile-id #:tile-id-to-coord #:prompt-read #:xy-x #:xy-y))
(defpackage #:ui
  (:use #:cl))
(defpackage #:game
  (:use	#:cl #:minesweeper #:gtk :gdk-pixbuf :gobject :glib :gio :pango :cairo))
