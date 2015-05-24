;;;; package.lisp

(defpackage #:graph-algorithms
  (:use #:cl #:cl-speedy-queue)
  (:export #:bfs #:connected-components #:strongly-connected-components))


