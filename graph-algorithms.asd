;;;; graph-algorithms.asd

(asdf:defsystem #:graph-algorithms
  :description "Assorted graph algorithms."
  :author "Fabricio Chalub <f@cp300.org>"
  :license "MIT"
  :depends-on (#:cl-speedy-queue
               #:minheap
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "test")
               (:file "graph-algorithms")))

