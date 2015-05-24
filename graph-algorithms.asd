;;;; graph-algorithms.asd

(asdf:defsystem #:graph-algorithms
  :description "Describe graph-algorithms here"
  :author "Fabricio Chalub <f@cp300.org>"
  :license "MIT"
  :depends-on (#:cl-speedy-queue
               #:minheap)
  :serial t
  :components ((:file "package")
               (:file "graph-algorithms")))

