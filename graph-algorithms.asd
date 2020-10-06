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
               (:file "graph-algorithms")))


(asdf:defsystem #:graph-algorithms/tests
  :description "Unit tests for :graph-algorithms."
  :author "Fabricio Chalub <f@cp300.org>"
  :license "MIT"
  :depends-on (#:graph-algorithms
               #:fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
