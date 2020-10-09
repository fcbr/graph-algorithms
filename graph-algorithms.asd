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
               (:file "maximal-cliques")
               (:file "breadth-first-search")
               (:file "connected-components")
               (:file "degrees")
               (:file "shortest-paths")))

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


  ;;; :perform (test-op (o s) (uiop:symbol-call :fiveam :run! 'graph-algorithms/tests:all-tests)))
