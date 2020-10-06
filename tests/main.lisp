(in-package #:graph-algorithms/tests)

(defvar *g2*
  '(1 (2 5)
    2 (1 3 5)
    3 (4 2)
    4 (3 5 6)
    5 (1 2 4)
    6 (4)))

(defvar *g1*
  '(:a (:b)
    :b (:c :e :f)
    :c (:d :g)
    :d (:c :g :h)
    :e (:a :f)
    :f (:g)
    :g (:f)
    :h (:g :d)))

(defun neighbor (graph v)
  "Handy method to return all neighbors of vertex V from graph GRAPH,
assuming that the graph is a property-list (p-list)."
  (getf graph v))

(test strongly-connected-components
  (let ((components nil))
    (strongly-connected-components '(:a :b :c :d :e :f :g :h)
                                   (lambda (n)
                                     (getf *g1* n))
                                   (lambda (scc)
                                     (push scc sccs)
                                     :test #'eql))
    (and (= (length sccs) 3)
         (member '(:c :d :h) sccs :test #'equal)
         (member '(:g :f) sccs :test #'equal)
         (member '(:a :b :e) sccs :test #'equal))))

(defun test-dijkstra ()
  (multiple-value-bind (dist prev)
      (dijkstra :a '(:a :b :c :d :e :f :g :h)
                (lambda (n)
                  (getf *g1* n))
                :test #'eql)
    ;; (maphash (lambda (k v) (format t "~a ~a ~%" k v)) dist)
    (print (reconstruct-path prev :f))))
  
(defun test-maximal-cliques ()
  (maximal-cliques '(1 2 3 4 5 6)
                   (lambda (n)
                     (getf *g2* n))
                   (lambda (c)
                     (format t "[~a]~%" c)) :test #'eql))
