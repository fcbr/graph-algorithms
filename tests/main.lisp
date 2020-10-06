(in-package #:graph-algorithms/tests)

(def-suite all-tests
    :description "The master suite of all tests.")

(in-suite all-tests)

(defun test-graph-algorithms ()
  (run! 'all-tests))

(defvar *simple-graph*
  '(:a (:b :c)
    :b (:d)
    :c (:f :a)))

(defvar *complex-graph*
  '(:a (:b)
    :b (:c :e :f)
    :c (:d :g)
    :d (:c :g :h)
    :e (:a :f)
    :f (:g)
    :g (:f)
    :h (:g :d)))

(defvar *cliques*
  '(:a (:b :e)
    :b (:a :c :e)
    :c (:d :b)
    :d (:c :e :f)
    :e (:a :b :d)
    :f (:d)))

(defun simple-bfs ()
  (let ((path nil))
    (bfs :a (lambda (n) (getf *simple-graph* n))
         (lambda (n) (push n path)))
    (reverse path)))

(defun simple-degrees (v)
  (multiple-value-bind (in-fn out-fn)
      (degrees '(:a :b :c :d :f) (lambda (n) (getf *simple-graph* n)))
    (list (funcall in-fn v) (funcall out-fn v))))

(defun complex-dijkstra (from to)
  (multiple-value-bind (prev dist)
      (dijkstra from '(:a :b :c :d :e :f :g :h)
                (lambda (n) (getf *complex-graph* n)))
    (list
     (gethash to dist)
     (reconstruct-path prev to))))

(defun complex-strongly-connected-components ()
  (let ((sccs nil))
    (strongly-connected-components
     '(:a :b :c :d :e :f :g :h)
     (lambda (n)
       (getf *complex-graph* n))
     (lambda (scc)
       (push scc sccs)))
    sccs))

(defun complex-connected-components ()
  (let ((ccs nil))
    (connected-components
     '(:a :b :c :d :e :f :g :h)
     (lambda (n)
       (getf *complex-graph* n))
     (lambda (cc)
       (push cc ccs)))
    ccs))

(defun cliques-maximal-cliques ()
  (let ((cliques nil))
    (maximal-cliques '(:a :b :c :d :e :f)
                     (lambda (n) (getf *cliques* n))
                     (lambda (n) (push n cliques)))
    cliques))

(test bfs
      (is (equal '(:a :b :c :d :f) (simple-bfs))))

(test degrees
      (is (equal '(1 2) (simple-degrees :a)))
      (is (equal '(1 1) (simple-degrees :b)))
      (is (equal '(1 2) (simple-degrees :c)))
      (is (equal '(1 0) (simple-degrees :d)))
      (is (equal '(1 0) (simple-degrees :f))))

(test dijkstra
      (is (equal '(2 (:b :f)) (complex-dijkstra :a :f)))
      (is (equal '(1 (:f)) (complex-dijkstra :b :f)))
      (is (equal '(4 (:b :c :d :h)) (complex-dijkstra :a :h))))

(test maximal-cliques
      (is (equal '((:d :e) (:d :f) (:c :d) (:b :c)
                   (:e :a :b))
                 (cliques-maximal-cliques))))

(test strongly-connected-components
      (is (equal '((:a :b :e)
                   (:c :d :h)
                   (:g :f))
                 (complex-strongly-connected-components))))

(test connected-components
      (is (equal '(:a) (complex-connected-components))))
