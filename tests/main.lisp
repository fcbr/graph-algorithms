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

(defun simple-bfs ()
  (let ((path nil))
    (bfs :a (lambda (n) (getf *simple-graph* n))
         (lambda (n) (push n path)))
    (reverse path)))

(defun simple-degrees (v)
  (multiple-value-bind (in-fn out-fn)
      (degrees '(:a :b :c :d :f) (lambda (n) (getf *simple-graph* n)))
    (list (funcall in-fn v) (funcall out-fn v))))

(test bfs
      (is (equal '(:a :b :c :d :f) (simple-bfs))))

(test degrees
      (is (equal '(1 2) (simple-degrees :a)))
      (is (equal '(1 1) (simple-degrees :b)))
      (is (equal '(1 2) (simple-degrees :c)))
      (is (equal '(1 0) (simple-degrees :d)))
      (is (equal '(1 0) (simple-degrees :f))))
