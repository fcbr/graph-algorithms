(in-package #:graph-algorithms)

(defvar *g*
  '(:a (:b)
    :b (:c :e :f)
    :c (:d :g)
    :d (:c :g :h)
    :e (:a :f)
    :f (:g)
    :g (:f)
    :h (:g :d)))

(defun test-strongly-connected-components ()
  (let ((sccs nil))
    (strongly-connected-components '(:a :b :c :d :e :f :g :h)
                                   (lambda (n)
                                     (getf *g* n))
                                   (lambda (scc)
                                     (push scc sccs)
                                     :test #'eql))
    (and (= (length sccs) 3)
         (member '(:c :d :h) sccs :test #'equal)
         (member '(:g :f) sccs :test #'equal)
         (member '(:a :b :e) sccs :test #'equal))))


                               
    
    
