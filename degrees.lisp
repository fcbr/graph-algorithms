(in-package #:graph-algorithms)

(defun degrees (vertices neighbors-fn)
  "Given a list of VERTICES and a NEIGHBOR-FN function, returns two
functions: one that gives the in degree of a vertex and another that
gives the out degree of a vertex."
  (let ((in-degree (make-hash-table))
        (out-degree (make-hash-table)))
    (flet ((+in-degree (v n)
             (setf (gethash v in-degree)
                   (+ n (gethash v in-degree))))
           (+out-degree (v n)
             (setf (gethash v out-degree)
                   (+ n (gethash v out-degree)))))
      (dolist (v vertices)
        (setf (gethash v in-degree) 0)
        (setf (gethash v out-degree) 0))
      (dolist (v vertices)
        (let ((neighbors (funcall neighbors-fn v)))
          (+out-degree v (length neighbors))
	  (when neighbors
	    (dolist (n neighbors)
	      (+in-degree n 1)))))
      (values (lambda (n)
                (ensure-gethash n in-degree 0))
              (lambda (n)
                (ensure-gethash n out-degree 0))))))
