(in-package #:graph-algorithms)

(defun breadth-first-search (source neighbors-fn visitor-fn
            &key (queue-depth 500000))
  "Performs a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search. QUEUE-DEPH is the maximum
queue depth used by CL-SPEEDY-QUEUE."
  (let ((Q (make-queue queue-depth))
        (discovered (make-hash-table)))
    (enqueue source Q)
    (setf (gethash source discovered) t)
    (loop until (queue-empty-p Q)
       do (let ((p (dequeue Q)))
            (funcall visitor-fn p)
            (dolist (v (funcall neighbors-fn p))
              (unless (gethash v discovered)
                (setf (gethash v discovered) t)
                (enqueue v Q)))))))
