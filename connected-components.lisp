(in-package #:graph-algorithms)

(defun connected-components (vertices neighbors-fn visitor-fn)
  "VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each representative vertex of found
components."
  (let ((discovered (make-hash-table)))
    (dolist (id vertices)
      (unless (gethash id discovered)
        (setf (gethash id discovered) t)
        (funcall visitor-fn id)
        (breadth-first-search id neighbors-fn
             (lambda (n)
               (setf (gethash n discovered) t)))))))

(defun strongly-connected-components (vertices neighbors-fn visitor-fn)
  "Tarjan's algorithm. Performs a single pass of depth first
search. It maintains a stack of vertices that have been explored by
the search but not yet assigned to a component, and calculates low
numbers of each vertex (an index number of the highest ancestor
reachable in one step from a descendant of the vertex) which it uses
to determine when a set of vertices should be popped off the stack
into a new component.

VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each SCC found.

This implementation is a naive translation of the pseudocode in
Wikipedia, not really optimized for any particular workflow."
  (let ((index 0)
        (stack nil)
        (on-stack (make-hash-table))
        (indices (make-hash-table))
        (low-links (make-hash-table)))
    (labels ((set-low-link (v n) (setf (gethash v low-links) n))
             (get-low-link (v) (gethash v low-links))
             (set-index (v n) (setf (gethash v indices) n))
             (get-index (v) (gethash v indices))
             (set-on-stack (v n) (setf (gethash v on-stack) n))
             (get-on-stack (v) (gethash v on-stack))
             (strong-connect (v)
               (set-index v index)
               (set-low-link v index)
               (incf index)
               (push v stack)
               (set-on-stack v t)

               ;; consider sucessors of v
               (dolist (w (funcall neighbors-fn v))
                 (if (not (get-index w))
                     ;; sucessor w has not yet been visited; recurse on it
                     (progn
                       (strong-connect w)
                       (set-low-link v (min (get-low-link v) (get-low-link w))))

                     (if (get-on-stack w)
                         ;; sucessor w is in stack and hence in the current
                         ;; SCC
                         (set-low-link v (min (get-low-link v) (get-index w))))))

               ;; if v is a root node, pop the stack and generate a SCC
               (when (= (get-low-link v) (get-index v))
                 (let ((w nil)
                       (connected-component nil))
                   (loop until (eql v w)
                        do
                        (setf w (pop stack))
                        (set-on-stack w nil)
                        (push w connected-component))

                   ;; emit the SCC
                   (funcall visitor-fn connected-component)))))
      (dolist (v vertices)
        (when (not (get-index v))
          (strong-connect v))))))
