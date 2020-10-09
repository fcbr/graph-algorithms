(in-package #:graph-algorithms)

(defun shortest-paths (source vertices neighbors-fn)
  "Dijkstra's shortest path algorithm.  All reachable vertices from
SOURCE are computed.  Returns DIST and PREV hash tables.  As in the
other methods, NEIGHBORS-FN is a function that receives a vertex and
returns its neighbors as a list of vertices.  Note that this
implementation does not consider weighted edges yet."
  (let* ((dist (make-hash-table))
         (prev (make-hash-table))
         (nodes (make-hash-table))
         (Q (make-instance 'pairing-heap)))
    (flet ((set-dist (v n) (setf (gethash v dist) n))
           (get-dist (v) (gethash v dist))
           (set-node (v n) (setf (gethash v nodes) n))
           (get-node (v) (gethash v nodes))
           (set-prev (v n) (setf (gethash v prev) n))
           (get-prev (v) (gethash v prev)))
      (set-dist source 0)
      (set-prev source nil)
      (dolist (v vertices)
        (when (not (eql v source))
          (set-dist v MOST-POSITIVE-FIXNUM)
          (set-prev v nil))
        (set-node v (insert Q (get-dist v) v)))
      (loop until (empty-p Q)
         do
           (let ((u (extract-min Q))
                 (alt 0))
             (dolist (v (funcall neighbors-fn u))
               (setf alt (+ (get-dist u) 1))
               (when (< alt (get-dist v))
                 (set-dist v alt)
                 (set-prev v u)
                 (decrease-key Q (get-node v) alt))))))
    (values prev dist)))

(defun reconstruct-path (prev target)
  "Given the PREV hash table returned by DIJKSTRA, reconstruct the
path from the original source vertex to TARGET."
  (let ((S nil)
        (u target))
    (loop while (gethash u prev)
       do
         (push u S)
         (setf u (gethash u prev)))
    S))
