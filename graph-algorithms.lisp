;;;; graph-algorithms.lisp

(in-package #:graph-algorithms)

(defun bfs (source neighbors-fn visitor-fn
            &key (test #'equal) (queue-depth 500000))
  "Perform a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search.  TEST is used to compare
vertices and QUEUE-DEPH is the maximum queue depth used by
CL-SPEEDY-QUEUE."
  (let ((q (make-queue queue-depth))
        (discovered (make-hash-table :test test)))
    (enqueue source q)
    (setf (gethash source discovered) 1)
    (loop until (queue-empty-p q)
       do (let ((p (dequeue q)))
            (funcall visitor-fn p)
            (dolist (v (funcall neighbors-fn p))
              (unless (gethash v discovered)
                (setf (gethash v discovered) 1)
                (enqueue v q)))))))

(defun connected-components (vertices neighbors-fn visitor-fn
                             &key (test #'equal))
  "VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
  return a list of immediate neighbor vertices of a given vertex.
  VISITOR-FN is called once for each representative vertex of found
  components."
  (let ((discovered (make-hash-table :test test)))
    (dolist (id vertices)
      (unless (gethash id discovered)
        (setf (gethash id discovered) 1)
        (funcall visitor-fn id)
        (bfs id neighbors-fn
             (lambda (n)
               (setf (gethash n discovered) 1)))))))

#|
1  function Dijkstra(Graph, source):
2      dist[source] ← 0                  // Initialization
3      for each vertex v in Graph:           
4          if v ≠ source
5              dist[v] ← infinity        // Unknown distance from source to v
6              prev[v] ← undefined       // Predecessor of v
7          end if
8          Q.add_with_priority(v, dist[v])
9      end for 
10
11     while Q is not empty:             // The main loop
12         u ← Q.extract_min()           // Remove and return best vertex
13         for each neighbor v of u:
14             alt = dist[u] + length(u, v) 
15             if alt < dist[v]
16                 dist[v] ← alt
17                 prev[v] ← u
18                 Q.decrease_priority(v, alt)
19             end if
20         end for
21     end while
21     return dist[], prev[]
|#

(defun dijkstra (source)
)  

(defun strongly-connected-components (vertices neighbors-fn visitor-fn &key (test #'equal))
  "Tarjan's strongly connected components algorithm, published by
   Robert Tarjan in 1972,[3] performs a single pass of depth first
   search. It maintains a stack of vertices that have been explored by
   the search but not yet assigned to a component, and calculates low
   numbers of each vertex (an index number of the highest ancestor
   reachable in one step from a descendant of the vertex) which it
   uses to determine when a set of vertices should be popped off the
   stack into a new component.

   VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
   return a list of immediate neighbor vertices of a given vertex.
   VISITOR-FN is called once for each SCC found."
  (let ((index 0)
        (stack nil)
        (on-stack (make-hash-table :test test))
        (indices (make-hash-table :test test))
        (low-links (make-hash-table :test test)))
    (labels ((low-link! (v n) (setf (gethash v low-links) n))
             (low-link? (v) (gethash v low-links))
             (index! (v n) (setf (gethash v indices) n))
             (index? (v) (gethash v indices))
             (on-stack! (v n) (setf (gethash v on-stack) n))
             (on-stack? (v) (gethash v on-stack))
             (strong-connect (v)
               (index! v index)
               (low-link! v index)
               (setf index (1+ index))
               (push v stack)
               (on-stack! v t)
               ;; consider sucessors of v
               (dolist (w (funcall neighbors-fn v))
                 (if (not (index? w))
                     ;; sucessor w has not yet been visited; recurse on it
                     (progn
                       (strong-connect w)
                       (low-link! v (min (low-link? v) (low-link? w))))
                     (if (on-stack? w)
                         ;; sucessor w is in stack and hence in the current
                         ;; SCC
                         (low-link! v (min (low-link? v) (index? w))))))

               ;; if v is a root node, pop the stack and generate an SCC
               (when (= (low-link? v) (index? v))
                 (let ((w nil)
                       (connected-component nil))
                   (loop until (funcall test v w)
                        do
                        (setf w (pop stack))
                        (on-stack! w nil)
                        (push w connected-component))
                   ;; emit the SCC
                   (funcall visitor-fn connected-component)))))
      (dolist (v vertices)
        (when (not (index? v))
          (strong-connect v))))))
