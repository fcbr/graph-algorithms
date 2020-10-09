;;;; graph-algorithms.lisp

(in-package #:graph-algorithms)

(defun maximal-cliques (vertices neighbors-fn visitor-fn)
  "Implementation of the Bron–Kerbosch algorithm for finding maximal
  cliques in an undirected graph, without pivoting."
  (bron-kerbosch nil vertices nil neighbors-fn visitor-fn))

(defun bron-kerbosch (R P X neighbors-fn visitor-fn)
  "The basic form of the Bron–Kerbosch algorithm is a recursive
backtracking algorithm that searches for all maximal cliques in a
given graph G. More generally, given three disjoint sets of vertices
R, P, and X, it finds the maximal cliques that include all of the
vertices in R, some of the vertices in P, and none of the vertices in
X. In each call to the algorithm, P and X are disjoint sets whose
union consists of those vertices that form cliques when added to R. In
other words, P ∪ X is the set of vertices which are joined to every
element of R. When P and X are both empty there are no further
elements that can be added to R, so R is a maximal clique and the
algorithm outputs R."
  (when (and (emptyp P) (emptyp X))
    (funcall visitor-fn R))
  (dolist (v P)
    (let ((nv (funcall neighbors-fn v)))
      (bron-kerbosch
       (union R (list v))
       (intersection P nv)
       (intersection X nv)
       neighbors-fn visitor-fn)
      (removef P v)
      (push v X))))

(defun bfs (source neighbors-fn visitor-fn
            &key (queue-depth 500000))
  "Performs a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search. QUEUE-DEPH is the maximum
queue depth used by CL-SPEEDY-QUEUE."
  (let ((q (make-queue queue-depth))
        (discovered (make-hash-table)))
    (enqueue source q)
    (setf (gethash source discovered) t)
    (loop until (queue-empty-p q)
       do (let ((p (dequeue q)))
            (funcall visitor-fn p)
            (dolist (v (funcall neighbors-fn p))
              (unless (gethash v discovered)
                (setf (gethash v discovered) t)
                (enqueue v q)))))))

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
        (bfs id neighbors-fn
             (lambda (n)
               (setf (gethash n discovered) t)))))))

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

(defun dijkstra (source vertices neighbors-fn)
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

(defun strongly-connected-components (vertices neighbors-fn visitor-fn)
  "Tarjan's strongly connected components algorithm, published by
Robert Tarjan in 1972,[3] performs a single pass of depth first
search. It maintains a stack of vertices that have been explored by
the search but not yet assigned to a component, and calculates low
numbers of each vertex (an index number of the highest ancestor
reachable in one step from a descendant of the vertex) which it uses
to determine when a set of vertices should be popped off the stack
into a new component.

VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each SCC found."
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

               ;; if v is a root node, pop the stack and generate an SCC
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

(defun transitive-closure (vertices relation-fn)
  "Gets the transitive closure of RELATION-FN over VERTICES."
  (let ((closure vertices)
        (tmp nil))
    (loop 
       (setf tmp (remove-duplicates 
                  (append closure
                          (flatten (mapcar relation-fn closure)))))
       (when (= (length tmp) (length closure))
	 (return))
       (setf closure tmp))
    (remove-duplicates closure)))
