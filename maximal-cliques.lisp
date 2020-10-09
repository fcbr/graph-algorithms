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
