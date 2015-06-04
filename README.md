# graph-algorithms

Several graph algorithms in Common Lisp.  Still very much in development.

```Lisp
(defun bfs (source neighbors-fn visitor-fn
            &key (test #'equal) (queue-depth 500000))
  "Performs a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search.  TEST is used to compare
vertices and QUEUE-DEPH is the maximum queue depth used by
CL-SPEEDY-QUEUE.")
```

```Lisp
(defun connected-components (vertices neighbors-fn visitor-fn
                             &key (test #'equal))
  "VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each representative vertex of found
components.")
```

```Lisp
(defun degrees (vertices neighbors-fn &key (test #'equal))
  "Given a list of VERTICES and a NEIGHBOR-FN function, returns two
functions: one that gives the in degree of a vertex and another that
gives the out degree of a vertex.")
```

```Lisp
(defun dijkstra (source vertices neighbors-fn &key (test #'equal))
  "Dijkstra's shortest path algorithm, simple implementation.  All
reachable vertices from SOURCE are computed.  Returns DIST and PREV
hash tables.  This implementation does not consider weighted edges yet.")
```

```Lisp
(defun reconstruct-path (prev target)
  "Given the PREV hash table returned by DIJKSTRA, reconstruct the
path from the original source vertex to TARGET.")
```

```Lisp
(defun strongly-connected-components (vertices neighbors-fn visitor-fn
                                      &key (test #'equal))
  "Tarjan's strongly connected components algorithm. 
VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each SCC found.")
```