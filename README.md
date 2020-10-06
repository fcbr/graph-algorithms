# Installation

This package uses ASD definition files. The easiest way to use this is via [Quicklisp](https://www.quicklisp.org/).

To use, clone this repository and link it from your local `~/quicklisp/local-projects/`, for example:

```
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/fcbr/graph-algorithms.git
[...]
$ sbcl
This is SBCL 2.0.9, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (require :graph-algorithms)
NIL
*
```

See the unit tests for sample usage of each of the defined methods.

# Testing

This package uses [FiveAM](https://github.com/lispci/fiveam/) for unit tests.

# Documentation

Graph algorithms in Common Lisp:

- Breadth first search
- Connected components
- Shortest paths
- Strongly connected components
- Maximal cliques
- Degrees of a vertex

```
(bfs source neighbors-fn visitor-fn)
```

Performs a breadth-first-search on the graph.  `SOURCE` is the vertex
used as the start of the search.  `NEIGHBORS-FN` should return a list of
immediate neighbor vertices of a given vertex.  `VISITOR-FN` is called
on each new vertex found by the search.

```
(connected-components vertices neighbors-fn visitor-fn)
```

`VERTICES` is the list of vertices of the graph. `NEIGHBORS-FN` should
return a list of immediate neighbor vertices of a given vertex.
`VISITOR-FN` is called once for each representative vertex of found
components.

```
(degrees vertices neighbors-fn)
```

Given a list of `VERTICES` and a `NEIGHBOR-FN` function, returns two
functions: one that gives the in degree of a vertex and another that
gives the out degree of a vertex.


```
(dijkstra source vertices neighbors-fn)
```

Dijkstra's shortest patha algorithm.  All reachable vertices from
`SOURCE` are computed.  Returns `DIST` and `PREV` hash tables.  As in the
other methods, `NEIGHBORS-FN` is a function that receives a vertex and
returns its neighbors as a list of vertices.  Note that this
implementation does not consider weighted edges yet.

```
(reconstruct-path prev target)
```

Given the PREV hash table returned by DIJKSTRA, reconstruct the
path from the original source vertex to TARGET.


```
(strongly-connected-components vertices neighbors-fn visitor-fn)
```

Tarjan's strongly connected components algorithm. `VERTICES` is
the list of vertices of the graph. `NEIGHBORS-FN` should return
a list of immediate neighbor vertices of a given vertex. `VISITOR-FN`
is called once for each SCC found.

```
(maximal-cliques vertices neighbors-fn visitor-fn)
```

Implementation of the Bron–Kerbosch algorithm for finding maximal
cliques in an undirected graph, without pivoting.
