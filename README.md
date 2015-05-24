# graph-algorithms
Several graph algorithms in Common Lisp.  Still very much in development.

<p><br>[Function]<br><b>bfs</b> <i>source neighbors-fn visitor-fn <tt>&amp;key</tt> test queue-depth</i> =&gt; <i>result</i>
<blockquote><br>

Perform a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search.  TEST is used to compare
vertices and QUEUE-DEPH is the maximum queue depth used by
CL-SPEEDY-QUEUE.

</blockquote>

<p><br>[Function]<br><b>connected-components</b> <i>vertices neighbors-fn visitor-fn <tt>&amp;key</tt> test</i> =&gt; <i>result</i>
<blockquote><br>

VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
  return a list of immediate neighbor vertices of a given vertex.
  VISITOR-FN is called once for each representative vertex of found
  components.

</blockquote>

<p><br>[Function]<br><b>strongly-connected-components</b> <i>vertices neighbors-fn visitor-fn <tt>&amp;key</tt> test</i> =&gt; <i>result</i>
<blockquote><br>

Tarjan&#039;s strongly connected components algorithm, published by
   Robert Tarjan in 1972,[3] performs a single pass of depth first
   search. It maintains a stack of vertices that have been explored by
   the search but not yet assigned to a component, and calculates low
   numbers of each vertex (an index number of the highest ancestor
   reachable in one step from a descendant of the vertex) which it
   uses to determine when a set of vertices should be popped off the
   stack into a new component.

   VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
   return a list of immediate neighbor vertices of a given vertex.
   VISITOR-FN is called once for each SCC found.

</blockquote>
