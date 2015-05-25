# graph-algorithms

Several graph algorithms in Common Lisp.  Still very much in development.

<p><br>[Function]<br><b>bfs</b> <i>source neighbors-fn visitor-fn <tt>&amp;key</tt> test queue-depth</i> =&gt; <i>result</i>
<blockquote>
Performs a breadth-first-search on the graph.  SOURCE is the vertex
used as the start of the search.  NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
on each new vertex found by the search.  TEST is used to compare
vertices and QUEUE-DEPH is the maximum queue depth used by
CL-SPEEDY-QUEUE.
</blockquote>

<p><br>[Function]<br><b>connected-components</b> <i>vertices neighbors-fn visitor-fn <tt>&amp;key</tt> test</i> =&gt; <i>result</i>
<blockquote>
VERTICES is the list of vertices of the graph. NEIGHBORS-FN should
return a list of immediate neighbor vertices of a given vertex.
VISITOR-FN is called once for each representative vertex of found
components.
</blockquote>

<p><br>[Function]<br><b>strongly-connected-components</b> <i>vertices neighbors-fn visitor-fn <tt>&amp;key</tt> test</i> =&gt; <i>result</i>
<blockquote>
Tarjan's strongly connected components algorithm.  VERTICES is the
list of vertices of the graph. NEIGHBORS-FN should return a list of
immediate neighbor vertices of a given vertex.  VISITOR-FN is called
once for each SCC found.
</blockquote>
