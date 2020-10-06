(require :sb-cover)

;;; Turn on generation of code coverage instrumentation in the compiler
(declaim (optimize sb-cover:store-coverage-data))

(asdf:oos 'asdf:load-op :graph-algorithms :force t)
(asdf:oos 'asdf:load-op :graph-algorithms/tests :force t)

(require :graph-algorithms/tests)

(graph-algorithms/tests::test-graph-algorithms)

(sb-cover:report "/tmp/")
