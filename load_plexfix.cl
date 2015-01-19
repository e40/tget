
(load (compile-file "utils.cl"))
(load (compile-file "plexfix.cl"))

(format t "~
;; Do this:
(plexfix \"...\" :no-execute t)
")
