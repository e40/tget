
(load "bittorrent/bittorrent_full.fasl")
(load (compile-file "utils.cl"))
(load (compile-file "transmission.cl"))
(load (compile-file "tcleanup.cl"))
(load "tget-config/tcleanup-config.cl")

(format t "~
;; Do this:
:pa tcleanup
(tcleanup-transmission)
(tcleanup-files)
")
