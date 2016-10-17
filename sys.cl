
(in-package :user)

(defvar *fasls* nil)

(load "bittorrent/load.cl")

(dolist (file '("defs.cl"
		"rssreader.cl"
		"utils.cl"
		"transmission.cl"
		"tcleanup.cl"
		"tget.cl"))
  (let ((f (compile-file file)))
    (push f *fasls*)
    (load f)))

(setq *fasls* (nreverse *fasls*))

(load (compile-file "t-tget.cl"))
