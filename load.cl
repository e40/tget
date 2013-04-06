
(setq *tget-data-directory* *default-pathname-defaults*)
(setq *database-name* (merge-pathnames "test.db" *load-pathname*))
(setq *version-file* (pathname (format nil "~a/version.cl" *database-name*)))
(setq *log-file* (merge-pathnames "ep.log" *load-pathname*))
(setq *config-file* (merge-pathnames "tget-config/config.cl" *load-pathname*))
(setq *debug* t)
(setq *feed-interval* 180)
(setq *learn* t)
(setq *verbose* t)
(setq *auto-backup* nil)

(dolist (file '("rssreader.cl" "tget.cl" "t-tget.cl"))
  (load (compile-file file)))

(defun opendb (&key reset copy-db)
  (setq *transmission-remote* nil)
  (when db.allegrocache::*allegrocache*
    (close-tget-database))
  (when copy-db
    (when (string= (namestring copy-db) (namestring *database-name*))
      (error "They're the same database!"))
    (when (probe-file *database-name*)
      (delete-directory-and-files *database-name*))
    (copy-directory (pathname-as-directory copy-db)
		    (pathname-as-directory *database-name*)
		    :quiet nil))
  (open-tget-database :if-exists (if* reset
				    then :supersede
				    else :open))
  (load *config-file* :verbose nil :print nil))

(dolist (v '(*tget-version*
	     *schema-version*
	     *tget-data-directory*
	     *auto-backup*
	     *database-name*
	     *version-file*
	     *config-file*
	     *debug*
	     *feed-interval*
	     *log-rss*
	     *log-file*))
  (format t ";; ~a => ~s~%"
	  v (symbol-value v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging

(open-log-files :truncate t)

(pprint
 `(progn
    (opendb :reset t)
    (setq rss-objects (feed-to-rss-objects :file "tget-test-data/btn.xml"))
    (setq rss-objects (feed-to-rss-objects :file "tget-test-data/eztv.xml"))
    (setq rss-objects (feed-to-rss-objects :file "tget-test-data/tvt.xml"))
    (setq show-objects (mapcar #'rss-to-episode rss-objects))
    (commit)))

(pprint
 `(progn
    (opendb :reset t)
    (process-groups)))

;; test speed of processing episodes:
(pprint
 `(progn
    (opendb :copy-db "~/.tget.d/db")
    (prof:with-profiling () (process-groups))))
