
(setq *tget-data-directory* *default-pathname-defaults*)
(setq *database-main-name* (merge-pathnames "main.db" *load-pathname*))
(setq *database-temp-name* (merge-pathnames "temp.db" *load-pathname*))
(setq *version-file* (pathname (format nil "~a/version.cl"
				       *database-main-name*)))
(setq *log-file* (merge-pathnames "ep.log" *load-pathname*))
(setq *config-file* (merge-pathnames "tget-config/config.cl" *load-pathname*))
(setq *debug* nil) ;; too verbose
(setq *feed-interval* 180)
(setq *learn* t)
(setq *verbose* 1)
(setq *auto-backup* nil)

(dolist (file '("rssreader.cl" "utils.cl" "tget.cl" "t-tget.cl"))
  (load (compile-file file)))

(defun opendb (&key reset copy-db compact)
  (setq *transmission-remote* nil)
  (when *main*
    (close-tget-database)
    (setq *torrent-handler* nil))
  (when copy-db
    (when (string= (namestring copy-db) (namestring *database-main-name*))
      (error "They're the same database!"))
    (when (probe-file *database-main-name*)
      (delete-directory-and-files *database-main-name*))
    (copy-directory (pathname-as-directory copy-db)
		    (pathname-as-directory *database-main-name*)
		    :quiet nil))
  (open-tget-database :compact compact
		      :if-exists (if* reset
				    then :supersede
				    else :open))
  (load *config-file* :verbose t))

(dolist (v '(*tget-version*
	     *schema-version*
	     *tget-data-directory*
	     *auto-backup*
	     *database-main-name*
	     *database-temp-name*
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
    (setq rss-objects (feed-to-rss-objects :file "tget-test-data/sporthd.xml"))
    ;;(setq rss-objects (feed-to-rss-objects :file "tget-test-data/btn.xml"))
    ;;(setq rss-objects (feed-to-rss-objects :file "tget-test-data/eztv.xml"))
    ;;(setq rss-objects (feed-to-rss-objects :file "tget-test-data/dtt.xml"))
    ;;(setq rss-objects (feed-to-rss-objects :file "tget-test-data/tvt.xml"))
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

(pprint `(test-tget))
(pprint `(test-tget-processing))
