
(setq *tget-data-directory* *default-pathname-defaults*)
(setq *database-name* (merge-pathnames "test.db" *load-pathname*))
(setq *version-file* (pathname (format nil "~a/version.cl" *database-name*)))
(setq *log-file* (merge-pathnames "ep.log" *load-pathname*))
(setq *config-file* (merge-pathnames "config.cl" *load-pathname*))
(setq *debug* t)
(setq *feed-interval* 180)
(setq *learn* t)
(setq *verbose* t)

(dolist (file '("rssreader.cl" "tget.cl"))
  (load (compile-file file)))

(defun opendb (&key reset)
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
    (setq rss-objects (feed-to-rss-objects :file "btn.xml"))
    (setq rss-objects (feed-to-rss-objects :file "eztv.xml"))
    (setq rss-objects
      #+ignore
      (feed-to-rss-objects
       :url "http://www.tvtorrents.com/mytaggedRSS?digest=0a8994688db11176c1a3ec73737ed3d0cfccf25a&hash=2ea04863f10de9ec35e9e9e15ebef3cb3bca793c&interval=30+days")
      (feed-to-rss-objects :file "tvt.xml"))

    (setq show-objects (mapcar #'rss-to-episode rss-objects))
    (commit)))

(pprint
 `(progn
    (opendb :reset t)
    (load *config-file*)
    (process-groups)))

