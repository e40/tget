
(setq *debug* t)
(setq *verbose* t)
(setq *learn* t)

(setq *config-file* (merge-pathnames "config.cl" *load-pathname*))

(dolist (file '("rssreader.cl" "tget.cl"))
  (load (compile-file file)))

(format t "*load-pathname* = ~s~%" *load-pathname*)

(defvar *old-database-name*)
(setq *old-database-name* *database-name*)
(setq *database-name* (merge-pathnames "test.db" *load-pathname*))

(defun copydb ()
  (let ((old-dir (pathname-as-directory *old-database-name*))
	(new-dir (pathname-as-directory *database-name*)))
    (when (probe-file new-dir)
      (delete-directory-and-files new-dir :quiet nil))
    (copy-directory old-dir new-dir :quiet nil))
  
  (opendb))

(defun opendb (&key reset)
  (open-tget-database :if-exists (if* reset
				    then :supersede
				    else :open))
  (load *config-file* :verbose nil :print nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging

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
    (setq *feed-interval* 180)
    (load "config.cl")
    (process-groups)
    ))


(pprint
 `(progn
    (copydb)
    ;;(opendb :reset nil)
    (load *config-file*)
    (process-groups)
    ))
