
(in-package :user)

(eval-when (compile eval load)
  (require :tester)
  (use-package :util.test))

(defun test-quality (res)
  `(:container :mp4 :source :hdtv :codec :x264 :resolution ,res))

(defmacro with-tget-tests (options &body body)
  (declare (ignore options))
  `(let ((*learn* t)
	 (*log-stream* nil) ;; messages to stdout
	 (*auto-backup* nil))
     ,@body))

(defun test-tget ()
  (test-tget-feed-reading)
  (test-tget-processing)
  (+ util.test:*test-errors* util.test:*test-unexpected-failures*))

(defun test-db-init ()
  (open-tget-database :if-exists :supersede)
  (setq *transmission-remote* nil)
  (load "tget-config/config.cl" :verbose t))

(defun test-tget-feed-reading ()
  (with-tget-tests ()
    (dolist (feed '("tget-test-data/btn.xml"
		    "tget-test-data/tvt-recent.xml"
		    "tget-test-data/tvt.xml"))
      (test-db-init)
      (format t "~%~%;;;;; PARSE FEED: ~a~%~%" feed)
      (mapcar #'rss-to-episode (feed-to-rss-objects :file feed)))))

(defun test-tget-processing (&aux downloaded-episodes)
  (with-tget-tests ()
    (labels
	((select-episode (ep)
	   (format t "select=~a~%" ep)
	   (push ep downloaded-episodes))
	 (make-eps (&rest things)
	   (test-db-init)
	   (dolist (thing things)
	     (destructuring-bind (filename &key hours transient)
		 thing
	       (multiple-value-bind (series-name season episode repack
				     container source codec resolution)
		   (extract-episode-info-from-filename filename)
		 (let ((title (format nil "~a S~aE~a" series-name
				      season episode))
		       (pub-date
			;; `hours' is the hours back from now
			(- (get-universal-time)
			   ;; seconds in the past
			   (floor (* hours 3600)))))
		   (make-episode :series-name series-name
				 :full-title title
				 :title title
				 :torrent-url title
				 :pub-date pub-date
				 :season season
				 :episode episode
				 :repack repack
				 :container container
				 :source source
				 :codec codec
				 :resolution resolution
				 :filename filename
				 :transient transient)))))
	   (commit)
	   (setq downloaded-episodes nil)
	   (process-transient-objects (retrieve-from-index 'group
							   'name :kevin)
				      #'select-episode)))

;;;; should wait longer for :sd ep
      (make-eps
       '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 5.5 :transient t)
       '("vikings.s01e01.720p.hdtv.x264-2hd.mkv" :hours 6.5 :transient t))
      (test 0 (length downloaded-episodes)
	    :fail-info "test 1")

;;;; should download the :sd ep
      (make-eps
       '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 6.5 :transient t)
       '("vikings.s01e01.720p.hdtv.x264-2hd.mkv" :hours 7.5 :transient t))
      (when (test 1 (length downloaded-episodes)
		  :fail-info "test 2.1")
	(test :sd (episode-resolution (car downloaded-episodes))
	      :fail-info "test 2.2"))
      
;;;; should download the repack
      (make-eps
       '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
       '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 6.5 :transient t))
      (when (test 1 (length downloaded-episodes)
		  :fail-info "test 3.1")
	(test t (episode-repack (car downloaded-episodes))
	      :fail-info "test 3.2"))

;;;; should NOT download the repack
      (make-eps
       '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
       '("vikings.s01e01.REPACK.720p.hdtv.x264-2hd.mkv" :hours 7.5
	 :transient t))
      (test 0 (length downloaded-episodes)
	    :fail-info "test 4")

;;;; should download the repack
      (make-eps
       '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
       '("vikings.s01e01.REPACK.720p.hdtv.x264-2hd.mkv" :hours 7.5
	 :transient t)
       '("vikings.s01e01.REPACK.hdtv.x264-2hd.mp4" :hours 6.5
	 :transient t))
      (when (test 1 (length downloaded-episodes)
		  :fail-info "test 5")
	(test t (episode-repack (car downloaded-episodes))
	      :fail-info "test 5.2")
	(test :sd (episode-resolution (car downloaded-episodes))
	      :fail-info "test 5.3"))

      )))

