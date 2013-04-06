
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
  (test-tget-fuzzy-matching)
  (test-tget-feed-reading)
  (test-tget-complete-to)
  (test-tget-processing)
  (+ util.test:*test-errors* util.test:*test-unexpected-failures*))

(defun test-tget-fuzzy-matching ()
  (let ((things '(("late night with jimmy fallon"
		   "jimmy fallon"
		   ;; result:
		   "late night with jimmy fallon")
		  ("archer 2009"
		   "archer (2009)"
		   "archer (2009)")
		  ("scandal us"
		   "scandal (us)"
		   "scandal (us)")
		  ("60 minutes"
		   "60 minutes (us)"
		   "60 minutes (us)")
		  ("nattpatruljen (norwegian)"
		   "nattpatruljen"
		   "nattpatruljen (norwegian)")
		  ("the tonight show with jay leno"
		   "jay leno"
		   "the tonight show with jay leno")
		  ("mike & molly"
		   "mike and molly"
		   "mike and molly")
		  ("ncis: los angeles"
		   "ncis los angeles"
		   "ncis: los angeles")
		  ("mr & mrs murder"
		   "mr and mrs murder"
		   "mr and mrs murder")
		  ("law and order: svu"
		   "law & order svu"
		   "law and order: svu")
		  ("dag (norwegian)"
		   "dag"
		   "dag (norwegian)")
		  ("the murdoch mysteries (2008)"
		   "murdoch mysteries"
		   "the murdoch mysteries (2008)")
		  ("pokemon"
		   "pokemon adventures in unova"
		   "pokemon adventures in unova")
		  ("antiques roadshow (uk)"
		   "antiques roadshow uk"
		   "antiques roadshow (uk)")
		  ("diners, drive-ins and dives"
		   "diners drive-ins & dives"
		   "diners, drive-ins and dives")
		  ("r u faster than a redneck?"
		   "r u faster than a redneck"
		   "r u faster than a redneck?")
		  ("foo" "foo" "foo")
		  ("the biggest loser (au)"
		   "the biggest loser australia"
		   "the biggest loser (au)")
		  )))
    (dolist (thing things)
      (test (third thing)
	    (fuzzy-compare-series-names (first thing) (second thing))
	    :test #'string=))))

(defun test-db-init ()
  (open-tget-database :if-exists :supersede)
  (setq *transmission-remote* nil)
  (load "tget-config/config.cl" :verbose t))

(defun test-tget-feed-reading ()
  (with-tget-tests ()
    (dolist (feed '("tget-test-data/btn.xml"
		    "tget-test-data/eztv.xml"
		    "tget-test-data/tvt-recent.xml"
		    "tget-test-data/tvt.xml"))
      (test-db-init)
      (format t "~%~%;;;;; PARSE FEED: ~a~%~%" feed)
      (mapcar #'rss-to-episode (feed-to-rss-objects :file feed)))))

(defun test-tget-complete-to ()
  (test-db-init)
  (with-tget-tests ()
    (macrolet
	((with-series ((var &key name complete-to discontinuous-episodes)
		       &body body)
	   (let ((g-name (gensym))
		 (g-c-t (gensym))
		 (g-d-e (gensym)))
	     `(let* ((,g-name ,name)
		     (,g-c-t ,complete-to)
		     (,g-d-e ,discontinuous-episodes)
		     (,var
		      (make-instance 'series 
			:name (gensym ,g-name)
			:complete-to ,g-c-t
			:discontinuous-episodes ,g-d-e)))
		(unwind-protect (progn ,@body)
		  (delete-instance ,var))))))

      (with-series (series
		    :name "vikings"
		    :complete-to nil
		    :discontinuous-episodes nil)
	(update-complete-to series 1 3)
	(test '(1 . 3) (series-complete-to series)
	      :test #'equal)
	(test nil (series-discontinuous-episodes series)))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 3)
	(test '(1 . 3) (series-complete-to series)
	      :test #'equal)
	(test nil (series-discontinuous-episodes series)))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 1)
	(test '(1 . 2) (series-complete-to series)
	      :test #'equal)
	(test nil (series-discontinuous-episodes series)))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 4)
	(test '(1 . 2) (series-complete-to series)
	      :test #'equal)
	(test '((1 . 4)) (series-discontinuous-episodes series)
	      :test #'equal))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 4)))
	(update-complete-to series 1 3)
	(test '(1 . 4) (series-complete-to series)
	      :test #'equal)
	(test nil (series-discontinuous-episodes series)))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 4) (1 . 5)
					      (1 . 7) (1 . 8)))
	(update-complete-to series 1 3)
	(test '(1 . 5) (series-complete-to series)
	      :test #'equal)
	(test '((1 . 7) (1 . 8)) (series-discontinuous-episodes series)
	      :test #'equal))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 6)))
	(update-complete-to series 1 4)
	(test '(1 . 2) (series-complete-to series)
	      :test #'equal)
	(test '((1 . 4) (1 . 6)) (series-discontinuous-episodes series)
	      :test #'equal))
      
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 10)
		    :discontinuous-episodes nil)
	(update-complete-to series 2 1)
	(test '(2 . 1) (series-complete-to series)
	      :test #'equal)
	(test nil (series-discontinuous-episodes series)
	      :test #'equal))
      
      ;; This should never happen, in practice, but you never know:
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 10)
		    :discontinuous-episodes nil)
	(update-complete-to series 2 2)
	(test '(1 . 10) (series-complete-to series)
	      :test #'equal)
	(test '((2 . 2)) (series-discontinuous-episodes series)
	      :test #'equal))
      
      )))

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
		   (make-episode
		    :series (or (query-series-name-to-series series-name)
				(error "no series??? ~s" series-name))
		    :series-name series-name
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

