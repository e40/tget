
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
  (test-tget-date-parser)
  (test-tget-epnum-comparisions)
  (test-tget-fuzzy-matching)
  (test-tget-feed-reading)
  (test-tget-feed-bugs)
  (test-tget-complete-to)
  (test-tget-processing)
  (+ util.test:*test-errors* util.test:*test-unexpected-failures*))

(defun test-tget-date-parser ()
  (test (* 5 3600)
	(- (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0500")
	   (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0000")))
  (test (* 7 3600)
	(- (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0000")
	   (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 -0700"))))

(defun test-tget-epnum-comparisions ()
  (test t   (epnum< 1 2))
  (test nil (epnum< 1 1))
  (test t   (epnum< 1 10))
  (test t   (epnum< 1 '(2 . 3)))
  (test t   (epnum< '(1 . 2) 3))
  (test nil (epnum< '(2 . 3) 1))
  (test nil (epnum< 3 '(1 . 2))))

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
		  ("the daily show with jon stewart"
		   "the daily show"
		   "the daily show with jon stewart")
		  ("ntsf:sd:suv" "ntsf sd suv" "ntsf:sd:suv")
		  ("ntsf sd suv" "ntsf:sd:suv" "ntsf:sd:suv")
		  )))
    (dolist (thing things)
      (test (third thing)
	    (fuzzy-compare-series-names (first thing) (second thing))
	    :test #'string=))))

(defun test-db-init ()
  (open-tget-database :if-exists :supersede)
  (setq *torrent-handler* nil)
  (load "tget-config/config.cl" :verbose t))

(defun test-tget-feed-reading ()
  (with-tget-tests ()
    (dolist (feed '("tget-test-data/btn.xml"
		    "tget-test-data/eztv.xml"
		    "tget-test-data/tvt.xml"))
      (test-db-init)
      (format t "~%~%;;;;; PARSE FEED: ~a~%~%" feed)
      (mapcar #'rss-to-episode (feed-to-rss-objects :file feed)))))

(defun test-tget-feed-bugs ()
  ;; malformed data
  (with-tget-tests ()
    (test-db-init)
    (let ((e (rss-to-episode
	      (make-rss-item
	       :source :tvtorrents.com
	 
	       :title "The Daily Show with Jon Stewart - 2013x19.12 - Jonah Hill (.mp4)"
	       :link "http://foo.bar.com/blahblah"
	       :guid nil
	       :comments "foo bar"
	       :pub-date "Tue, 31 Dec 2013 00:25:15 +0000"
	 
	       :description
	       "Show Name:The Daily Show with Jon Stewart; Show Title: Jonah Hill (.mp4); Season: 2013; Episode: 19.12; Filename: The.Daily.Show.2013.19.12.Jonah.Hill.HDTV.x264-2HD.mp4;"
	       :type "application/x-bittorrent"
	       :length "417684110"))))
      (test t (episode-p e))
      (test 2013 (episode-season e))
      (test 353 (episode-episode e)))
    
    (let ((e (rss-to-episode
	      (make-rss-item
	       :source :tvtorrents.com
	 
	       :title "Vikings - 4x00.11 - Foo the Bar (.mp4)"
	       :link "http://foo.bar.com/blahblah"
	       :guid nil
	       :comments "foo the bar"
	       :pub-date "Tue, 31 Dec 2013 00:25:15 +0000"
	 
	       :description
	       "Show Name:Vikings; Show Title: Foo the Bar (.mp4); Season: 4; Episode: 00.11; Filename: Vikings.S04.Foo.the.Bar.HDTV.x264-2HD.mp4;"
	       :type "application/x-bittorrent"
	       :length "417684110"))))
      (test t (episode-p e))
      (test 4 (episode-season e))
      (test 11 (episode-episode e)))))

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
	      :test #'equal :fail-info "test 1a")
	(test nil (series-discontinuous-episodes series)
	       :fail-info "test 1b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 3)
	(test '(1 . 3) (series-complete-to series)
	      :test #'equal :fail-info "test 2a")
	(test nil (series-discontinuous-episodes series) :fail-info "test 2b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 1)
	(test '(1 . 2) (series-complete-to series)
	      :test #'equal :fail-info "test 3a")
	(test nil (series-discontinuous-episodes series) :fail-info "test 3b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes nil)
	(update-complete-to series 1 4)
	(test '(1 . 2) (series-complete-to series)
	      :test #'equal :fail-info "test 4a")
	(test '((1 . 4)) (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 4b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 4)))
	(update-complete-to series 1 3)
	(test '(1 . 4) (series-complete-to series)
	      :test #'equal :fail-info "test 5a")
	(test nil (series-discontinuous-episodes series) :fail-info "test 5b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 4) (1 . 5)
					      (1 . 7) (1 . 8)))
	(update-complete-to series 1 3)
	(test '(1 . 5) (series-complete-to series)
	      :test #'equal :fail-info "test 6a")
	(test '((1 . 7) (1 . 8)) (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 6b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 2)
		    :discontinuous-episodes '((1 . 6)))
	(update-complete-to series 1 4)
	(test '(1 . 4) (series-complete-to series)
	      :test #'equal :fail-info "test 7a")
	(test '((1 . 6)) (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 7b"))
      
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 10)
		    :discontinuous-episodes nil)
	(update-complete-to series 2 1)
	(test '(2 . 1) (series-complete-to series)
	      :test #'equal :fail-info "test 8a")
	(test nil (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 8b"))
      
      ;; This should never happen, in practice, but you never know:
      (with-series (series
		    :name "vikings"
		    :complete-to '(1 . 10)
		    :discontinuous-episodes nil)
	(update-complete-to series 2 2)
	(test '(1 . 10) (series-complete-to series)
	      :test #'equal :fail-info "test 9a")
	(test '((2 . 2)) (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 9b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(28 . 11)
		    :discontinuous-episodes '((28 . 14)))
	(update-complete-to series 28 11)
	(test '(28 . 11) (series-complete-to series) :test #'equal
	      :fail-info "test 10a")
	(test '((28 . 14)) (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 10b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(28 . 11)
		    :discontinuous-episodes '((28 . 14)))
	(update-complete-to series 28 9999)
	(test '(28 . 9999) (series-complete-to series) :test #'equal
	      :fail-info "test 11a")
	(test nil (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 11b"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(28 . 11)
		    :discontinuous-episodes '((28 . 14) (28 . 16) (28 . 20)))
	(update-complete-to series 28 9999)
	(test '(28 . 9999) (series-complete-to series) :test #'equal
	      :fail-info "test 11c")
	(test nil (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 11d"))
      
      (with-series (series
		    :name "vikings"
		    :complete-to '(28 . 11)
		    :discontinuous-episodes '((28 . 14) (28 . 16) (28 . 20)
					      (29 . 1)))
	(update-complete-to series 28 9999)
	(test '(29 . 1) (series-complete-to series) :test #'equal
	      :fail-info "test 12a")
	(test nil (series-discontinuous-episodes series)
	      :test #'equal :fail-info "test 12b"))
      )))

(defvar *test-downloaded-episodes* nil)

(defun test-select-episode (ep)
  (format t "select=~a~%" ep)
  (push ep *test-downloaded-episodes*))

(defun test-make-eps (&rest things)
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
	   :pretty-epnum
	   (season-and-episode-to-pretty-epnum season episode)
	   :repack repack
	   :container container
	   :source source
	   :codec codec
	   :resolution resolution
	   :filename filename
	   :transient transient)))))
  (tget-commit *main*)
  (tget-commit *temp*)
  (setq *test-downloaded-episodes* nil)
  (process-transient-objects (retrieve-from-index 'group 'name :kevin
						  :db *main*)
			     #'test-select-episode))

(defun test-tget-processing (&aux *test-downloaded-episodes*)
  (with-tget-tests ()
;;;; weird naming:
    (test-make-eps
     '("Vikings.S22E11E12.720p.HDTV.X264-DIMENSION.mkv" :hours 1
       :transient t))
    (test 0 (length *test-downloaded-episodes*)
	  :fail-info "test 0")

;;;; should wait longer for :sd ep
    (test-make-eps
     '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 5.5 :transient t)
     '("vikings.s01e01.720p.hdtv.x264-2hd.mkv" :hours 6.5 :transient t))
    (test 0 (length *test-downloaded-episodes*)
	  :fail-info "test 1")

;;;; should download the :sd ep
    (test-make-eps
     '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 6.5 :transient t)
     '("vikings.s01e01.720p.hdtv.x264-2hd.mkv" :hours 7.5 :transient t))
    (when (test 1 (length *test-downloaded-episodes*)
		:fail-info "test 2.1")
      (test :sd (episode-resolution (car *test-downloaded-episodes*))
	    :fail-info "test 2.2"))
      
;;;; should download the repack
    (test-make-eps
     '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
     '("vikings.s01e01.repack.hdtv.x264-2hd.mp4" :hours 6.5 :transient t))
    (when (test 1 (length *test-downloaded-episodes*)
		:fail-info "test 3.1")
      (test t (episode-repack (car *test-downloaded-episodes*))
	    :fail-info "test 3.2"))

;;;; should NOT download the repack
    (test-make-eps
     '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
     '("vikings.s01e01.REPACK.720p.hdtv.x264-2hd.mkv" :hours 7.5
       :transient t))
    (test 0 (length *test-downloaded-episodes*)
	  :fail-info "test 4")

;;;; should download the repack
    (test-make-eps
     '("vikings.s01e01.hdtv.x264-2hd.mp4" :hours 10 :transient nil)
     '("vikings.s01e01.REPACK.720p.hdtv.x264-2hd.mkv" :hours 7.5
       :transient t)
     '("vikings.s01e01.REPACK.hdtv.x264-2hd.mp4" :hours 6.5
       :transient t))
    (when (test 1 (length *test-downloaded-episodes*)
		:fail-info "test 5")
      (test t (episode-repack (car *test-downloaded-episodes*))
	    :fail-info "test 5.2")
      (test :sd (episode-resolution (car *test-downloaded-episodes*))
	    :fail-info "test 5.3"))

    (dolist (name '("Mad.Men.S06E01-E02.PROPER.HDTV.x264-2HD.mp4"
		    "Mad.Men.S06E01E02.PROPER.HDTV.x264-2HD.mp4"
		    "mad.men.s06e01e02.repack.hdtv.x264-2hd.mp4"))
      (test-make-eps (list name :hours 7 :transient t))
      (when (test 1 (length *test-downloaded-episodes*)
		  :fail-info "test 6.1")
	(let ((ep (car *test-downloaded-episodes*)))
	  (test t (episode-repack ep)
		:fail-info "test 6.2")
	  (test :sd (episode-resolution ep)
		:fail-info "test 6.3")
	  (test "S06E01-E02" (episode-pretty-epnum ep)
		:test #'string=
		:fail-info "test 6.4")
	  (test '(6 . 2) (series-complete-to (episode-series ep))
		:test #'equal
		:fail-info "test 6.5"))))
    ))
