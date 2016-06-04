
(in-package :user)

(eval-when (compile eval load)
  (require :anydate)
  (require :tester)
  (use-package :util.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We do this globally so that any Lisp with the test suite loaded never
;; messes with the production database.
(let ((p (merge-pathnames "tmp/")))
  (ensure-directories-exist p)
  (setq *tget-data-directory* p))

(setq *test* t) ;; make sure we don't fetch external feeds

(defun reset-and-open-test-database ()
  (reset-program-state)
  
  (open-tget-database :if-exists :supersede)
  ;; Define this before loading the config file, so we don't rely on that
  ;; value.  The test suite requires the value `6'.
  (pushnew :debug *features* :test #'eq)
  (let ((*features* *features*))
    (push :testing *features*)
    (load "tget-config/config.cl" :verbose t)))

(defvar *debug-feed* nil)

(defun debug-feed (tracker)
  (declare (ignore tracker))
  (or *debug-feed*
      (error "*debug-feed* is nil")))

(defmacro with-tget-tests (options &body body)
  (declare (ignore options))
  `(let ((*learn* t) ;; don't download anything
	 (*log-stream* nil) ;; messages to stdout
	 (*log-file* "tmp/ep.log")
	 (*auto-backup* nil))
     (open-log-files :truncate t)
     ,@body))

(defun test-tget ()
;;;; Test individual components, ones that can be tested without invoking
;;;; the main processing engine.
  (test-tget-date-parser)
  (test-tget-episode-parser)
  (test-tget-epnum-comparisions)
  (test-tget-fuzzy-matching)
  (test-tget-feed-reading)
  (test-tget-feed-bugs)
  (test-tget-complete-to)
;;;; This is the meat of the engine and most of it used to be tested by
;;;; test.sh.  Now in Lisp and it does a lot more.
  (test-tget-processing)
  (+ util.test:*test-errors* util.test:*test-unexpected-failures*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-tget-date-parser ()
  (test (* 5 3600)
	(- (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0500")
	   (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0000")))
  (test (* 7 3600)
	(- (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 +0000")
	   (parse-rss20-date "Sun, 06 Nov 1994 08:49:37 -0700"))))

(defun test-tget-episode-parser ()
  (macrolet ((test-values (values string epr ja)
	       `(do* ((ref ,values (cdr ref))
		      (vals (multiple-value-list
			     (parse-name-season-and-episode
			      ,string
			      :episode-required ,epr
			      :junk-allowed ,ja))
			    (cdr vals)))
		    ((null ref)
		     (when vals (error "extra values: ~s." vals)))
		  (test (car ref)
			(car vals)
			:test #'equal
			:fail-info (list ,values ,string)))))
    ;;                                                  ep? junk?
    (test-values '("Foo" 3 4)       "Foo.S03E04.mp4"     t   t  )
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04.E05.mp4" t   t  )
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04.E05.mp4" nil t  )
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04E05.mp4"  t   t  )
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04E05.mp4"  nil t  )
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04.E05"     t   nil)
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04.E05"     nil nil)
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04E05"      t   nil)
    (test-values '("Foo" 3 (4 . 5)) "Foo.S03E04E05"      nil nil)
    (test-values '("Foo" 3 4)       "Foo.S03E04"         t   t  )
    (test-values '("Foo" 2015 2)    "Foo.S2015E02"       t   nil)
    (test-values '("Foo" 2015 2)    "Foo.S2015E02.mp4"   t   t  )
    
    (test-values '("Foo" 3 4 t)     "Foo.3x04.mp4"       t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.3x04"           t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.3x04"           t   nil)
    (test-values '("Foo" 3 4 t)     "Foo.304.mp4"        t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.304.720p.mp4"   t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.304"            t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.304"            t   nil)
    (test-values '("Foo" 3 4 t)     "Foo.0304.mp4"       t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.0304.720p.mp4"  t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.0304"           t   t  )
    (test-values '("Foo" 3 4 t)     "Foo.0304"           t   nil)
    (test-values '(nil)             "Foo.0S03.mp4"       t   t  )
    (test-values '(nil)             "Foo.S03E04.mp4"     t   nil)
    (test-values '("Foo" 3 4)       "Foo S03E04.mp4"     t   t  )
    (test-values '("Foo" 3 4)       "Foo S03E04"         t   t  )
    (test-values '("Foo" 3 4)       "Foo S03E04"         t   nil)
    (test-values '("Foo" 3 4)       "Foo.S03E04"         t   nil)
    (test-values '(nil)             "Foo.S03E04.mp4"     t   nil)
    (test-values '("Foo" 3 nil)     "Foo S03.mp4"        nil t  )
    (test-values '("Foo" 3 nil)     "Foo S03"            nil nil)
    (test-values '(nil)             "Foo S03"            t   nil)
    (test-values '("Foo")           "Foo.720p.mp4"       nil t  )
    (test-values '(nil)             "Foo.720p.mp4"       t   t  )
    (test-values '(nil)             "Foo.720p.mp4"       nil nil)
    (test-values '(nil)             "Foo.720p.mp4"       t   nil)
    (test-values '("Foo.2015" 1 nil)"Foo.2015.S01.mp4"   nil t  )

    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar.2014.01.02.mp4" nil t)
    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar 2014.01.02"     nil nil)
    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar 2014.01.02"     nil t)
    (test-values '(nil)              "The.Bar 2014.01"            nil nil)
    (test-values '("The.Bar" 20 14 t) "The.Bar 2014"              nil nil)
    (test-values '("The.Bar" 20 14 t) "The.Bar 2014"              nil t)
    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar.2014x01.02.mp4" nil t)
    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar 2014x01.02"     nil nil)
    (test-values '("The.Bar" 2014 2
		   nil "2014" "01" "02") "The.Bar 2014x01.02"     nil t)
    (test-values '(nil)              "The.Bar 2014x01"            nil nil)
    
    (test-values '("UFC.112" 2015 18 nil "2015" "01" "18")
		 "UFC.112.Jan.18th.2015.HDTV.x264-Sir.Paul.mp4"
		 nil t)
    (test-values '("UFC.112.Prelims" 2015 18 nil "2015" "01" "18")
		 "UFC.112.Prelims.Jan.18th.2015.HDTV.x264-Sir.Paul.mp4"
		 nil t)
    (test-values '("UFC.FN.59" 2015 18 nil "2015" "01" "18")
		 "UFC.FN.59.Jan.18th.2015.HDTV.x264-Sir.Paul.mp4"
		 nil t)
    (test-values '("UFC.FN.59.Prelims" 2015 18 nil "2015" "01" "18")
		 "UFC.FN.59.Prelims.Jan.18th.2015.HDTV.x264-Sir.Paul.mp4"
		 nil t)
    (test-values '("Frontline.US" 2015 2)
		 "Frontline.US.S2015E02.Putins.Way.720p.HDTV.x264-TOPKEK.mkv"
		 t t)
    (test-values '("Frontline.US" 2015 2)
		 "Frontline.US.S2015E02.Putins.Way.720p.HDTV.x264-TOPKEK.mkv"
		 nil t)
    
    ;; Crikey, when will they stop making up new formats!!!!!!
    (test-values '("parks.and.recreation" 7 (3 . 4) t)
		 "parks.and.recreation.70304.hdtv-lol.mp4"
		 t t)
    (test-values '("parks.and.recreation" 7 (3 . 4) t)
		 "parks.and.recreation.70304.hdtv-lol.mp4"
		 nil t)
    
    (test-values '("at.midnight" 2015 19 t "2015" "01" "19")
		 "at.midnight.150119-yestv.mp4"
		 t t)
    
    (test-values '("at.midnight" 2015 19 t "2015" "01" "19")
		 "at.midnight.150119-yestv.mp4"
		 nil t)
    
    (test-values
     '("UFC.FN.59.McGregor.vs.Siver.Early.Prelims")
     "UFC.FN.59.McGregor.vs.Siver.Early.Prelims.720p.WEB.DL.x264.mp4"
     nil t)
    
    ;; this present a challenge! ;-)
    #|
    "UFC.112.McGregor.vs.Siver.Early.Prelims.720p.WEB.DL.x264.mp4"
    |#
    ))
 
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

(defun test-tget-feed-reading ()
  (with-tget-tests ()
    (dolist (feed '("tget-test-data/btn.xml"
		    "tget-test-data/eztv.xml"
		    "tget-test-data/test001.xml"
		    "tget-test-data/freshon.xml"))
      (reset-and-open-test-database)
      (format t "~%~%;;;;; PARSE FEED: ~a~%~%" feed)
      (mapcar #'rss-to-episode (feed-to-rss-objects :file feed)))))

(defun test-tget-feed-bugs ()
  ;; malformed data
  (with-tget-tests ()
    (reset-and-open-test-database)
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
      (test 11 (episode-episode e)))
    
    
    (let ((e
	   (rss-to-episode
	    (make-rss-item
	     :source :broadcasthe.net
	     :title "Black Mirror - S03E01 [ 2014 ] [ MKV | x264 | HDTV | 720p | Scene ] [ Uploader: Anonymous ]  [ Black.Mirror.S02.Special.White.Chirstmas.720p.HDTV.x264-TLA ] "
	     :link "https://broadcasthe.net/..."
	     :guid nil
	     :comments ", Drama, SciFi, , fantasy, sciencefiction, "
	     :pub-date "Wed, 17 Dec 2014 00:32:18 +0000"
	     :description "Episode Name: White Christmas<br />Season: 3<br />Episode: 1<br />Aired: 2014-12-16<br /><br />Episode Overview:<br />This feature-length special consists of three interwoven stories. In a mysterious and remote snowy outpost, Matt and Potter share a Christmas meal, swapping creepy tales of their earlier lives in the outside world. Matt is a charismatic American trying to bring the reserved, secretive Potter out of his shell. But are both men who they appear to be?<br /><br />Episode Image <br />[img=http://thetvdb.com/banners/episodes/253463/5057304.jpg]"
	     :type nil
	     :length nil
	     :fileName nil))))
      (test t (episode-p e))
      (test "black mirror" (episode-series-name e) :test #'string=)
      (test 3 (episode-season e))
      (test 1 (episode-episode e)))
    
    ;; Bad description, make sure title is correctly parsed
    (let ((e
	   (rss-to-episode
	    (make-rss-item
	     :source :broadcasthe.net
             :title "The Daily Show with Jon Stewart - 2014.12.18 [ 2014 ] [ MKV | x264 | HDTV | 720p | Scene | FastTorrent ] [ Uploader: Mako ]  [ The.Daily.Show.2014.12.18.Chris.Rock.720p.HDTV.x264-BATV ] "
             :link "https://broadcasthe.net/torrents.php?action=download&authkey=c435c406028ce61eed547c44198ee2f6&torrent_pass=xxxxxxxxxx&id=461312"
             :guid nil
             :comments ", Comedy, talkshow, news, "
             :pub-date "Fri, 19 Dec 2014 06:05:27 +0000"
             :description "Episode Name: <br />Season: <br />Episode: <br />Aired: 2014.12.18<br /><br />Episode Overview:"
             :type nil
             :length nil
             :fileName nil)
	    )))
      (test t (episode-p e))
      (test "the daily show with jon stewart"
	    (episode-series-name e) :test #'string=)
      (test 2014 (episode-season e))
      (test  352 (episode-episode e)))
    ))

(defun test-tget-complete-to ()
  (reset-and-open-test-database)
  
  ;; Test catching up a date-based series
  (let ((s (query-series-name-to-series "the daily show")))
    (catch-up-series "the daily show 2015.01.15")
    (test '(2015 . 15) (series-complete-to s) :test #'equal))
  
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
			:date-based nil
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-eps ()
  (let ((n 0))
    (doclass (ep (find-class 'episode) :db *main*)
      (declare (ignore ep))
      (incf n))
    n))

(defun all-episodes ()
  (let ((res '()))
    (doclass (ep (find-class 'episode) :db *main*)
      (push (append (list (episode-series-name ep)
			  (episode-season ep)
			  (episode-episode ep))
		    (when (episode-repack ep)
		      (list :repack)))
	    res))
    (sort res #'episode-sort-function)))

(defun episode-sort-function (e1 e2)
  (let ((e1-epnum (if* (consp (third e1))
		     then (car (third e1))
		     else (third e1)))
	(e2-epnum (if* (consp (third e2))
		     then (car (third e2))
		     else (third e2))))
    (if* (string= (first e1) (first e2))
       then (if* (= (second e1) (second e2))
	       then (if* (= e1-epnum e2-epnum)
		       then e1
		       else (< e1-epnum e2-epnum))
	       else (< (second e1) (second e2)))
       else (string< (first e1) (first e2)))))

(defvar *initial-base-episodes* nil)
(defvar *base-episodes* nil)
(defvar *base-series* nil)

(defun note-episode (ep)
  (setq *base-episodes*
    (sort (cons ep *base-episodes*) #'episode-sort-function)))

(defun check-episodes (base)
  (format t ";; checking episodes...~%")

  ;; does an equal on what's in the database compared to BASE, but it makes
  ;; it easier to know what's different.
  
  (do* ((base base (cdr base))
	(ep (all-episodes) (cdr ep))
	mismatch
	(n 0))
      ((or mismatch
	   (null base)
	   (null ep))
       (if* mismatch
	  then (error "mismatch after ~d episodes; expected ~s, got ~s"
		      n
		      (car base)
		      (car ep))
	elseif base
	  then (error "~d left over episodes in base: ~s" (length base) base)
	elseif ep
	  then (error "~d extra episodes in db: ~s" (length ep) ep))
       t)
    (when (not (equal (car base) (car ep)))
      (error "check-episodes[~d]: expected ~s got ~s."
	     n (car base) (car ep)))
    (incf n)))

(defun check-series (base)
  (format t ";; checking series...~%")
  (dolist (name base)
    (test t (not (null (query-series-name-to-series name))))))

(defun pprint-episodes (eps)
  (dolist (ep eps)
    (format t "~s~%" ep)))

(setq *initial-base-episodes*
  '(("8 out of 10 cats" 15 4)
    ("8 out of 10 cats" 15 5)
    ("8 out of 10 cats" 15 6)
    ("8 out of 10 cats" 15 7)
    ("8 out of 10 cats" 15 10)
    ("bates motel" 1 6)
    ("childrens hospital (us)" 5 1)
    ("drunk history" 1 1)
    ("drunk history" 1 2)
    ("eagleheart" 2 11)
    ("elementary" 1 8)
    ("elementary" 1 9)
    ("elementary" 1 10)
    ("elementary" 1 11)
    ("elementary" 1 12)
    ("elementary" 1 13)
    ("elementary" 1 14)
    ("elementary" 1 15)
    ("elementary" 1 16)
    ("elementary" 1 17)
    ("elementary" 1 18)
    ("elementary" 1 19)
    ("elementary" 1 21)
    ("elementary" 1 (23 . 24))
    ("frontline (us)" 24 11)
    ("frontline (us)" 29 14)
    ("frontline (us)" 30 21)
    ("frontline (us)" 31 1)
    ("frontline (us)" 31 2)
    ("frontline (us)" 31 3)
    ("frontline (us)" 31 4)
    ("frontline (us)" 31 5)
    ("frontline (us)" 31 6)
    ("frontline (us)" 31 7)
    ("frontline (us)" 31 8)
    ("frontline (us)" 31 9)
    ("frontline (us)" 31 10)
    ("frontline (us)" 31 11)
    ("frontline (us)" 31 12)
    ("frontline (us)" 31 13)
    ("hannibal" 1 1)
    ("hannibal" 1 2)
    ("hannibal" 1 3)
    ("hannibal" 1 5)
    ("hannibal" 1 6)
    ("hannibal" 1 7)
    ("hannibal" 1 8)
    ("hannibal" 1 9)
    ("hannibal" 1 10)
    ("hannibal" 1 11)
    ("hannibal" 1 12)
    ("hannibal" 1 13)
    ("justified" 4 7)
    ("justified" 4 8)
    ("justified" 4 9)
    ("justified" 4 10)
    ("justified" 4 12)
    ("justified" 4 13)
    ("longmire" 2 1)
    ("longmire" 2 2)
    ("longmire" 2 3)
    ("longmire" 2 4)
    ("longmire" 2 5)
    ("longmire" 2 6 :repack)
    ("longmire" 2 7)
    ("longmire" 2 8)
    ("luther" 3 1)
    ("luther" 3 2)
    ("luther" 3 3)
    ("luther" 3 4)
    ("mad men" 6 (1 . 2) :repack)
    ("mad men" 6 3 :repack)
    ("mad men" 6 4 :repack)
    ("mad men" 6 5)
    ("mad men" 6 6)
    ("midsomer murders" 15 6)
    ("nova" 40 8)
    ("nova" 40 9)
    ("nova" 40 10)
    ("nova" 40 11)
    ("nova" 40 12)
    ("nova" 40 13)
    ("nova" 40 14)
    ("nova" 40 15)
    ("nova" 40 16)
    ("nova" 40 17)
    ("nova" 40 18)
    ("nova" 40 19)
    ("nova" 40 20)
    ("parks and recreation" 5 12)
    ("parks and recreation" 5 13)
    ("parks and recreation" 5 14)
    ("parks and recreation" 5 15)
    ("ray donovan" 1 1)
    ("ray donovan" 1 2 :repack)
    ("ray donovan" 1 3)
    ("ray donovan" 1 4)
    ("regular show" 4 1)
    ("regular show" 4 2)
    ("regular show" 4 3)
    ("regular show" 4 4)
    ("regular show" 4 7)
    ("regular show" 4 8)
    ("regular show" 4 9)
    ("regular show" 4 10)
    ("regular show" 4 11)
    ("regular show" 4 12)
    ("regular show" 4 13)
    ("regular show" 4 14)
    ("regular show" 4 15)
    ("regular show" 4 16)
    ("regular show" 4 17)
    ("regular show" 4 18)
    ("regular show" 4 20)
    ("regular show" 4 21)
    ("regular show" 4 22)
    ("regular show" 4 23)
    ("regular show" 4 24)
    ("regular show" 4 25)
    ("regular show" 4 26)
    ("regular show" 4 27)
    ("regular show" 4 28)
    ("regular show" 4 29)
    ("ridiculousness" 3 1)
    ("ridiculousness" 3 2)
    ("ridiculousness" 3 3)
    ("ridiculousness" 3 4)
    ("ridiculousness" 3 5)
    ("ridiculousness" 3 6)
    ("ridiculousness" 3 7)
    ("ridiculousness" 3 8)
    ("ridiculousness" 3 9)
    ("shark tank" 4 14)
    ("shark tank" 4 16)
    ("shark tank" 4 17)
    ("shark tank" 4 21)
    ("shark tank" 4 22)
    ("shark tank" 4 23)
    ("shark tank" 4 24)
    ("shark tank" 4 25)
    ("shark tank" 4 26)
    ("the daily show with jon stewart" 2013 35)
    ("the daily show with jon stewart" 2013 36)
    ("the daily show with jon stewart" 2013 37)
    ("the daily show with jon stewart" 2013 38)
    ("the daily show with jon stewart" 2013 42)
    ("the daily show with jon stewart" 2013 43)
    ("the daily show with jon stewart" 2013 44)
    ("the daily show with jon stewart" 2013 45)
    ("the daily show with jon stewart" 2013 50)
    ("the daily show with jon stewart" 2013 51 :repack)
    ("the daily show with jon stewart" 2013 52)
    ("the daily show with jon stewart" 2013 56)
    ("the daily show with jon stewart" 2013 57)
    ("the daily show with jon stewart" 2013 58)
    ("the daily show with jon stewart" 2013 59)
    ("the daily show with jon stewart" 2013 133)
    ("the daily show with jon stewart" 2013 161)
    ("the daily show with jon stewart" 2013 162)
    ("the daily show with jon stewart" 2013 168)
    ("the daily show with jon stewart" 2013 196)
    ("the daily show with jon stewart" 2013 197)
    ("the daily show with jon stewart" 2013 198)
    ("the daily show with jon stewart" 2013 199)
    ("the daily show with jon stewart" 2013 203)
    ("the daily show with jon stewart" 2013 205)
    ("the daily show with jon stewart" 2013 206)
    ("the good wife" 4 13)
    ("the good wife" 4 14)
    ("the good wife" 4 15)
    ("the good wife" 4 16)
    ("the good wife" 4 17)
    ("the good wife" 4 18)
    ("the good wife" 4 19)
    ("the good wife" 4 20)
    ("the good wife" 4 21)
    ("the graham norton show" 12 7)
    ("the graham norton show" 12 15)
    ("the graham norton show" 12 16)
    ("the graham norton show" 12 17)
    ("the graham norton show" 12 18)
    ("the graham norton show" 12 19)
    ("the graham norton show" 13 1)
    ("the graham norton show" 13 2)
    ("the graham norton show" 13 3)
    ("the graham norton show" 13 4)
    ("the graham norton show" 13 5)
    ("the graham norton show" 13 6)
    ("the graham norton show" 13 7)
    ("the graham norton show" 13 8)
    ("the graham norton show" 13 9)
    ("the graham norton show" 13 10)
    ("the graham norton show" 13 11)
    ("the graham norton show" 13 12)
    ("the graham norton show" 13 13)
    ("the newsroom (2012)" 2 1)
    ("the newsroom (2012)" 2 2)
    ("the simpsons" 24 1)
    ("the simpsons" 24 3)
    ("the simpsons" 24 19)
    ("the ultimate fighter" 17 2)
    ("the ultimate fighter" 17 3)
    ("the ultimate fighter" 17 4)
    ("the ultimate fighter" 17 5)
    ("the ultimate fighter" 17 6)
    ("the ultimate fighter" 17 7)
    ("the ultimate fighter" 17 8 :repack)
    ("the ultimate fighter" 17 9)
    ("the ultimate fighter" 17 10)
    ("the ultimate fighter" 17 11)
    ("the ultimate fighter" 17 12)
    ("the ultimate fighter" 17 13)
    ("top gear" 18 8)
    ("top gear" 18 9)
    ("top gear" 18 10)
    ("top gear" 18 11)
    ("top gear" 20 1)
    ("top gear" 20 2)
    ("top gear" 20 3)
    ("top gear" 20 4)
    ("top of the lake" 1 1)
    ("top of the lake" 1 2)
    ("top of the lake" 1 3)
    ("top of the lake" 1 4)
    ("top of the lake" 1 5)
    ("top of the lake" 1 6)
    ("top of the lake" 1 7)
    ("tosh 0" 5 1)
    ("tosh 0" 5 2)
    ("tosh 0" 5 3)
    ("tosh 0" 5 4)
    ("tosh 0" 5 5)
    ("tosh 0" 5 6)
    ("tosh 0" 5 7 :repack)
    ("tosh 0" 5 8)
    ("tosh 0" 5 9)
    ("tosh 0" 5 10)
    ("tosh 0" 5 11)
    ("tosh 0" 5 12)
    ("tosh 0" 5 13)
    ("tosh 0" 5 14)
    ("tosh 0" 5 15)
    ("white collar" 4 12 :repack)
    ("white collar" 4 13)
    ("white collar" 4 14)
    ))

(setq *base-series*
  '("8 out of 10 cats"
    "bates motel"
    "childrens hospital (us)"
    "eagleheart"
    "elementary"
    "frontline (us)"
    "hannibal"
    "justified"
    "longmire"
    "luther"
    "mad men"
    "midsomer murders"
    "nova"
    "parks and recreation"
    "ray donovan"
    "regular show"
    "ridiculousness"
    "shark tank"
    "the daily show with jon stewart"
    "the good wife"
    "the graham norton show"
    "the newsroom (2012)"
    "the simpsons"
    "the ultimate fighter"
    "top gear"
    "top of the lake"
    "tosh 0"))

(defun test-tget-processing (&aux (*debug* 
				   ;; `t' is too verbose
				   nil)
				  ep
				  (neps (length *initial-base-episodes*)))
  (with-tget-tests ()
    ;; First thing to do is populate the database with a known set of
    ;; things, then we run further tests that should add episodes, or not,
    ;; depending on the conditions.
    ;;
    ;; We used to only read the (old) TVT feed and process episodes from
    ;; that, using the current time.  We still do that, for lack of a
    ;; better resource.  The data file for this is
    ;; tget-test-data/test001.xml.

    (reset-and-open-test-database)
    
    (setq *base-episodes* (copy-list *initial-base-episodes*))
    
    ;; make sure it's really empty:
    (test 0 (count-eps) :test #'=
	  :fail-info "#1")
    
;;;;;;;;;;;
    (format t ";;;;; test 1~%")
    (setq *now*
      (excl:string-to-universal-time
       ;; hours avail max is 11 with this value:
       "Fri, 26 Jul 2013 23:34:38 +0000"))
    (setq *debug-feed* "tget-test-data/test001.xml")
    (time (process-groups))

    (test t (check-episodes *base-episodes*))
    (test neps (count-eps) :test #'=
	  :fail-info "#2")
    (check-series *base-series*)

;;;;;;;;;;; 
    (format t ";;;;; test 2: wait longer for :high~%")
    (setq *debug-feed* "tget-test-data/test002.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 05:59:00 +0000"))
    (process-groups)
    (test neps (count-eps) :test #'=
	  :fail-info "#3")

;;;;;;;;;;;
    (format t ";;;;; test 3: grab :high~%")
    (setq *debug-feed* "tget-test-data/test003.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 06:00:10 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#4")
    (note-episode '("vikings" 1 1))

;;;;;;;;;;; 
    (format t ";;;;; test 4: already have ep~%")
    (setq *debug-feed* "tget-test-data/test004.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 06:30:00 +0000"))
    (process-groups)
    (test neps (count-eps) :test #'=
	  :fail-info "#5")

;;;;;;;;;;; 
    (format t ";;;;; test 5: get :normal not :high~%")
    (setq *debug-feed* "tget-test-data/test005.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:00:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#6")
    (note-episode '("vikings" 1 2))
    (setq ep (query-episode :series-name "vikings" :season 1 :ep-number 2))
    (test 1 (length ep))
    (setq ep (car ep))
    (test :normal (episode-quality ep))

;;;;;;;;;;; 
    (format t ";;;;; test 5b: do NOT download :high REPACK~%")
    (setq *debug-feed* "tget-test-data/test005b.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:30:00 +0000"))
    (process-groups)
    (test neps (count-eps) :test #'=
	  :fail-info "#7")

;;;;;;;;;;; 
    (format t ";;;;; test 6: Get repack over non-repack ep~%")
    (setq *debug-feed* "tget-test-data/test006.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:00:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#8")
    (note-episode '("vikings" 1 3))
    (setq ep (query-episode :series-name "vikings" :season 1 :ep-number 3))
    (test 1 (length ep))
    (setq ep (car ep))
    (test :normal (episode-quality ep))
    (test t (episode-repack ep))
    
;;;;;;;;;;;
    (format t ";;;;; test 7: grab ep~%")
    (setq *debug-feed* "tget-test-data/test007.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:30:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#9")
    (note-episode '("vikings" 1 4))

;;;;;;;;;;;
    (format t ";;;;; test 7a: download the :normal repack~%")
    (setq *debug-feed* "tget-test-data/test007a.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:45:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#10")
    (note-episode '("vikings" 1 4))
    (setq ep (query-episode :series-name "vikings" :season 1 :ep-number 4
			    :repack t))
    (test 1 (length ep))
    (setq ep (car ep))
    (test :normal (episode-quality ep))

;;;;;;;;;;;
    (format t ";;;;; test 8: grab ep~%")
    (setq *debug-feed* "tget-test-data/test008.xml")
    (setq *now*
      (excl:string-to-universal-time "Sat, 27 Jul 2013 07:55:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#11")
    (note-episode '("vikings" 1 5))

;;;;;;;;;;;
    (format t ";;;;; test 8a: do NOT download any repack~%")
    (setq *debug-feed* "tget-test-data/test008a.xml")
    (setq *now*
      (excl:string-to-universal-time "Sun, 28 Jul 2013 20:00:00 +0000"))
    (process-groups)
    (test neps (count-eps) :test #'=
	  :fail-info "#12")

;;;;;;;;;;;
    (format t ";;;;; test 9: grab REPACK~%")
    (setq *debug-feed* "tget-test-data/test009.xml")
    (setq *now*
      (excl:string-to-universal-time "Sun, 28 Jul 2013 20:00:00 +0000"))
    (process-groups)
    (incf neps)
    (test neps (count-eps) :test #'=
	  :fail-info "#13")
    (note-episode '("vikings" 1 (6 . 7)))
    (setq ep (query-episode :series-name "vikings" :season 1
			    :ep-number '(6 . 7) :repack t))
    (test 1 (length ep))

;;;;;;;;;;;
    (format t ";;;;; test 10: do NOT download 1080p or 1080i~%")
    (setq *debug-feed* "tget-test-data/test010.xml")
    (setq *now*
      (excl:string-to-universal-time "Sun, 28 Jul 2013 20:00:00 +0000"))
    (process-groups)
    (test neps (count-eps) :test #'=
	  :fail-info "#14")
    
    ))
