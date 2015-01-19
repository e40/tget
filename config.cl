;; Config file for tget

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General options

;; This is now reset once per run, so it's not too much data to save,
;; and it's good for debugging feed issues.
(setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))

;; A good resource to see why something is or isn't downloading
(setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))

;; Use transmission-remote to tell your torrent client to download the
;; episode:
(set-torrent-handler
 (make-transmission-remote-handler
  :host (sys:getenv "TRANSMISSION_HOST")
  :port (sys:getenv "TRANSMISSION_PORT")
  :username (sys:getenv "TRANSMISSION_USER")
  :password (sys:getenv "TRANSMISSION_PASS")
  :ssh-user (sys:getenv "TRANSMISSION_SSH_USER")
  :ssh-identity (sys:getenv "TRANSMISSION_SSH_IDENTITY")
  :add-paused nil
  :trash-torrent-file t
  ;; The default when there is tracker defined ratio
  :ratio 1.1))

;; An alternate method for downloading .torrent files: put them into a
;; specific directory, where your torrent client will see them.
#+ignore
(set-torrent-handler (pathname "~/Downloads/"))

(setq *download-root* "/me/layer/videos/")

(defvar *codec-x264*
    ;; It goes by two different names:
    '(:x264 :h.264))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trackers

(deftracker :eztv
    :url "https://ezrss.it/..."
    :debug-feed :eztv
    :public t
    :download-delay #-debug 1 #+debug 0
    :disabled t ;; RSS is still offline 
    :ratio 1.0)

(deftracker :freshon
    :url "https://freshon.tv/..."
    :debug-feed :freshon
    :download-delay #-debug 1 #+debug 0
    :ratio 1.3)

(deftracker :btn
    :url "https://broadcasthe.net/..."
    :debug-feed :btn
    :disabled #-debug nil #+debug t
    :download-delay #-debug 6 #+debug 0
    :ratio 1.5)

(defvar *trackers*
    (list :freshon :eztv :btn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quality settings

(defquality :high-1080p
    :priority -1
    :codec *codec-x264* 
    :resolution :1080p)

(defquality :high-1080i
    :priority -1
    :codec *codec-x264* 
    :resolution :1080i)

(defquality :normal
    ;; The priority of a quality allows selection of episodes when more
    ;; than one quality is available at the same time, as is often the
    ;; case.  Higher numerical priority is given precedence.
    :priority 50
    :source :hdtv
    :codec *codec-x264* 
    :resolution :sd)

(defquality :high
    :priority 40
    :codec *codec-x264* 
    :resolution :720p)

(defquality :low
    :priority 30
    :source :hdtv
    :codec :xvid
    :resolution :sd)

(defquality :btn
    :priority 60
    :container :mp4
    :source :hdtv
    :codec *codec-x264*)

;; The additional delay waiting to download a high quality ep while
;; waiting for a normal quality one to become available
(defvar *download-hq-delay* #-debug 1 #+debug 5)

;; The additional delay waiting to download a low quality ep while
;; waiting for a normal or high quality one to become available
(defvar *download-lq-delay* #-debug 24 #+debug 24)

;; While I'm tuning the new delays, let's be verbose:
(pushnew :tget-config-debug *features*)

(defun my-quality (episode &aux (tracker (episode-tracker episode))
				temp)
  (flet
      ((pending-msg (temp)
	 (when temp
	   ;; we would have downloaded this if enough time had passed, so
	   ;; let's say that
	   (format t "Will download episode in ~d more hours:~%   ~a~%"
		   episode (- temp (hours-available episode))))))
    ;; My defined quality, as a function.  This allows me to download
    ;; different qualities based on different criteria.
    ;;
    ;; We also output progress reports for episodes not downloaded,
    ;; so we can monitor progress, for debugging purposes.
    (when (and (null
		;; See if there is an episode with :normal quality.  The
		;; :transient keyword is important, since it restricts the
		;; search to episodes we have *not* downloaded yet.
		(query-episode :episode episode :quality :normal :transient t))
	       (eq :high (episode-quality episode))
	       (and tracker
		    (numberp (tracker-download-delay tracker))
		    (if* (>= (hours-available episode)
			     (setq temp
			       (+ (tracker-download-delay tracker)
				  *download-hq-delay*)))
		       thenret
		       else (@log ">>>waiting for ~a more hours for this HQ ep"
				  (- temp (hours-available episode)))
			    nil)))
      ;; :normal quality is not available and the :high quality episode
      ;; has been available for a set amount of hours, then take this
      ;; one
      (return-from my-quality :high))
  
    #+tget-config-debug (pending-msg temp)
  
    (when (=~ "broadcasthe.net" (episode-torrent-url episode))
      (return-from my-quality :btn))

    (setq temp nil)
    (when (and (null
		;; See if there is an episode with :normal or :high
		;; quality.
		(or
		 (query-episode :episode episode :quality :normal :transient t)
		 (query-episode :episode episode :quality :high :transient t)))
	       (eq :low (episode-quality episode))
	       (and tracker
		    (>= (hours-available episode)
			(setq temp
			  (+ (tracker-download-delay tracker)
			     *download-lq-delay*)))))
      (return-from my-quality :low))
    
    #+tget-config-debug (pending-msg temp)
  
    :normal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups

(defgroup :adrian
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian/" *download-root*))

(defgroup :anh
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defgroup :kevin
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defgroup :adrian+kevin
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defgroup :anh+kevin
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "anh+kevin/" *download-root*))

;;;;TODO: seems like this shouldn't be here... figure out a way to get
;;;;      around it.  See --add command line argument.
(defgroup :manual
    :rss-url nil
    :delay 0
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TV shows

;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
;; Use ... :remove t ... to delete a series
;; Use ... :subdir "dirname" ... to put the episodes into a subdirectory of
;;          the group download path -- this is a hack to make Plex Media
;;          Server see episodes of The Daily Show and The Colbert Report.

(defseries "8 Out of 10 Cats" :kevin)
(defseries "8 Out of 10 Cats Does Countdown" :kevin)
(defseries "An Idiot Abroad" :adrian+kevin)
(defseries "Archer (2009)" :kevin)
(defseries "At Midnight" :kevin :date-based t :subdir "At.Midnight"
	   :aliases ("@midnight"))
(defseries "Bates Motel" :anh+kevin)
(defseries "Bear Grylls: Escape From Hell" :kevin)
(defseries "Black Mirror" :kevin)
(defseries "Brooklyn Nine-Nine" :kevin)
(defseries "Childrens Hospital (US)" :kevin)
(defseries "Community" :adrian+kevin)
(defseries "Curb your Enthusiasm" :anh+kevin)
(defseries "Doc Martin" :anh+kevin)
(defseries "Downton Abbey" :anh)
(defseries "Dragons Den (UK)" :kevin)
(defseries "Drunk History" :kevin :catch-up "S02")
(defseries "Eagleheart" :adrian+kevin)
(defseries "Elementary" :kevin)
(defseries "Fargo" :kevin)
(defseries "Frontline (US)" :kevin)
(defseries "Game of Thrones" :kevin :private t :delay 0) ;; immediate download
(defseries "Hannibal" :kevin :delay 0) ;; immediate download
(defseries "Hell on Wheels" :kevin)
(defseries "Homeland" :kevin :private t :delay 0) ;; immediate download
(defseries "Intruders" :kevin :catch-up "S01E02")
(defseries "James May's Man Lab" :adrian+kevin)
(defseries "Justified" :kevin)
(defseries "Last Week Tonight with John Oliver" :kevin :private t
  :subdir "Last.Week.Tonight.With.John.Oliver")
(defseries "Longmire" :kevin)
(defseries "Louis Theroux Documentaries" :kevin)
(defseries "Louie" :kevin)
(defseries "Luther" :kevin)		;gone forever??
(defseries "Mad Men" :kevin)
(defseries "Midsomer Murders" :anh)
(defseries "Modern Family" :adrian+kevin)
(defseries "Naked and Afraid" :kevin :catch-up "S01")
(defseries "Nathan for You" :adrian+kevin)
(defseries "Nova" :kevin)
(defseries "Parks and Recreation" :adrian+kevin)
(defseries "Person of Interest" :kevin)
(defseries "Running Wild with Bear Grylls" :kevin :catch-up "S01E04")
(defseries "Ray Donovan" :kevin :private t)
(defseries "Regular Show" :adrian+kevin)
(defseries "Rick and Morty" :adrian+kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Sirens (2014)" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Colbert Report" :kevin :subdir "The.Colbert.Report"
	   :date-based t :remove t)
(defseries "The Daily Show with Jon Stewart" :kevin :subdir "The.Daily.Show"
	   :date-based t
	   :aliases ("The Daily Show"))
(defseries "The Good Wife" :anh+kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The Meltdown with Jonah and Kumail" :kevin :catch-up "S01E04")
(defseries "The Mentalist" :adrian+kevin)
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Newsroom (2012)" :kevin :private t)
(defseries "The Simpsons" :adrian+kevin)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
(defseries "Top Gear" :adrian+kevin :quality :high)
(defseries "Top of the Lake" :anh+kevin)
(defseries "Tosh.0" :kevin)
(defseries "True Detective" :kevin :private t)
(defseries "Vikings" :kevin)
(defseries "Wallander" :anh+kevin)
(defseries "White Collar" :anh+kevin :remove t)
(defseries "Witness (2012)" :kevin :private t)
(defseries "World's Craziest Fools" :adrian+kevin)
(defseries "Would I Lie To You?" :kevin :catch-up "S08E01"
	   :aliases ("Would I Lie To You"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These items are for the test suite only, and are not used in production
;; mode:

