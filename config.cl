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
    ;; The usual place for EZTV is down (https://ezrss.it/feed/),
    ;; try this, which I got from this page:
    ;;   http://www.bt-chat.com/rsstag.php?
    :url "http://rss.bt-chat.com/..."
    :debug-feed :eztv
    :public t
    :download-delay 0
    :disabled nil
    :ratio 1.0)

(deftracker :freshon
    ;; The feed URL is
    ;;  https://freshon.tv/rss.php?feed=dl&passkey=<passkey>"
    ;; Get the <passkey> from your profile page.
    :url "https://freshon.tv/..."
    :debug-feed :freshon
    :download-delay 0
    ;; See if limiting the upload rate allows freshon to correctly count my
    ;; upload credits.  Sick of uploading lots and not getting credit for it.
    :upload-limit 100 ;; KB/s
    :ratio 1.3)

(deftracker :btn
    ;; RSS feed configured by creating a notification filter with these
    ;; options: 
    ;; - categories: episode
    ;; - containers: avi, mkv, mp4
    ;; - codecs: x264, h.264
    ;; - resolutions: sd, 720p
    ;; All other checkboxs were unchecked.
    ;;
    :url "https://broadcasthe.net/..."
    :debug-feed :btn
    :disabled #-debug nil #+debug t
    ;; Can't make the delay too long or shows will fall off the RSS feed
    :download-delay #-debug 1 #+debug 0
    :ratio 1.5)

(deftracker :shazbat
    :url "https://www.shazbat.tv/..."
    :debug-feed :shazbat
    :disabled t
    :download-delay 0
    :ratio 1.5)

(defvar *trackers*
    ;; move :freshon to last.  Their tracker is being very annoying lately
    ;; and not giving me upload credit.  Grrr.
    (list :btn :shazbat :eztv :freshon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quality settings

(defquality :high-1080p
    :priority -1			; never download
    :codec *codec-x264* 
    :resolution :1080p)

(defquality :high-1080i
    :priority -1			; never download
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
    :container '(:mkv :mp4)
    ;;:source :hdtv
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
      (#+tget-config-debug
       (debug-msg (temp)
	 (when temp
	   ;; we would have downloaded this if enough time had passed, so
	   ;; let's say that
	   (format t "~
DEBUG: (tracker delay + quality delay) - hours avail = ~d hours for:
       ~a~%"
		   (- temp (hours-available episode)) episode))))
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
  
    #+tget-config-debug (debug-msg temp)
  
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
    
    #+tget-config-debug (debug-msg temp)
  
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
(defseries "Better Call Saul" :adrian+kevin)
(defseries "Black Mirror" :adrian+kevin)
(defseries "Brooklyn Nine-Nine" :adrian+kevin)
(defseries "Childrens Hospital (US)" :adrian+kevin)
(defseries "Community" :adrian+kevin)
(defseries "Curb your Enthusiasm" :adrian+kevin)
(defseries "Doc Martin" :anh+kevin)
(defseries "Downton Abbey" :anh)
(defseries "Dragons Den (UK)" :kevin)
(defseries "Eagleheart" :adrian+kevin)
(defseries "Elementary" :kevin)
(defseries "Fargo" :kevin)
(defseries "Frontline (US)" :kevin)
(defseries "Game of Thrones" :kevin :private t :delay 0 :quality :high)
(defseries "Hannibal" :kevin :delay 0)
(defseries "Hell on Wheels" :kevin)
(defseries "Homeland" :kevin :private t :delay 0)
(defseries "Intruders" :kevin :catch-up "S01E02")
(defseries "James May's Man Lab" :adrian+kevin)
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
(defseries "Ray Donovan" :kevin :private t)
(defseries "Regular Show" :adrian+kevin)
(defseries "Rick and Morty" :adrian+kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Sirens (2014)" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Daily Show with Jon Stewart" :kevin :subdir "The.Daily.Show"
	   :date-based t
	   :aliases ("The Daily Show"))
(defseries "The Good Wife" :anh+kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The Last Man on Earth" :adrian+kevin :catch-up "S01E02")
(defseries "The Meltdown with Jonah and Kumail" :kevin :catch-up "S01E04")
(defseries "The Mentalist" :adrian+kevin)
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Nightly Show with Larry Wilmore" :kevin
  :aliases ("The Nightly Show")
  :subdir "The.Nightly.Show" :date-based t)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0)
(defseries "Top Gear" :adrian+kevin :quality :high)
(defseries "Tosh.0" :kevin)
(defseries "True Detective" :kevin :private t)
(defseries "Vikings" :kevin)
(defseries "Wallander" :anh+kevin)
(defseries "Witness (2012)" :kevin :private t)
(defseries "World's Craziest Fools" :adrian+kevin)
(defseries "Would I Lie To You?" :kevin :catch-up "S08E01"
	   :aliases ("Would I Lie To You"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These items are for the test suite only, and are not used in production
;; mode:

#+testing (defseries "The Newsroom (2012)" :kevin)
#+testing (defseries "Top of the Lake" :kevin)
#+testing (defseries "Parks and Recreation" :adrian+kevin)
#+testing (defseries "The Simpsons" :adrian+kevin)
#+testing (defseries "Drunk History" :kevin)
#+testing (defseries "White Collar" :anh+kevin)
#+testing (defseries "Justified" :kevin)
