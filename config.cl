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

;;;; Now using transmission in a container, with no ssh
  :docker-container-name "transmission"
  :docker-user (sys:getenv "USER")
;;;; 
  ;;:ssh-user (sys:getenv "TRANSMISSION_SSH_USER")
  ;;:ssh-identity (sys:getenv "TRANSMISSION_SSH_IDENTITY")

  :add-paused nil
  :trash-torrent-file t
  ;; The default when there is tracker defined ratio
  :ratio 1.1))

;; An alternate method for downloading .torrent files: put them into a
;; specific directory, where your torrent client will see them.
#+ignore
(set-torrent-handler (pathname "~/Downloads/"))

;; Same in and outside the transmission container
(setq *download-root* "/me/tplex/content/videos/")

(defvar *codec-x264*
    ;; It goes by two different names:
    '(:x264 :h.264))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trackers

#+testing
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

#+ignore
(deftracker :sporthd
    ;; RSS feed configured by creating a notification filter with these
    ;; options: category: NBA, direct download of torrent
    :url "http://sporthd.org/..."
    :disabled nil
    :download-delay 0
    :ratio 1.5)

(defvar *trackers*
    ;; move :freshon to last.  Their tracker is being very annoying lately
    ;; and not giving me upload credit.  Grrr.
    (list :btn :shazbat #+testing :eztv :freshon #+ignore :sporthd))

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
;;(pushnew :tget-config-debug *features*)

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

(defgroup :temp
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "tmp/" *download-root*))

;;;;TODO: seems like this shouldn't be here... figure out a way to get
;;;;      around it.  See --add command line argument.
(defgroup :manual
    :rss-url nil
    :delay 0
    :quality 'my-quality
    ;; a dummy value, overridden by whatever group matches
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
(defseries "Ash vs Evil Dead" :kevin)
(defseries "At Midnight" :kevin :date-based t :subdir "At.Midnight"
	   :aliases ("@midnight"))
(defseries "Bates Motel" :anh+kevin)
(defseries "Better Call Saul" :adrian+kevin)
(defseries "Black Mirror" :adrian+kevin)
(defseries "Blunt Talk" :kevin)
(defseries "Broad City" :kevin :catch-up "S02")
(defseries "Brooklyn Nine-Nine" :adrian+kevin)
(defseries "Cesar 911" :kevin :catch-up "S02")
(defseries "Childrens Hospital (US)" :adrian+kevin)
(defseries "Childhood's End" :kevin)
(defseries "Community" :adrian+kevin)
(defseries "Curb your Enthusiasm" :adrian+kevin)
(defseries "Doc Martin" :anh)
(defseries "Downton Abbey" :anh)
(defseries "Dragons Den (UK)" :kevin)
(defseries "Eagleheart" :adrian+kevin)
(defseries "Elementary" :kevin :subdir "Elementary")
(defseries "Fargo" :kevin)
(defseries "Fear the Walking Dead" :kevin :delay 0)
(defseries "Frontline (US)" :kevin)
(defseries "Full Frontal with Samantha Bee" :kevin)
(defseries "Game of Thrones" :kevin :private t :delay 0 :quality :high)
(defseries "Hannibal" :kevin :delay 0 #-testing :quality #-testing :high)
(defseries "Hap and Leonard" :kevin :catch-up "S01E02")
(defseries "Hell on Wheels" :kevin)
(defseries "Homeland" :kevin :private t :delay 0)
(defseries "Inside Amy Schumer" :kevin :catch-up "S03")
(defseries "James May's Man Lab" :adrian+kevin)
(defseries "Late Show with Stephen Colbert" :kevin
  :subdir "The.Late.Show.with.Stephen.Colbert"
  :aliases ("The Late Show with Stephen Colbert" "Stephen Colbert"))
(defseries "Last Week Tonight with John Oliver" :kevin :private t
  :subdir "Last.Week.Tonight.With.John.Oliver")
(defseries "Louis Theroux Documentaries" :kevin)
(defseries "Louie" :kevin)
(defseries "Luther" :kevin)
(defseries "Man Seeking Woman" :adrian+kevin :catch-up "S01")
(defseries "Midsomer Murders" :anh)
(defseries "Mr Robot" :kevin)
(defseries "NBA Warriors" :temp :date-based t :quality :high)
(defseries "Not Safe with Nikki Glaser" :kevin)
(defseries "Naked and Afraid" :kevin :catch-up "S01")
(defseries "Nathan for You" :adrian+kevin)
(defseries "Nature" :kevin :subdir "Nature")
(defseries "Nova" :kevin :subdir "Nova")
(defseries "Penn & Teller: Fool Us" :adrian+kevin :quality :high
	   :catch-up "S02E01"
	   :aliases ("Penn and Teller Fool Us"))
(defseries "Review" :kevin :catch-up "S01"
	   :aliases ("Review with Forrest MacNeil"))
(defseries "Rick and Morty" :adrian+kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Show me a hero" :kevin)
(defseries "Sirens (2014)" :kevin)
(defseries "StarTalk with Neil deGrasse Tyson" :kevin :catch-up "S01")
(defseries "The Americans (2013)" :kevin)
(defseries "The Daily Show with Trevor Noah" :kevin :subdir "The.Daily.Show"
	   :date-based t
	   :aliases ("The Daily Show"))
(defseries "The Expanse" :kevin)
(defseries "The Carmichael Show" :kevin :catch-up "S01"
  :subdir "The.Carmichael.Show")
(defseries "The Good Wife" :anh+kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The Knick" :kevin :catch-up "S01")
(defseries "The Last Man on Earth" :adrian+kevin :catch-up "S01E02")
(defseries "The Leftovers" :kevin :catch-up "S01")
(defseries "The Meltdown with Jonah and Kumail" :kevin :catch-up "S01E04")
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Nightly Show with Larry Wilmore" :kevin
  :aliases ("The Nightly Show")
  :subdir "The.Nightly.Show" :date-based t)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0)
(defseries "The X-Files" :kevin :catch-up "S09")
(defseries "Tosh.0" :kevin)
(defseries "True Detective" :kevin :private t)
(defseries "Wallander" :anh+kevin)
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
#+testing (defseries "Ray Donovan" :kevin :private t)
#+testing
(defseries "The Daily Show with Jon Stewart" :kevin :subdir "The.Daily.Show"
	   :date-based t
	   :aliases ("The Daily Show"))
#+testing (defseries "Top Gear" :adrian+kevin :quality :high)
#+testing (defseries "Witness (2012)" :kevin :private t)
#+testing (defseries "The Mentalist" :adrian+kevin)
#+testing (defseries "Vikings" :kevin)
#+testing (defseries "Intruders" :kevin :catch-up "S01E02")
;; On Netflix now, watch there
#+testing (defseries "Longmire" :kevin)
#+testing (defseries "Mad Men" :kevin)
#+testing (defseries "Regular Show" :adrian+kevin)
#+testing (defseries "Modern Family" :adrian+kevin)


(defseries "Minority Report" :kevin :remove t)
(defseries "Heroes Reborn" :kevin :remove t)
(defseries "The Strain" :kevin :remove t)
(defseries "Blindspot" :kevin :remove t)
(defseries "The Player (2015)" :kevin :remove t)
