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
  :ratio "1.1"))

;; An alternate method for downloading .torrent files: put them into a
;; specific directory, where your torrent client will see them.
#+ignore
(set-torrent-handler (pathname "~/Downloads/"))

(setq *download-root* "/me/layer/videos/")

(defvar *codec-x264*
    ;; It goes by two different names:
    '(:x264 :h.264))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main TV shows

;; The delay before an episode is downloaded.  This cuts down on bogus
;; episodes and the need to download a repack.
(setq *download-delay* 1)
;; The additional delay waiting to download a high quality ep while
;; waiting for a normal quality one to become available.
(setq *download-hq-delay* 1)
;; The additional delay waiting to download a low quality ep while
;; waiting for a normal or high quality one to become available.
(setq *download-lq-delay* 24)

;; Not all sites support the idea of a feed interval.
;; It's a nice feature, because if you decide to download a new series,
;; you'll get any episodes released in this period of time.  And, for the
;; initial installation, you can specify a really high interval (on the
;; command line, not here), to populate your database with your shows.
(setq *feed-interval* 21)

;; When --debug is given on the command line, the debug version is used,
;; and that's what this is.  No need to bombard the RSS server with
;; requests while debugging.
(defvar *debug-feed* "tget-test-data/debug.xml")

;; This is how you define names for qualities you care about.
;;
(defquality :normal
    ;; The priority of a quality allows selection of episodes when more
    ;; than one quality is available at the same time, as is often the
    ;; case.  Higher numerical priority is given precedence.
    ;;
    ;; This is my preferred quality.
    
;;;; The documentation for these options is in the README.md file.
    :priority 50
    :source :hdtv
    :codec *codec-x264* 
    :resolution :sd)

(defquality :high
    :priority 40
    #|:source :hdtv|#			;allow any source
    :codec *codec-x264* 
    :resolution :720p)

(defquality :low
    :priority 30
    :source :hdtv
    :codec :xvid
    :resolution :sd)

(defquality :indi :resolution :sd)

(defquality :x264-hdtv-mp4
    :priority 60
    :container :mp4
    :source :hdtv
    :codec *codec-x264*)

;; This is a user-defined quality function.

(defun my-quality (episode)
  (if* (and (null
	     ;; See if there is an episode with :normal quality.  The
	     ;; :transient keyword is important, since it restricts the
	     ;; search to episodes we have *not* downloaded yet.
	     (query-episode :episode episode :quality :normal :transient t))
	    (eq :high (episode-quality episode))
	    (>= (hours-available episode) (+ *download-delay*
					     *download-hq-delay*)))
     then ;; :normal quality is not available and the :high quality episode
	  ;; has been available for a set amount of hours, then take this
	  ;; one
	  :high
   elseif (=~ "broadcasthe.net" (episode-torrent-url episode))
     then :x264-hdtv-mp4
   elseif (and (null
		;; See if there is an episode with :normal or :high
		;; quality.
		(or
		 (query-episode :episode episode :quality :normal :transient t)
		 (query-episode :episode episode :quality :high :transient t)))
	       (eq :low (episode-quality episode))
	       (>= (hours-available episode) *download-lq-delay*))
     then :low
     else :normal))

(defvar *eztv-rss* "https://ezrss.it/...")

(defvar *freshon-rss*
    "https://freshon.tv/...")

(defvar *btn-rss* "https://broadcasthe.net/...")

(defvar *rss-urls* (list *freshon-rss*
			 #+ignore *eztv-rss*
			 *btn-rss*))

(defvar *ppv-rss-urls* (list *freshon-rss*
			     *btn-rss*))

(defgroup :adrian
    :rss-url '#.*rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian/" *download-root*))

(defgroup :anh
    :rss-url '#.*rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defgroup :kevin
    :rss-url '#.*rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defgroup :kevin-ppv ;; don't use public trackers for this
    :rss-url '#.*ppv-rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defgroup :adrian+kevin
    :rss-url '#.*rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defgroup :anh+kevin
    :rss-url '#.*rss-urls*
    :debug-feed *debug-feed*
    :delay *download-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh+kevin/" *download-root*))

(defgroup :manual
    :rss-url nil
    :debug-feed nil
    :delay 0
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
;; Use ... :remove t ... to delete a series
;; Use ... :subdir "dirname" ... to put the episodes into a subdirectory of
;;          the group download path -- this is a hack to make Plex Media
;;          Server see episodes of The Daily Show and The Colbert Report.

(defseries "8 Out of 10 Cats" :kevin)
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
(defseries "Game of Thrones" :kevin-ppv :delay 0) ;; immediate download
(defseries "Hannibal" :kevin :delay 0) ;; immediate download
(defseries "Hell on Wheels" :kevin)
(defseries "Homeland" :kevin-ppv :delay 0) ;; immediate download
(defseries "Intruders" :kevin :catch-up "S01E02")
(defseries "James May's Man Lab" :adrian+kevin)
(defseries "Justified" :kevin)
(defseries "Last Week Tonight with John Oliver" :kevin-ppv
  :subdir "Last.Week.Tonight.With.John.Oliver")
(defseries "Longmire" :kevin)
(defseries "Louis Theroux Documentaries" :kevin)
(defseries "Louie" :kevin)
(defseries "Luther" :kevin)
(defseries "Mad Men" :kevin)
(defseries "Midsomer Murders" :anh)
(defseries "Modern Family" :adrian+kevin)
(defseries "Naked and Afraid" :kevin :catch-up "S01")
(defseries "Nathan for You" :adrian+kevin)
(defseries "Nova" :kevin)
(defseries "Parks and Recreation" :adrian+kevin)
(defseries "Person of Interest" :kevin)
(defseries "Running Wild with Bear Grylls" :kevin :catch-up "S01E04")
(defseries "Ray Donovan" :kevin-ppv)
(defseries "Rick and Morty" :adrian+kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Sirens (2014)" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Colbert Report" :kevin :subdir "The.Colbert.Report"
	   :date-based t)
(defseries "The Daily Show with Jon Stewart" :kevin :subdir "The.Daily.Show"
	   :date-based t
	   :aliases ("The Daily Show"))
(defseries "The Good Wife" :anh+kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The Meltdown with Jonah and Kumail" :kevin :catch-up "S01E04")
(defseries "The Mentalist" :adrian+kevin)
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Newsroom (2012)" :kevin-ppv)
(defseries "The Simpsons" :adrian+kevin)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
(defseries "Top Gear" :adrian+kevin :quality :high)
(defseries "Top of the Lake" :anh+kevin)
(defseries "Tosh.0" :kevin)
(defseries "True Detective" :kevin-ppv)
(defseries "Vikings" :kevin)
(defseries "Wallander" :anh+kevin)
(defseries "White Collar" :anh+kevin)
(defseries "Would I Lie To You?" :kevin :catch-up "S08E01"
	   :aliases ("Would I Lie To You"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BTN-only shows

(defvar *btn-my-series-feed*
    "https://broadcasthe.net/...")

(defvar *btn-debug-feed* "tget-test-data/btn.xml")

;; BTN quality is kinda funky and inconsistent.  Define some different
;; qualities for shows from them.

(defquality :x264-?dtv-mp4
    :priority 10
    :container :mp4
    :source '(:pdtv :hdtv)
    :codec *codec-x264*)

(defgroup :btn-adrian+kevin
    :rss-url *btn-my-series-feed*
    :debug-feed *btn-debug-feed*
    :ratio "-1" 
    :quality :normal
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defgroup :btn-kevin
    :rss-url *btn-my-series-feed*
    :debug-feed *btn-debug-feed*
    :ratio "-1" 
    :quality :normal
    :download-path (merge-pathnames "kevin/" *download-root*))

(defseries "Regular Show" :btn-adrian+kevin :quality :high)
(defseries "World's Craziest Fools" :btn-adrian+kevin :quality :x264-?dtv-mp4)
(defseries "Witness (2012)" :btn-kevin :quality :x264-?dtv-mp4)
(defseries "8 Out of 10 Cats Does Countdown"
    :btn-kevin :quality :high)
