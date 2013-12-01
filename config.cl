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
  :add-paused nil
  :trash-torrent-file t
  :ratio "1.04"))

;; An alternate method for downloading .torrent files: put them into a
;; specific directory, where your torrent client will see them.
#+ignore
(set-torrent-handler (pathname "~/Downloads/"))


(setq *download-root* "/me/layer/videos/")

(defvar *codec-x264*
    ;; It goes by two different names:
    '(:x264 :h.264))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TVT

;; Not all sites support the idea of a delay, but TVT does.
;; It's a nice feature, because it allows you to delay "seeing" RSS items,
;; to give the various downloads time to settle.  It reduces the chances of
;; having to download repacks or propers (another name for repacks).
;;
;; I like a 6 hour delay.
(defvar *tvt-delay* 6)

;; Not all sites support the idea of a feed interval, but TVT does.
;; It's a nice feature, because if you decide to download a new series,
;; you'll get any episodes released in this period of time.  And, for the
;; initial installation, you can specify a really high interval (on the
;; command line, not here), to populate your database with your shows.
(setq *feed-interval* 14)

;; This function is given as the value of the defgroup :rss-url option.
;; The function is called, when tget needs to fetch the feed, with the
;; value *feed-interval* or the command line override for that variable
;; (the --interval argument).
(defun tvt-rss-feed (interval)
  (format nil "~a&interval=~d+days"
	  ;; This is the "Recent torrents" feed instead of the "Favorite
	  ;; shows" feed I was using before.
	  ;;
	  ;; Using "Favorite shows" feed means you have to maintain your
	  ;; list of shows in *two* places, which I find very annoying.
	  ;;
	  "http://www.tvtorrents.com/..."
	  interval))

(defvar *tvt-rss* 'tvt-rss-feed)

;; When --debug is given on the command line, the debug version is used,
;; and that's what this is.  No need to bombard the RSS server with
;; requests while debugging.
(defvar *tvt-debug-feed* "tget-test-data/tvt.xml")

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
    :source :hdtv
    :codec *codec-x264* 
    :resolution :720p)

(defquality :low
    :priority 30
    :source :hdtv
    :codec :xvid
    :resolution :sd)

(defquality :indi :resolution :sd)

;; This is a user-defined quality function.

(defun my-quality (episode)
  ;; Download :normal quality immediately.
  ;; If that's not available, then download :high quality after 12 hours
  ;; and :low quality after 2 days.  There are still some shows that never
  ;; get anything but :low, for some episodes.
  (if* (and (null
	     ;; See if there is an episode with :normal quality.  The
	     ;; :transient keyword is important, since it restricts the
	     ;; search to episodes we have *not* downloaded yet.
	     (query-episode :episode episode :quality :normal :transient t))
	    (eq :high (episode-quality episode))
	    (>= (hours-available episode) 12))
     then ;; :normal quality is not available and the :high quality episode
	  ;; has been available for 12 hours, then return...
	  :high
   elseif (and (null
		;; See if there is an episode with :normal or :high
		;; quality.
		(or
		 (query-episode :episode episode :quality :normal :transient t)
		 (query-episode :episode episode :quality :high :transient t)))
	       (eq :low (episode-quality episode))
	       (>= (hours-available episode) 48))
     then :low
     else :normal))

(defgroup :adrian
    :rss-url *tvt-rss*
    :debug-feed *tvt-debug-feed*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian/" *download-root*))

(defgroup :anh
    :rss-url *tvt-rss*
    :debug-feed *tvt-debug-feed*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defgroup :kevin
    :rss-url *tvt-rss*
    :debug-feed *tvt-debug-feed*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defgroup :adrian+kevin
    :rss-url *tvt-rss*
    :debug-feed *tvt-debug-feed*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defgroup :anh+kevin
    :rss-url *tvt-rss*
    :debug-feed *tvt-debug-feed*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh+kevin/" *download-root*))

;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
;; Use ... :remove t ... to delete a series

(defseries "8 Out of 10 Cats" :kevin)
(defseries "Almost Human" :kevin)
(defseries "An Idiot Abroad" :adrian+kevin)
(defseries "Archer" :kevin)
(defseries "Bates Motel" :anh+kevin)
(defseries "Bear Grylls: Escape From Hell" :kevin :catch-up "S01E05")
(defseries "Black Mirror" :kevin)
(defseries "Boardwalk Empire" :kevin)
(defseries "Brooklyn Nine-Nine" :kevin)
(defseries "Childrens Hospital (US)" :kevin)
(defseries "Come Fly with Me (2010)" :kevin)
(defseries "Community" :adrian+kevin)
(defseries "Curb your Enthusiasm" :anh+kevin)
(defseries "Doc Martin" :anh+kevin)
(defseries "Downton Abbey" :anh)
(defseries "Dracula (2013)" :kevin)
(defseries "Dragons Den (UK)" :kevin)
(defseries "Eagleheart" :adrian+kevin)
(defseries "Elementary" :kevin)
(defseries "Family Tree" :kevin)
(defseries "Frontline (US)" :kevin)
(defseries "Game of Thrones" :kevin :delay 0) ;; immediate download
(defseries "Hannibal" :anh+kevin)
(defseries "Hell on Wheels" :kevin)
(defseries "Homeland" :kevin :delay 0) ;; immediate download
(defseries "James May's Man Lab" :adrian+kevin)
(defseries "Justified" :kevin)
(defseries "Kung Fu Panda: Legends of Awesomeness" :adrian
  ;; All there is on TVT
  :quality :indi)
(defseries "Longmire" :kevin)
(defseries "Louis Theroux Documentaries" :kevin)
(defseries "Louie" :kevin)
(defseries "Luther" :kevin)
(defseries "Mad Men" :kevin)
(defseries "Maron" :kevin)
(defseries "Midsomer Murders" :anh)
(defseries "Misfits" :kevin)
(defseries "Mob City" :kevin)
(defseries "Modern Family" :adrian+kevin)
(defseries "Motive" :kevin)
(defseries "Mythbusters" :adrian+kevin)
(defseries "NCIS" :adrian+kevin)
(defseries "Nathan for You" :adrian+kevin)
(defseries "Nova" :adrian+kevin)
(defseries "Oliver Stone's Untold History of the United States" :adrian+kevin)
(defseries "Parks and Recreation" :adrian+kevin)
(defseries "Person of Interest" :kevin)
(defseries "Phineas and Furb" :adrian)
(defseries "Ray Donovan" :kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :adrian+kevin)
(defseries "Sherlock" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Blacklist" :adrian+kevin)
(defseries "The Burn" :kevin)
(defseries "The Colbert Report" :kevin)
(defseries "The Daily Show with Jon Stewart" :kevin)
(defseries "The Good Wife" :anh+kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The IT Crowd" :kevin)
(defseries "The Jeselnik Offensive" :kevin)
(defseries "The Mentalist" :adrian+kevin)
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Newsroom (2012)" :kevin)
(defseries "The Simpsons" :adrian+kevin)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
(defseries "The White Queen" :anh+kevin)
(defseries "Top Gear (US)" :adrian+kevin :quality :high)
(defseries "Top Gear" :adrian+kevin :quality :high)
(defseries "Top of the Lake" :anh+kevin)
(defseries "Tosh.0" :kevin)
(defseries "Vikings" :kevin)
(defseries "Wallander" :anh+kevin)
(defseries "White Collar" :anh+kevin)
(defseries "Would I Lie To You" :adrian+kevin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BTN

;; BTN quality is kinda funky and inconsistent.  Define some different
;; qualities for shows from them.

(defquality :high-any-source
    :priority 40
    :codec *codec-x264*
    :resolution :720p)

(defquality :x264-?dtv-mp4
    :priority 10
    :container :mp4
    :source '(:pdtv :hdtv)
    :codec *codec-x264*)

(defvar *btn-my-series-feed*
    "https://broadcasthe.net/...")

(defvar *btn-debug-feed* "tget-test-data/btn.xml")

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

(defseries "Regular Show" :btn-adrian+kevin :quality :high-any-source)
(defseries "Spongebob Squarepants" :btn-adrian+kevin :quality :high-any-source)
(defseries "World's Craziest Fools" :btn-adrian+kevin :quality :x264-?dtv-mp4)
(defseries "Witness (2012)" :btn-kevin :quality :x264-?dtv-mp4)
