
(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed and other options

;;;;Not really using this, and it's a lot of data:
;;(setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
(setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))

;; Wait 6 hours before downloading (most) episodes, to wait for repacks and
;; propers.
(defvar *tvt-delay* 6)

(defun tvt-rss-feed (days)
  (format nil "~a&interval=~d+days"
	  "http://www.tvtorrents.com/..."
	  days))

(defvar *tvt-rss* 'tvt-rss-feed)

(deftransmission ()
    :host (sys:getenv "TRANSMISSION_HOST")
    :port (sys:getenv "TRANSMISSION_PORT")
    :username (sys:getenv "TRANSMISSION_USER")
    :password (sys:getenv "TRANSMISSION_PASS")
    :add-paused nil
    :trash-torrent-file t
    :ratio "1.04")

(setq *download-root* "/me/layer/videos/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quality settings

(defquality :normal
    :priority 50
    :source :hdtv
    :codec :x264 
    :resolution :sd)

(defquality :high
    :priority 40
    :source :hdtv
    :codec :x264 
    :resolution :720p)

(defquality :low
    :priority 30
    :source :hdtv
    :codec :xvid
    :resolution :sd)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :adrian
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian/" *download-root*))

(defseries "Phineas and Furb" :adrian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defseries "Downton Abbey" :anh)
(defseries "Midsomer Murders" :anh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defseries "8 Out of 10 Cats" :kevin)
(defseries "Boardwalk Empire" :kevin)
(defseries "Breaking Bad" :kevin :delay 0) ;; immediate download
(defseries "Childrens Hospital (US)" :kevin)
(defseries "Come Fly with Me (2010)" :kevin)
(defseries "Dexter" :kevin)
(defseries "Dragons Den" :kevin)
(defseries "Elementary" :kevin)
(defseries "Falling Skies" :kevin)
(defseries "Frontline" :kevin)
(defseries "Game of Thrones" :kevin :delay 0) ;; immediate download
(defseries "Homeland" :kevin)
(defseries "Justified" :kevin :delay 0) ;; immediate download
(defseries "Longmire" :kevin)
(defseries "Louis Theroux Documentaries" :kevin)
(defseries "Louis" :kevin)
(defseries "Luther" :kevin)
(defseries "Mad Men" :kevin)
(defseries "Misfits" :kevin)
(defseries "Motive" :kevin)
(defseries "Orphan Black" :kevin)
(defseries "Person of Interest" :kevin)
(defseries "Shameless (US)" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Southland" :kevin)
(defseries "Strike Back" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Colbert Report" :kevin)
(defseries "The Daily Show with Jon Stewart" :kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The IT Crowd" :kevin)
(defseries "The Jeselnik Offensive" :kevin)
(defseries "The Newsroom (2012)" :kevin)
(defseries "The Ultimate Fighter" :kevin)
(defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
(defseries "Tosh.0" :kevin)
(defseries "Vikings" :kevin)
(defseries "Witness (2012)" :kevin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :adrian+kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defseries "An Idiot Abroad" :adrian+kevin)
(defseries "Community" :adrian+kevin)
(defseries "Eagleheart" :adrian+kevin)
(defseries "Futurama" :adrian+kevin)
(defseries "James May's Man Lab" :adrian+kevin)
(defseries "Modern Family" :adrian+kevin)
(defseries "Mythbusters" :adrian+kevin)
(defseries "Nathan for You" :adrian+kevin)
(defseries "NCIS" :adrian+kevin)
(defseries "Nova" :adrian+kevin)
(defseries "Parks and Recreation" :adrian+kevin)
(defseries "Ridiculousness" :adrian+kevin)
(defseries "Shark Tank" :adrian+kevin)
(defseries "The Mentalist" :adrian+kevin)
(defseries "The Neighbors (2012)" :adrian+kevin)
(defseries "The Simpsons" :adrian+kevin)
(defseries "Top Gear" :adrian+kevin :quality :high)
(defseries "Top Gear (US)" :adrian+kevin :quality :high)
(defseries "World's Craziest Fools" :adrian+kevin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh+kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :quality 'my-quality
    :download-path (merge-pathnames "anh+kevin/" *download-root*))

(defseries "Bates Motel" :anh+kevin)
(defseries "Curb your Enthusiasm" :anh+kevin)
(defseries "Doc Martin" :anh+kevin)
(defseries "The Following" :anh+kevin)
(defseries "The Good Wife" :anh+kevin)
(defseries "The Killing" :anh+kevin)
(defseries "Wallander" :anh+kevin)
(defseries "White Collar" :anh+kevin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BTN quality is kinda funky and inconsistent.  Define some different
;; qualities for shows from them.

(defquality :high-any-source
    :priority 40
    :codec :x264 
    :resolution :720p)

(defquality :x264-?dtv-mp4
    :priority 10
    :container :mp4
    :source '(:pdtv :hdtv)
    :codec :x264)

(defvar *btn-my-series-feed*
    "https://broadcasthe.net/...")

(defgroup :btn-adrian+kevin
    :rss-url *btn-my-series-feed*
    :ratio "-1" 
    :quality :normal
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defseries "Regular Show" :btn-adrian+kevin :quality :high-any-source)
(defseries "Spongebob Squarepants" :btn-adrian+kevin :quality :high-any-source)
(defseries "World's Craziest Fools" :btn-crazy-fools :quality :x264-?dtv-mp4)
