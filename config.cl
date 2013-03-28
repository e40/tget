
(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed and other options

(setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
(setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))

;; Wait 6 hours before downloading (most) episodes, to wait for repacks and
;; propers.
(defvar *tvt-delay* 6)

(defun tvt-rss-feed (days)
  (format nil "~a&interval=~d+days" (sys:getenv "TGET_TVT_RSS_BASE") days))

(defvar *tvt-rss* 'tvt-rss-feed)

;; custom rss feed with these options:
;; - feed name: RegularShow
;; - series: Regular Show
;; - category: Episode
;; - resolutions: 720p (less than this quality are crap)
(defvar *btn-regular-show-rss* (sys:getenv "TGET_BTN_RSS_REGULARSHOW"))

;; custom rss feed with these options:
;; - feed name: Spongebob
;; - series: Spongebob Squarepants
;; - category: Episode
;; - resolutions: 720p (less than this quality are crap)
(defvar *btn-spongebob-rss* (sys:getenv "TGET_BTN_RSS_SPONGEBOB"))

#+not-yet
(deftransmission ()
    :host (sys:getenv "TRANSMISSION_HOST")
    :port 9091
    :username (sys:getenv "TRANSMISSION_USER")
    :password (sys:getenv "TRANSMISSION_PASS")
    :add-paused nil
    :ratio "1.04"
    ;; Don't enable this because I like to see what's there before it's
    ;; removed.
    :remove-when-done nil)

(setq *download-root* "/me/layer/videos/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quality settings

(defquality :normal
    :source :hdtv
    :codec :x264 
    :resolution :<720p)

(defquality :high
    :source :hdtv
    :codec :x264 
    :resolution :720p)

(defquality :low
    :source :hdtv
    :codec :xvid
    :resolution :<720p)

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
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "adrian/" *download-root*))

(defseries "Phineas and Furb" :adrian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defseries "Downton Abbey" :anh)
(defseries "Midsomer Murders" :anh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
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
(defseries "Mad men" :kevin)
(defseries "Misfits" :kevin)
(defseries "Motive" :kevin)
(defseries "Orphan Black" :kevin)
(defseries "Person of Interest" :kevin)
(defseries "Shameless (us)" :kevin)
(defseries "Sherlock" :kevin)
(defseries "Southland" :kevin)
(defseries "Strike Back" :kevin)
(defseries "The Americans (2013)" :kevin)
(defseries "The Colbert Report" :kevin)
(defseries "The Daily Show with Jon Stewart" :kevin)
(defseries "The Graham Norton Show" :kevin)
(defseries "The IT crowd" :kevin)
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
    :client :transmission-rpc
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :btn-regular-show
    :rss-url *btn-regular-show-rss*
    :ratio "-1" 
    :client :transmission-rpc
    :quality t ;; built into the feed
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defseries "regular show" ::btn-regular-show)

(defgroup :btn-spongebob
    :rss-url *btn-spongebob-rss*
    :ratio "-1" 
    :client :transmission-rpc
    :quality t ;; built into the feed
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defseries "Spongebob Squarepants" :btn-spongebob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh+kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
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
