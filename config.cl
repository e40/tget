
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

(defseries "phineas and furb" :adrian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "anh/" *download-root*))

(defseries "downton abbey" :anh)
(defseries "midsomer murders" :anh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defseries "8 out of 10 cats" :kevin)
(defseries "boardwalk empire" :kevin)
(defseries "breaking bad" :kevin :delay 0) ;; immediate download
(defseries "childrens hospital (us)" :kevin)
(defseries "come fly with me (2010)" :kevin)
(defseries "dexter" :kevin)
(defseries "dragons den" :kevin)
(defseries "elementary" :kevin)
(defseries "falling skies" :kevin)
(defseries "frontline" :kevin)
(defseries "game of thrones" :kevin :delay 0) ;; immediate download
(defseries "homeland" :kevin)
(defseries "justified" :kevin :delay 0) ;; immediate download
(defseries "longmire" :kevin)
(defseries "louis theroux documentaries" :kevin)
(defseries "louis" :kevin)
(defseries "luther" :kevin)
(defseries "mad men" :kevin)
(defseries "misfits" :kevin)
(defseries "motive" :kevin)
(defseries "orphan black" :kevin)
(defseries "person of interest" :kevin)
(defseries "shameless (us)" :kevin)
(defseries "sherlock" :kevin)
(defseries "southland" :kevin)
(defseries "strike back" :kevin)
(defseries "the americans (2013)" :kevin)
(defseries "the colbert report" :kevin)
(defseries "the daily show with jon stewart" :kevin)
(defseries "the graham norton show" :kevin)
(defseries "the it crowd" :kevin)
(defseries "the jeselnik offensive" :kevin)
(defseries "the newsroom (2012)" :kevin)
(defseries "the ultimate fighter" :kevin)
(defseries "the walking dead" :kevin :delay 0) ;; immediate download
(defseries "tosh.0" :kevin)
(defseries "vikings" :kevin)
(defseries "witness (2012)" :kevin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :adrian+kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "adrian+kevin/" *download-root*))

(defseries "an idiot abroad" :adrian+kevin)
(defseries "community" :adrian+kevin)
(defseries "eagleheart" :adrian+kevin)
(defseries "futurama" :adrian+kevin)
(defseries "modern family" :adrian+kevin)
(defseries "mythbusters" :adrian+kevin)
(defseries "nathan for you" :adrian+kevin)
(defseries "ncis" :adrian+kevin)
(defseries "nova" :adrian+kevin)
(defseries "parks and recreation" :adrian+kevin)
(defseries "ridiculousness" :adrian+kevin)
(defseries "shark tank" :adrian+kevin)
(defseries "the mentalist" :adrian+kevin)
(defseries "the neighbors (2012)" :adrian+kevin)
(defseries "the simpsons" :adrian+kevin)
(defseries "top gear" :adrian+kevin :quality :high)
(defseries "top gear (us)" :adrian+kevin :quality :high)

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

(defseries "spongebob squarepants" :btn-spongebob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup :anh+kevin
    :rss-url *tvt-rss*
    :delay *tvt-delay*
    :client :transmission-rpc
    :quality 'my-quality
    :download-path (merge-pathnames "anh+kevin/" *download-root*))

(defseries "bates motel" :anh+kevin)
(defseries "curb your enthusiasm" :anh+kevin)
(defseries "doc martin" :anh+kevin)
(defseries "the following" :anh+kevin)
(defseries "the good wife" :anh+kevin)
(defseries "the killing" :anh+kevin)
(defseries "wallander" :anh+kevin)
(defseries "white collar" :anh+kevin)
