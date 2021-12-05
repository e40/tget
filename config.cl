;; Config file for tget

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General options

;; This is now reset once per run, so it's not too much data to save,
;; and it's good for debugging feed issues.
(setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
(setq *log-xml* (merge-pathnames "error.xml" *tget-data-directory*))

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

(defvar *for-mom* "/me/tplex/content/videos/for-mom/")
(defvar *tv-archive* "/me/tplex/content/videos/archive/")

(defvar *allowed-codecs* '(:x264 :h.264))

;;;;;;;;;;;;;;;;;;;
;; For --cleanup ;;
;;;;;;;;;;;;;;;;;;;

;; Minimum seed time in seconds: 3 days, for torrents that don't have a
;; tracker-specific rule that takes precedence over this value.
(setq *minimum-seed-seconds* #.(* 3600 24 3))

;; Ignore anything watched in the last 3 days:
(setq *ignore-watched-within* #.(* 24 3))

(setq *sqlite3-binary*
  ;; CentOS 6.6 doesn't have an up-to-date enough sqlite3 binary, so we use
  ;; one from sqlite.org (version 3.8.10.2).
  "/usr/local/bin/sqlite3")

;; The directories to clean
(setq *watch-directories*
  '(("/me/tplex/content/videos/kevin" "/data/videos/kevin" . "TV Shows")
    ("/me/tplex/content/videos/movies/kevin" "/data/videos/movies/kevin"
     . "Movies")
    ("/me/tplex/content/videos/tmp" "/data/videos/tmp" . "temp")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trackers

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
    :ratio 1.3
    :re "freshon\\.tv"
    :char "F"
    :setter (lambda (torrent)
	      ;; 1.2 ratio and 3 days should do it?
	      (setf (torrent-ratio-limit torrent) 1.2)
	      (setf (torrent-seed-min-time torrent) (* 3600 24 3))))

(deftracker :btn
    ;; RSS feed configured by creating a notification filter with these
    ;; options: 
    ;; - categories: episode, season
    ;; - containers: avi, mkv, mp4
    ;; - codecs: x264, h.264
    ;; - resolutions: sd, 720p
    ;; All other checkboxs were unchecked.
    ;;
    ;; passkey is in DL links.  Very hard to figure this out!
    ;;
    :url "https://broadcasthe.net/..."
    :debug-feed :btn
    :disabled #-debug nil #+debug t
    ;; Can't make the delay too long or shows will fall off the RSS feed
    :download-delay #-debug 1 #+debug 0
    :ratio 1.5
    :re "broadcasthe\\.net|landof\\.tv"
    :char "B"
    :setter (lambda (torrent)
	      (setf (torrent-ratio-limit torrent) 1.50)
	      (if* (torrent-seasonp torrent)
		 then ;; Seed for a week + slop 
		      (setf (torrent-seed-min-time torrent)
			(* 3600 24 12))
		 else ;; The rules state you need to seed to 1:1 or 24
		      ;; hours.  Give it some slop, to make sure I don't
		      ;; get a H&R
		      ;; NOTE: the BTN tracker is notorious for not
		      ;;       counting seed time, so use 3d as the minimum
		      ;;       time here to make sure we don't get a H&R
		      (setf (torrent-seed-min-time torrent) (* 3600 24 3)))))

(deftracker :mtv
    :url "https://www.morethan.tv/..."
    :disabled nil
    :download-delay 0
    :ratio 1.5
    :re "morethantv"
    :char "T"
    :setter (lambda (torrent)
	      (setf (torrent-ratio-limit torrent) 1.5)
	      (setf (torrent-seed-min-time torrent) (* 3600 24 100))))

(deftracker :shazbat
    :url "https://www.shazbat.tv/..."
    :debug-feed :shazbat
    :disabled t
    :download-delay 0
    :ratio 1.5
    :re "shazbat|bttracker\\.tv"
    :char "S"
    :setter (lambda (torrent)
	      ;; 1.2 ratio and 3 days should do it?
	      (setf (torrent-ratio-limit torrent) 1.2)
	      (setf (torrent-seed-min-time torrent) (* 3600 24 3))))

;;;;TODO: rename this to *auto-trackers* or something
(defvar *trackers*
    ;; Give the new guy first crack:
    (list #+ignore :mtv ;; for now
	  :btn
	  #+ignore :shazbat ;; hasn't worked in forever???
	  ))

;; for --cleanup, only manually downloaded
(deftracker :720pier
    :re "720pier"
    :char "7"
    :setter (lambda (torrent)
	      ;; 1.5 ratio and 3 days should do it?
	      (setf (torrent-ratio-limit torrent) 1.5)
	      (setf (torrent-seed-min-time torrent) (* 3600 24 3))))

;; for --cleanup, only manually downloaded
(deftracker :mma-tracker
    :re "mma-tracker"
    :char "M"
    :setter (lambda (torrent)
	      ;; Hard to seed stuff here, so seed longer.
	      (setf (torrent-seed-min-time torrent) (* 3600 24 4))
	      (setf (torrent-ratio-limit torrent) 1.5)))

(deftracker :ptn
    :re "piratethenet"
    :char "x"
    :setter (lambda (torrent)
	      ;; Hard to seed stuff here, so seed longer.
	      (setf (torrent-seed-min-time torrent) (* 3600 24 4))
	      (setf (torrent-ratio-limit torrent) 1.1)))

;; Public trackers, just in case
(deftracker :public-misc
    :re "tracker\\.ccc\\.de|openbittorrent\\.com|thepiratebay"
    :char "p"
    :setter (lambda (torrent)
	      ;; Public tracker, so just seed to 1:1
	      (setf (torrent-ratio-limit torrent) 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quality settings

(defquality :high-1080p
    :priority 10
    :codec *allowed-codecs* 
    :resolution :1080p)

(defquality :high-1080i
    :priority 1
    :codec *allowed-codecs* 
    :resolution :1080i)

(defquality :normal
    ;; The priority of a quality allows selection of episodes when more
    ;; than one quality is available at the same time, as is often the
    ;; case.  Higher numerical priority is given precedence.
    :priority 50
    :source :hdtv
    :codec *allowed-codecs* 
    :resolution :sd)

(defquality :high
    :priority 40
    :codec *allowed-codecs* 
    :resolution :720p)

(defquality :low
    :priority 30
    :source :hdtv
    :codec :xvid
    :resolution :sd)

(defquality :btn
    :priority 90
    :codec *allowed-codecs* 
    :resolution :720p)

;; The additional delay waiting to download a high quality ep while
;; waiting for a normal quality one to become available
(defvar *download-hq-delay* 0)

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

(defgroup :kevin
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "kevin/" *download-root*))

(defgroup :temp
    :trackers '#.*trackers*
    :quality 'my-quality
    :download-path (merge-pathnames "tmp/" *download-root*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TV shows

;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
;; Use ... :remove t ... to delete a series
;; Use ... :subdir "dirname" ... to put the episodes into a subdirectory of
;;          the group download path -- this is a hack to make Plex Media
;;          Server see episodes of The Daily Show and The Colbert Report.

(defseries "8 Out of 10 Cats" :kevin :archive #.*for-mom*
	   :subdir "8.out.of.10.cats")
(defseries "8 Out of 10 Cats Does Countdown" :kevin :archive #.*for-mom*
	   :subdir "8.out.of.10.cats.does.countdown")
(defseries "American Experience" :kevin :subdir "American.Experience")
(defseries "Atlanta" :kevin :subdir "Atlanta")
(defseries "Barry" :kevin :subdir "Barry" :private t)
(defseries "Better Call Saul" :kevin :archive #.*tv-archive*)

(defseries "Corporate" :kevin)

(defseries "Elementary" :kevin :subdir "Elementary")
(defseries "Fargo" :kevin :quality :high :archive #.*tv-archive*)
(defseries "Frontline (US)" :kevin)
(defseries "Future Man" :kevin :catch-up "S01" :archive #.*tv-archive*)
(defseries "Good Talk With Anthony Jeselnik" :kevin :subdir "Good.Talk")

(defseries "Jeff Ross Presents Roast Battle" :kevin)
(defseries "Luther" :kevin)
(defseries "Man Seeking Woman" :kevin :catch-up "S01")
(defseries "Not Safe with Nikki Glaser" :kevin)
(defseries "Naked and Afraid" :kevin :catch-up "S01"
	   :subdir "Naked.and.Afraid")
(defseries "Naked and Afraid XL" :kevin :catch-up "S02"
	   :subdir "Naked.and.Afraid.XL")
(defseries "Nathan for You" :kevin)
(defseries "Nature" :kevin :subdir "Nature")
(defseries "Nova" :kevin :subdir "Nova")
(defseries "Penn & Teller: Fool Us" :kevin :quality :high
	   :catch-up "S02E01"
	   :aliases ("Penn and Teller Fool Us"))
(defseries "Rick and Morty" :kevin :archive #.*tv-archive*)
(defseries "Sherlock" :kevin)
(defseries "Star Trek: Discovery" :kevin :archive #.*tv-archive*
	   :aliases ("Star Trek Discovery"))
(defseries "Star Trek: Picard" :kevin :archive #.*tv-archive*)
(defseries "Taskmaster" :kevin :catch-up "S03"
	   :aliases ("Taskmaster UK" "Taskmaster (UK)")
	   :subdir "Taskmaster")
(defseries "The Graham Norton Show" :kevin)
(defseries "The Righteous Gemstones" :kevin :subdir "The.Righteous.Gemstones"
	   :private t)
(defseries "The Terror" :kevin :catch-up "S01" :archive #.*tv-archive*)
(defseries "Tosh.0" :kevin)

(defseries "Would I Lie To You?" :kevin :catch-up "S08E01"
	   :aliases ("Would I Lie To You")
	   :archive #.*for-mom*)
(defseries "Hannibal" :kevin :delay 0 :quality :high)
(defseries "Killing Eve" :kevin :quality :high
	   :subdir "Killing.Eve")
(defseries "The Mandalorian" :kevin :quality :high :archive #.*tv-archive*
	   :subdir "The.Mandalorian")
(defseries "Avenue 5" :kevin :subdir "Avenue.5")


(defseries "Dispatches from Elsewhere" :kevin :catch-up "S01E02")

(defseries "The Last Dance" :kevin :catch-up "S01E02")
(defseries "Penny Dreadful City of Angels" :kevin
  :subdir "Penny.Dreadful.City.of.Angels")
(defseries "I Know This Much is True" :kevin)

(defseries "Resident Alien" :kevin)
(defseries "Criminal UK" :kevin)

(defseries "Loki" :kevin :catch-up "S01E01")

(defseries "Kin" :kevin)

(defseries "Squid Game" :kevin :catch-up "S01" :archive #.*tv-archive*)
