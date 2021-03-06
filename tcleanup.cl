;; tcleanup :: torrent maintenance does two things:
;;  1. removes from Transmission torrents which are done seeding, and
;;  2. removes videos from the filesystem which have been watched. 
;;
;; TODO:
;; * need to find some way to rename badly named torrents, ones that Plex
;;   won't see.  Need to do it in a way that allows Transmission to
;;   continue to seed.  Possibilities:
;;     1. use the JSON-RPC interface to do it
;;     2. use ssh and create a symlink with the correct name
;;   (1) is preferred and I'm assuming (2) will work with Plex

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART I: remove torrents from Transmission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variables from the config file

(defvar *minimum-seed-seconds* nil)

(defvar *remove-seeded* nil)
(defvar *remove-watched* nil)

(defun tm (&rest args)
  (multiple-value-bind (stdout stderr code)
      (command-output
       (format nil
	       "transmission-remote ~a:~a --auth=~a:~a~{ ~a~}"
	       (sys:getenv "TRANSMISSION_HOST")
	       (sys:getenv "TRANSMISSION_PORT")
	       (sys:getenv "TRANSMISSION_USER")
	       (sys:getenv "TRANSMISSION_PASS")
	       args))
    (when (/= 0 code)
      (error "transmission-remote failed: ~d:~{~% ~a~}~%" code stderr))
    stdout))

(defun init-torrent-data (&aux (state :start)
			       words
			       id
			       torrent-data
			       (res '()))
  ;; Get a list of torrents from Transmission (via transmission-remote)
  ;; and return a list of (torrent-id . hash-table) where the hash table
  ;; is populated by ``key: value'' from the output of transmission-remote
  ;; so it can be easily queried later.
  ;;
  (dolist (line (tm "-t" "all" "--info"))
    (setq words (split-re " +" line))
    ;;(and *debug* (format t "~s line=~s~%" state words))
   :top
    (ecase state
      (:start
       (when (string/= "NAME" (car words)) (error "Expected NAME"))
       (setq torrent-data (make-hash-table :size 777 :test #'equal))
       (setq state :id))
      (:id
       (when (not (and (string= "" (first words))
		       (string= "Id:" (second words))))
	 (error "Expected Id:"))
       (when (not (=~ "^(\\d+)$" (third words)))
	 (error "Expected number after Id: ~a" (third words)))
       (setq id (parse-integer $1))
       (setq state :gobble))
      (:gobble
       (when (null words) (go :next))
       (when (string= "NAME" (car words))
	 (push (cons id torrent-data) res)
	 (setq state :start)
	 (go :top))
       
       (when (string/= "" (car words)) (go :next))
       
       (when (not (=~ "^  (.*): (.*)" line))
	 (error "Couldn't parse line: ~s" line))
       
       (setf (gethash $1 torrent-data) $2)))
   :next
    )

  (when (and id torrent-data)
    (push (cons id torrent-data) res))
  (nreverse res))

(defstruct torrent
  ;; set from info from transmission-remote
  id
  name
  filename
  percent-done
  ratio
  ratio-limit
  date-finished
  state
  hash
  tracker-name				; the name of the tracker
  tracker-instance			; the actual tracker object
  tracker-char
  tracker-seed-time
  ;; calculated info
  seed-min-time				; nil or a number of seconds to seed
  seeded-for				; how much time seeded
  seed-for				; how much time left to seed
  series-name
  season				; season #
  episode				; episode # or nil
  seasonp				; (and season (not episode))
  removed				; non-nil if removed in pass I
  never-delete
  archive
  error
  )

(defvar *torrents*
    ;; A list of torrents which are seeding.  It's created in pass I and
    ;; used in pass II.  It maps the "Name" in the torrent file to a
    ;; torrent structure object.
    nil)

(defvar *video-types* '("avi" "mp4" "mkv" "wmv" "ts" "flv" "m4v"))

;; Forward references to a few things in tget.cl, prevent compiler warning
(eval-when (compile)
  (declaim (ftype (function)
		  query-series-name-to-series
		  series-never-delete
		  series-archive
		  series-name
		  canonicalize-series-name
		  query-episode
		  delete-episode-1)))

(defun tcleanup-transmission
    (&optional remove-bad
     &aux (default-seed-ratio
	      1.04
	      ;; BOGUS: transmission-remote returns 1.04 as 1.0!! ARGH!!
	      #+ignore
	      (or
	       (dolist (line (tm "-t" "all" "-si"))
		 (when (=~ "^  Default seed ratio limit: ([0-9]+\\.[0-9]+)$"
			   line)
		   (return (read-from-string $1))))
	       (error "Couldn't find the default seed ration limit")))
	  print-done
	  print-other)
  "When REMOVE-BAD is given, then only remove torrents which are not
registered with the tracker.  These are torrents which have been removed
and we should abandon them and delete the episode."
  (declare (special user::*all-trackers*))

  (with-verbosity 2
    (format t "Default seed ratio limit: ~,2f~%" default-seed-ratio))    

  (setq *now* (get-universal-time))
  
  (setq *torrents* (make-hash-table :size 777 :test #'equal))

  (dolist (info (init-torrent-data))
    (let* ((data (cdr info))
	   (name (get-torrent-info "Name" data))
	   (location (get-torrent-info "Location" data))
	   (torrent
	    (make-torrent
	     :id (car info)
	     :name name
	     :filename name
	     :percent-done (get-torrent-info "Percent Done" data)
	     :ratio (get-torrent-info "Ratio" data)
	     :ratio-limit (get-torrent-info "Ratio Limit" data)
	     :date-finished (get-torrent-info "Date finished" data
					      :missing-ok t)
	     :state (get-torrent-info "State" data)
	     :hash (get-torrent-info "Hash" data)
	     :tracker-seed-time (get-torrent-info "Seeding Time" data
						  :missing-ok t)
	     :error (get-torrent-info "Tracker gave an error" data
				      :missing-ok t))))
      
      ;; used by PART II
      (let (path)
	(if* (and location name
		  (setq path (format nil "~a/~a" location name))
		  (file-directory-p path))
	   then (setq path (pathname-as-directory path))
		(with-verbosity 2
		  (format t ";; Torrent directory:~%"))
		(dolist (file (directory path))
		  (when (member (pathname-type file) *video-types*
				:test #'equalp)
		    (with-verbosity 2
		      (format t ";;   file: ~a~%" (file-namestring file)))
		    (setf (gethash (file-namestring file) *torrents*)
		      torrent)))
	   else (setf (gethash name *torrents*) torrent)))
      
      (multiple-value-bind (series-name season episode)
	  (extract-episode-info-from-filename (torrent-filename torrent)
					      :episode-required nil)
	(let ((s (query-series-name-to-series series-name)))
	  (when s
	    (setf (torrent-never-delete torrent) (series-never-delete s))
	    (setf (torrent-archive torrent) (series-archive s))))
	(when series-name
	  (if* season
	     then (let (pretty)
		    (setq pretty
		      (season-and-episode-to-pretty-epnum season episode))
		    (setf (torrent-name torrent)
		      (format nil "~a ~a" series-name pretty))
		    (setf (torrent-season torrent) season)
		    (setf (torrent-episode torrent) episode)
		    (setf (torrent-seasonp torrent)
		      (and season (null episode))))
	     else (setf (torrent-name torrent) series-name))
	  (setf (torrent-series-name torrent) series-name)))
      
      (when (and (torrent-error torrent)
		 (=~ "(not registered with this tracker|Unregistered torrent)"
		     (torrent-error torrent)))
	;; Since the torrent is "unregistered", delete it and the episode
	;; in the db.
	(push (cons torrent :error) print-done)
	(remove-torrent torrent :delete-episode t)
	(go :next))
      (when remove-bad
	;; we're just removing torrents with errors and nothing more, so
	;; skip the rest
	(go :next))

      (when (string/= "100%" (torrent-percent-done torrent))
	;; skip it since it's not done
	(with-verbosity 1
	  (push (cons torrent :incomplete) print-other))
	(go :next))
      
;;;; Setup torrent data
    
      (setf (torrent-ratio-limit torrent)
	(if* (or (string= "Default" (torrent-ratio-limit torrent))
		 (string= "Unlimited" (torrent-ratio-limit torrent)))
	   then default-seed-ratio
	   else (read-from-string (torrent-ratio-limit torrent))))
    
      (setf (torrent-ratio torrent)
	(read-from-string (torrent-ratio torrent)))

      (when (torrent-date-finished torrent)
	(setf (torrent-date-finished torrent)
	  (string-trim '(#\space) (torrent-date-finished torrent)))
	(setf (torrent-date-finished torrent)
	  (or (string-to-universal-time (torrent-date-finished torrent))
	      (error "Couldn't parse date: ~s"
		     (torrent-date-finished torrent))))

	;; The number of seconds we have been seeding this torrent
	(if* (and (string= "Finished" (torrent-state torrent))
		  ;; Only private trackers provide this:
		  (torrent-tracker-seed-time torrent))
	   then ;; use the tracker's value of time seeded, if it's available
		(when (not (=~ "\\((\\d+) seconds\\)"
			       (torrent-tracker-seed-time torrent)))
		  (error "Could not parse tracker 'Seeding Time': ~a."
			 (torrent-tracker-seed-time torrent)))
		(setf (torrent-seeded-for torrent)
		  (parse-integer $1))
	   else ;; still seeding or done but tracker didn't tell us how
		;; long it was seeded
		(setf (torrent-seeded-for torrent)
		  (- *now* (torrent-date-finished torrent)))))
      
      (let ((name (transmission-filename-to-tracker (torrent-filename torrent)
					  :hash (torrent-hash torrent)
					  :debug *debug*)))
	(setf (torrent-tracker-name torrent) name)
	(when name
	  (setf (torrent-tracker-instance torrent)
	    (tracker-name-to-instance name))))

;;;; Use torrent data to determine status
      
      ;; Determine if this torrent is "done" and can be removed.
      ;; Usually this is if either 1) seeding is complete, or 2) we have
      ;; seeded the torrent for *minimum-seed-seconds* seconds, but there
      ;; are exceptions and complications.  See below for the exact rules.
      
      ;; Check for handlers for specific trackers.  If they are, give
      ;; the handler the `torrent' object and let it possibly set
      ;; various times.
      ;;
      (if* (null (torrent-tracker-name torrent))
	 thenret ;; (warn "null tracker: ~a" torrent)
       elseif (dolist (tracker *all-trackers* t)
		(when (match-re (tracker-re tracker)
				(torrent-tracker-name torrent)
				:return nil)
		  (setf (torrent-tracker-char torrent) (tracker-char tracker))
		  (funcall (tracker-setter tracker) torrent)
		  (return nil)))
	 then ;; Didn't match a tracker
	      (warn "Couldn't match tracker (~a) for ~a."
		    (torrent-tracker-name torrent)
		    (torrent-name torrent)))
      
      (when (symbolp (torrent-ratio torrent))
	;; Inf or None, either way, call ratio 0.0.
	(setf (torrent-ratio torrent) 0.0))
      
      ;; First, determine if seeding is complete.
      ;; transmission-remote doesn't give us a "seeding complete"
      ;; indication, so we use "Ratio" >= "Ratio Limit".
      (when (or (>= (torrent-ratio torrent)
		    (torrent-ratio-limit torrent))
		(string= "Finished" (torrent-state torrent)))
	(push (cons torrent :complete-ratio) print-done)
	(remove-torrent torrent)
	(go :next))

      ;; Seeding is not complete.  See if we've seeded for the minimum
      ;; amount of time.

      (if* (null (torrent-seeded-for torrent))
	 then (push (cons torrent :seeding) print-other)
       elseif (> (torrent-seeded-for torrent)
		 (or
		  ;; Prefer the torrent-specific value over the global one
		  (torrent-seed-min-time torrent)
		  *minimum-seed-seconds*))
	 then (push (cons torrent :complete-time) print-done)
	      (remove-torrent torrent)
	 else (with-verbosity 1
		(setf (torrent-seed-for torrent)
		  (- (or (torrent-seed-min-time torrent)
			 *minimum-seed-seconds*)
		     (torrent-seeded-for torrent)))
		(push (cons torrent :seeding) print-other)))
      
      )
   :next
    )

  (when print-done
    (setq print-done (nreverse print-done))
    (when (not remove-bad)
      (if* *remove-seeded*
	 then (format t "These torrents were removed:~%~%")
	 else (format t "These torrents are complete:~%~%")))
    (let ((header t))
      (dolist (item print-done)
	(destructuring-bind (torrent . status) item
	  (print-torrent torrent status :brief t :header header)
	  (setq header nil))) ))
  
  (when (not remove-bad)
    (when print-other
      (setq print-other (nreverse print-other))
      (format t "~%These torrents are incomplete:~%~%")
      (let ((header t))
	(dolist (item print-other)
	  (destructuring-bind (torrent . status) item
	    (print-torrent torrent status :brief t :header header)
	    (setq header nil)))))))

(defun print-torrent (torrent status
		      &key brief header
		      &aux (name (torrent-name torrent)))
  (and *debug* (format t "~s~%" torrent))
  (cond
   (brief
    (when header
      (format t "~2a~41a~6a~6a~12a~12a~%"
	      "T" "name" "%done" "ratio" "seeded" "left"))
    (format t "~2a~41a~6a~@[~6a~]~@[~12a~]~@[~12@a~]~%"
	    (or (torrent-tracker-char torrent) "")
	    ;; truncate to 40
	    (if (> (length name) 40) (subseq name 0 40) name)
	    (torrent-percent-done torrent)
	    (if* (eq :error status)
	       then "Error"
	     elseif (not (eq :incomplete status))
	       then (format nil "~,2f" (torrent-ratio torrent)))
	    (if* (eq :error status)
	       then "Error"
	     elseif (not (eq :incomplete status))
	       then (relative-time-formatter (torrent-seeded-for torrent)
					     :brief t))
	    (when (eq :seeding status)
	      (relative-time-formatter (torrent-seed-for torrent)
				       :brief t))))
   (t
    (format t "~%~a~%" name)
    (ecase status
      (:error (format t "  ERROR: ~a~%" (torrent-error torrent)))
      (:incomplete
       (format t "  incomplete: ~a done~%" (torrent-percent-done torrent)))
      (:complete-ratio
       (format t "  COMPLETE: seeded to ~,2f (seeded for ~a)~%"
	       (torrent-ratio torrent)
	       (relative-time-formatter (torrent-seeded-for torrent))))
      (:complete-time
       (format t "  COMPLETE: seeded for ~a (ratio: ~,2f)~%"
	       (relative-time-formatter (torrent-seeded-for torrent))
	       (torrent-ratio torrent)))
      (:seeding
       (format t "  incomplete: ratio: ~,2f (target ~,2f)~%"
	       (torrent-ratio torrent)
	       (torrent-ratio-limit torrent))
       (format t "              seeded for ~a~%"
	       (relative-time-formatter (torrent-seeded-for torrent)))
       (format t "              to go: ~a~%"
	       (relative-time-formatter (torrent-seed-for torrent))))))))

(defun relative-time-formatter (seconds &key brief)
  (if* (null seconds)
     then "---"
   elseif brief
     then (if* (> seconds #.(* 3600 24))
	     then (universal-time-to-string
		   (+ *now* seconds)
		   :relative *now*
		   :format (ut-to-string-formatter "%Dd %2H:%2M:%2S"))
	     else (universal-time-to-string
		   (+ *now* seconds)
		   :relative *now*
		   :format (ut-to-string-formatter "%2H:%2M:%2S")))
   elseif (> seconds #.(* 3600 24))
     then ;; more than a day, included days
	  (universal-time-to-string
	   (+ *now* seconds)
	   :relative *now*
	   :format (ut-to-string-formatter "%D day%p, %2H:%2M:%2S"))
     else (universal-time-to-string
	   (+ *now* seconds)
	   :relative *now*
	   :format (ut-to-string-formatter "%2H:%2M:%2S"))))

(defun remove-torrent (torrent &key delete-episode &aux res)
  (when (or delete-episode *remove-seeded*)
    (if* (and (setq res (tm "-t" (torrent-id torrent) "-r"))
	      (=~ "success" (car res)))
       then (setf (torrent-removed torrent) t)
	    t
       else (error "Failed to remove ~a." (torrent-filename torrent)))
    
    ;; If the torrent became `unregistered' then delete it, since there was
    ;; something wrong with it.
    (when delete-episode
      (let* ((series-name (torrent-series-name torrent))
	     (season (torrent-season torrent))
	     (epnum (torrent-episode torrent))
	     series
	     ep)
	(if* (and series-name season epnum)
	   then (setq series-name (canonicalize-series-name series-name))
		(if* (setq series (query-series-name-to-series series-name))
		   then (setq ep
			  (query-episode :series-name (series-name series)
					 :season season
					 :ep-number epnum))
			(if* (and ep (cdr ep))
			   then (warn "remove-torrent: got more than one ep: ~s"
				      ep)
			 elseif ep
			   then (setq ep (car ep))
				(format t "~
NOTE: removing broken episode:
        ~a
      Do \"tget --run\" to see if it is downloaded, and, if not,
      download it manually.~%"
					ep)
				(delete-episode-1 ep)
			   else (warn "remove-torrent: unknown ep: ~s ~s ~s"
				      series season epnum))
		   else (warn "remove-torrent: could not find series from ~s"
			      series-name))
	   else (warn "remove-torrent: could not find episode from torrent: ~s"
		      torrent))))))

(defun get-torrent-info (key hash &key missing-ok)
  (or (gethash key hash)
      (if* missing-ok
	 then nil
	 else (error "Couldn't find ~a in torrent data" key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART II: remove files which have been watched
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variables from the config file

(defvar *ignore-watched-within*
    "Ignore any torrents which have been watched within this number of hours.
The default is 72 hours, or 3 days."
  #.(* 24 3))

(defvar *sqlite3-binary* "sqlite3")

(defvar *watch-directories* nil)

(defvar *plex-db*
    "/me/tplex/plex-config/Library/Application Support/Plex Media Server/Plug-in Support/Databases/com.plexapp.plugins.library.db")

(defvar *watched-hash-table*
    ;; Hash table:
    ;;   key=  :: path to video
    ;;   value :: hours since video was watched
    nil)

(defvar *plex-files-hash-table*
    ;; Hash table:
    ;;   key   :: path to video
    ;;   value :: path to video
    nil)

(defun tcleanup-files (&aux (initial-newline t) header
			    (symlink-pass t)
			    torrent)
  (flet ((announce (format-string &rest args)
	   (when initial-newline
	     (format t "~%")
	     (setq initial-newline nil))
	   (when header
	     (format t "~a~%" header)
	     (setq header nil))
	   (apply #'format t format-string args)))

    ;; Initialize a hash table of watched videos and files in plex
    (initialize-watched)
    (initialize-plex-files)
    
;;;;TODO: a bunch of files aren't being archived properly... debug this!!!

    (dolist (thing *watch-directories*)
      (destructuring-bind (directory . label) thing
	(setq header (format nil "~a:" label))
	(map-over-directory
	 (lambda (p &aux series-name series never-delete archive)
	   ;; Check for NEVER-DELETE and ARCHIVE options.  If there's not
	   ;; an active torrent with the information, need to query it from
	   ;; the raw filename.
	   (if* (setq torrent (gethash (file-namestring p) *torrents*))
	      then (setq never-delete (torrent-never-delete torrent))
		   (setq archive (torrent-archive torrent))
	    elseif (and (setq series-name
			  (extract-episode-info-from-filename
			   (file-namestring p)
			   :episode-required nil))
			(setq series
			  (query-series-name-to-series series-name)))
	      then (setq never-delete (series-never-delete series))
		   (setq archive (series-archive series)))

	   (when (and archive (not (probe-file archive)))
	     (.error "archive directory does not exist: ~a.~%" archive))
	   
	   (cond
	    (never-delete (announce "NEVER DELETE: ~a~%" (file-namestring p)))

	    ((member (pathname-type p) *video-types* :test #'equalp)
	     (multiple-value-bind (ready-to-remove reason) (watchedp p)
	       (if* ready-to-remove
		  then (if* *remove-watched*
			  then (cleanup-file p #'announce :move-to archive)
			elseif archive
			  then (announce "ARCHIVE ~a:~a~%"
					 reason (file-namestring p))
			  else (announce "YES ~a:~a~%"
					 reason (file-namestring p)))
		elseif (null reason)
		  thenret ;; not watched
		  else (with-verbosity 1
			 ;; Watched, but not ready to remove for some reason
			 (announce "NO ~a:~a~%" reason (file-namestring p)))))
	     
	     (when (and (not (gethash (namestring p) *plex-files-hash-table*))
			(probe-file p))
	       (announce "HIDDEN: ~a~%" (file-namestring p))))

	    ((or (equalp "srt" (pathname-type p))
		 (equalp "idx" (pathname-type p))
		 (equalp "sub" (pathname-type p)))
	     (when (not (find-video-match-for-srt p))
	       (if* *remove-watched*
		  then (cleanup-file p #'announce :move-to archive)
		elseif archive
		  then (announce "ARCHIVE: ~a~%" (file-namestring p))
		  else (announce "YES: ~a~%" (file-namestring p)))))

	    ((or (match-re "\\.ds_store" (file-namestring p)
			   :case-fold t :return nil)
		 (member (pathname-type p) '("iso" "nfo" "rar" "sfv" "part"
					     "jpg"
					     ;; for RARBG
					     "exe" "txt")
			 :test #'equalp)
		 ;; rar file parts:
		 (match-re "^r\\d\\d$" (pathname-type p) :return nil))
	     ;; ignore files
	     )

	    (t (announce "unknown: ~a~%" (file-namestring p)))))

	 (pathname-as-directory directory)
	 :recurse t
	 :include-directories nil)

	(when symlink-pass
	  ;; Look for bad symbolic links, now that we've possible removed some
	  ;; files.
	  (map-over-directory
	   (lambda (p)
	     (when (and (symbolic-link-p p) (not (probe-file p)))
	       (announce "SYMLINK: ~a~%" (file-namestring p))
	       (delete-file p)))
	   (pathname-as-directory directory)
	   :recurse t
	   :include-directories nil))))))

(defun find-video-match-for-srt (p &aux temp)
  (flet ((simple-match (p)
	   (dolist (type *video-types*)
	     (when (or
		    (probe-file
		     (merge-pathnames (make-pathname :type type) p))
		    ;; sometimes srt's are in subdirs
		    (probe-file
		     (merge-pathnames
		      (merge-pathnames (make-pathname :type type)
				       (file-namestring p))
		      (merge-pathnames "../" (path-namestring p)))))
	       ;; The easy case, named the same as the srt file
	       (return t))))
	 (reduce-filename (p)
	   ;; Given a filename like
	   ;;   "Foo 720p H264 AC3 - CODY.Portuguese-Bra.srt"
	   ;; return
	   ;;   "Foo 720p H264 AC3 - CODY.srt"
	   (let ((name (pathname-name p)))
	     (when (setq temp (position #\. name :from-end t))
	       (merge-pathnames 
		(make-pathname :name (subseq name 0 temp))
		p))))
	 )
    ;; When given an srt file, see if there is an existing video to match
    ;; it.
    (if* (or (simple-match p)
	     ;; See if a reduced version of the filename has a
	     ;; corresponding video file
	     (and (setq temp (reduce-filename p))
		  (simple-match temp)))
       thenret 
     elseif (and (parent-is-named-p p "Subs")
		 ;; RARBG torrents are structured this way
		 (find-sole-file-of-type (parent-directory p) *video-types*))
       thenret
       else (format t "didn't find video for p=~s~%" p)
	    nil)))

(defun parent-is-named-p (path name)
  (equalp (car (last (pathname-directory path)))
	  name))

(defun parent-directory (path)
  (merge-pathnames "../" (path-pathname path)))

(defun find-sole-file-of-type (path types)
  (let ((count 0) match)
    (dolist (p (directory path))
      (when (member (pathname-type p) types :test #'equalp)
	(incf count)
	(setq match p)))
    (and (= count 1)
	 match)))

(defun cleanup-file (p announce
		     &key move-to
		     &aux aux-p rar)
  (flet ((df (p)
	   (when (probe-file p)
	     (if* move-to
		then (funcall announce "~@[Would do:~* ~]mv ~a ~a~%"
			      *debug* p move-to)
		     (when (not *debug*)
		       (rename-file p
				    (merge-pathnames
				     (file-namestring p)
				     (pathname-as-directory move-to))))
		else (funcall announce "~@[Would do:~* ~]rm ~a~%" *debug* p)
		     (when (not *debug*)
		       (delete-file p))))))
    (if* (setq aux-p (symbolic-link-p p))
       then ;; delete the symlink and what it points to
	    (setq aux-p (merge-pathnames aux-p p))
	    (df aux-p)
	    (df p)
     elseif (or (probe-file (setq rar (merge-pathnames #p(:type "rar") p)))
		(probe-file (fiddle-case-filename rar :downcase))
		(probe-file (fiddle-case-filename rar :upcase)))
       then ;; the watched file came from a downloaded rar file, remove
	    ;; everything else, too
	    (let* ((name (pathname-name p))
		   (wildcard (merge-pathnames (merge-pathnames "*.*" p)))
		   (files (directory wildcard)))
	      (dolist (file files)
		(when (equalp name (pathname-name file))
		  (df file))))
	    (df p)
       else (df p))))

(defun fiddle-case-filename (p direction)
  (merge-pathnames
   (make-pathname
    :name (funcall (if* (eq :upcase direction)
		      then #'string-upcase
		      else #'string-downcase)
		   (pathname-name p))
    :type (pathname-type p))
   p))

(defun initialize-watched ()
  (or (probe-file *plex-db*)
      (error "Couldn't find PMS database."))
  (or (excl.osi:find-in-path *sqlite3-binary*)
      (error "Couldn't find command ~s." *sqlite3-binary*))
  
  (setq *watched-hash-table*
    (if* *watched-hash-table*
       then (clrhash *watched-hash-table*)
       else (make-hash-table :size 777 :test #'equal)))
  (let* ((sqlite-cmd
	  ;; Use a vector so shell escaping isn't an issue, with spaces
	  ;; in those filenames, etc.
	  (vector *sqlite3-binary* "-csv" (namestring (truename *plex-db*))))
	 (nl #\newline)
	 (sql
	  (util.string:string+
	   "select p.file,s.last_viewed_at" nl
	   "from media_parts p, media_items mi, metadata_items md,metadata_item_settings s" nl
	   "where mi.id = p.media_item_id AND" nl
	   "   md.id = mi.metadata_item_id AND" nl
	   "   md.guid = s.guid AND" nl
	   "   s.view_count > 0;" nl))
	 lines)
    (multiple-value-bind (stdout stderr exit-code)
	(command-output sqlite-cmd :input sql :whole t)
      (if* (/= 0 exit-code)
	 then (error "exit code is ~s: stdout is ~a, stderr is: ~a."
		     exit-code stdout stderr)
       elseif (or (null stdout) (string= "" stdout))
	 then ;; No watched shows??  I guess it's possible
	      (return-from initialize-watched nil))
	  
      (when (not (setq lines (split-re "$" stdout :multiple-lines t)))
	(error "could not split sqlite3 output."))

      (dolist (line lines)
	(when (=~ "^\s*$" line) (return))
	(when (not (=~ "\\s*(.*)\\|(.*)\\s*" line))
	  (error "Could not parse sqlite3 output: ~a." line))
	;;(format t "file=~s~%" $1)
	;;(format t "  exists=~s~%" (probe-file $1))
	(let* ((file $1)
	       (date (or (excl:string-to-universal-time $2)
			 (error "couldn't parse date: ~a." $2)))
	       (hours (truncate (/ (- *now* date) 3600))))
	  (with-verbosity 2
	    (format t "add watched: ~s, ~s~%" file hours))
	  (setf (gethash file *watched-hash-table*) hours))))))

(defun initialize-plex-files ()
  (or (probe-file *plex-db*)
      (error "Couldn't find PMS database."))
  (or (excl.osi:find-in-path *sqlite3-binary*)
      (error "Couldn't find command ~s." *sqlite3-binary*))
  
  (setq *plex-files-hash-table*
    (if* *plex-files-hash-table*
       then (clrhash *plex-files-hash-table*)
       else (make-hash-table :size 777 :test #'equal)))
  (let* ((sqlite-cmd
	  ;; Use a vector so shell escaping isn't an issue, with spaces
	  ;; in those filenames, etc.
	  (vector *sqlite3-binary* "-csv" (namestring (truename *plex-db*))))
	 (nl #\newline)
	 (sql (util.string:string+ "select p.file from media_parts p;" nl))
	 files)
    (multiple-value-bind (stdout stderr exit-code)
	(command-output sqlite-cmd :input sql :whole t)
      (if* (/= 0 exit-code)
	 then (error "exit code is ~s: stderr is: ~a." exit-code stderr)
       elseif (or (null stdout) (string= "" stdout))
	 then ;; No files??  I guess it's possible
	      (return-from initialize-plex-files nil))
	  
      (when (not (setq files (split-re "$" stdout :multiple-lines t)))
	(error "could not split sqlite3 output."))
      
      (dolist (file files)
	(when (=~ "^\s*$" file) (go skip))
	(setq file (string-trim '(#\space #\newline) file))
	(when (file-in-watched-directory-p file)
	  (with-verbosity 2
	    (format t "add file: ~s~%" file))
	  (setf (gethash file *plex-files-hash-table*) file))
       skip
	))))

(eval-when (eval load compile) (require :strlib))

(defun file-in-watched-directory-p (file)
  (dolist (xx *watch-directories*)
    (when (prefixp (car xx) file)
      (return t))))

#+ignore ;; unused, keep tho
(defun escape-for-sqlite (filename)
  ;; single quotes are doubled
  (replace-re (namestring filename) "'" "''"))

(defun watchedp (p &aux (file (namestring p)))
  ;; Return non-nil if the video given by P (a pathname) has been watched.
  ;; Return values are: ready-to-remove description
  ;;
  (let ((hours (gethash file *watched-hash-table*)))
    ;; Return if not watched
    (when (not hours) (return-from watchedp nil))
    ;; Return if seeding
    (when (seedingp file)
      (return-from watchedp (values nil "seeding")))

    (when (< hours *ignore-watched-within*)
      (return-from watchedp
	(values nil
		(format nil "~dh<~dh" hours *ignore-watched-within*))))

    ;; Meets our time-based criteria for removal
    (return-from watchedp
      (values
       t ;; yes, remove it
       (if* (> hours 24)
	  then (format nil ">~dd" (truncate (/ hours 24)))
	  else (format nil ">~dh" hours))))))

(defun seedingp (file &aux name)
  ;; Return non-nil if we are seeding FILE.  Need to be careful, though,
  ;; since we might be looking at a symlink and not the original file being
  ;; seeded.
  (if* (setq name (symbolic-link-p file))
     then (setq name (file-namestring name))
     else (setq name (file-namestring file)))
  (let ((torrent (gethash name *torrents*)))
    (and torrent (null (torrent-removed torrent)))))
