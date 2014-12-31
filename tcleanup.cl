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

(eval-when (compile eval load)
  (require :anydate)
  (require :osi)
  (require :shell)
  (require :autozoom)
  (require :tget-utils "utils.fasl"))

(defpackage :tcleanup
  (:use #:common-lisp
	#:excl
	#:excl.osi
	#:excl.shell)
  (:import-from #:cl-user
		#:with-verbosity
		#:*verbose*))

(in-package :tcleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART I: remove torrents from Transmission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variables from the config file

(defvar *minimum-seed-seconds* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *config-file* (list "sys:tcleanup-config.cl" "~/.tcleanup.cl"))

(defparameter *trackers* nil)

(defstruct tracker
  re
  char
  setter)

(defmacro deftracker (re char setter)
  `(push (make-tracker :re ,re :char ,char :setter ,setter)
	 *trackers*))

(defvar *remove-seeded* nil)
(defvar *remove-watched* nil)
(defvar *debug* nil)

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
  tracker
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
  )

(defvar *now* nil)

(defvar *torrents*
    ;; A list of torrents which are seeding.  It's created in pass I and
    ;; used in pass II.  It maps the "Name" in the torrent file to a
    ;; torrent structure object.
    nil)

(defun tcleanup-transmission
    (&aux (default-seed-ratio
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

  (with-verbosity 2
    (format t "Default seed ratio limit: ~,2f~%" default-seed-ratio))    

  (setq *now* (get-universal-time))
  
  (setq *torrents* (make-hash-table :size 777 :test #'equal))

  (dolist (info (init-torrent-data))
    (let* ((data (cdr info))
	   (name (get-torrent-info "Name" data))
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
						  :missing-ok t))))
      
      ;; used by PART II
      (setf (gethash name *torrents*) torrent)
      
      (multiple-value-bind (series-name season episode)
	  (user::extract-episode-info-from-filename (torrent-filename torrent)
						    :episode-required nil)
	(when series-name
	  (let ((pretty (user::season-and-episode-to-pretty-epnum
			 season episode)))
	    (setf (torrent-name torrent)
	      (format nil "~a ~a" series-name pretty)))
	  (setf (torrent-series-name torrent) series-name)
	  (setf (torrent-season torrent) season)
	  (setf (torrent-episode torrent) episode)
	  (setf (torrent-seasonp torrent)
	    (and season
		 (null episode)
		 ;; episode might be null if this is a special:
		 (not (match-re "special" (torrent-filename torrent)
				:case-fold t :return nil))))))
      
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
      
      (setf (torrent-tracker torrent)
	(user::transmission-filename-to-tracker (torrent-filename torrent)
						:hash (torrent-hash torrent)
						:debug *debug*))

;;;; Use torrent data to determine status
      
      ;; Determine if this torrent is "done" and can be removed.
      ;; Usually this is if either 1) seeding is complete, or 2) we have
      ;; seeded the torrent for *minimum-seed-seconds* seconds, but there
      ;; are exceptions and complications.  See below for the exact rules.
      
      ;; Check for handlers for specific trackers.  If they are, give
      ;; the handler the `torrent' object and let it possibly set
      ;; various times.
      ;;
      (if* (null (torrent-tracker torrent))
	 thenret ;; (warn "null tracker: ~a" torrent)
       elseif (dolist (tracker *trackers* t)
		(when (match-re (tracker-re tracker) (torrent-tracker torrent)
				:return nil)
		  (setf (torrent-tracker-char torrent) (tracker-char tracker))
		  (funcall (tracker-setter tracker) torrent)
		  (return nil)))
	 then ;; Didn't match a tracker
	      (warn "Couldn't match tracker (~a) for ~a."
		    (torrent-tracker torrent)
		    (torrent-name torrent)))

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
      
      (if* (> (torrent-seeded-for torrent)
	      (or
	       ;; Prefer the torrent-specific value over the global one
	       (torrent-seed-min-time torrent)
	       *minimum-seed-seconds*))
	 then (push (cons torrent :complete-time) print-done)
	      (remove-torrent torrent)
	 else (with-verbosity 1
		(setf (torrent-seed-for torrent)
		  (- (or (torrent-seed-min-time torrent) *minimum-seed-seconds*)
		     (torrent-seeded-for torrent)))
		(push (cons torrent :seeding) print-other))))
   :next
    )

  (when print-done
    (setq print-done (nreverse print-done))
    (if* *remove-seeded*
       then (format t "These torrents were removed:~%~%")
       else (format t "These torrents are complete:~%~%"))
    (let ((header t))
      (dolist (item print-done)
	(destructuring-bind (torrent . status) item
	  (print-torrent torrent status :brief t :header header)
	  (setq header nil))) ))
  
  (when print-other
    (setq print-other (nreverse print-other))
    (format t "~%These torrents are incomplete:~%~%")
    (let ((header t))
      (dolist (item print-other)
	(destructuring-bind (torrent . status) item
	  (print-torrent torrent status :brief t :header header)
	  (setq header nil))))))

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
	    (when (not (eq :incomplete status))
	      (format nil "~,2f" (torrent-ratio torrent)))
	    (when (not (eq :incomplete status))
	      (relative-time-formatter (torrent-seeded-for torrent)
				       :brief t))
	    (when (eq :seeding status)
	      (relative-time-formatter (torrent-seed-for torrent)
				       :brief t))))
   (t
    (format t "~%~a~%" name)
    (ecase status
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
  (if* brief
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

(defun remove-torrent (torrent &aux res)
  (when *remove-seeded*
    (if* (and (setq res (tm "-t" (torrent-id torrent) "-r"))
	      (=~ "success" (car res)))
       then (setf (torrent-removed torrent) t)
	    t
       else (error "Failed to remove ~a." (torrent-filename torrent)))))

(defun get-torrent-info (key hash &key missing-ok)
  (or (gethash key hash)
      (if* missing-ok
	 then nil
	 else (error "Couldn't find ~a in torrent data" key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART II: remove files which have been watched
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variables from the config file

(defvar *ignore-watched-within* nil)

(defvar *watch-directories* nil)

(defvar *plex-db*
    "~/Library/Application Support/Plex Media Server/Plug-in Support/Databases/com.plexapp.plugins.library.db")

(defvar *watched-hash-table*
    ;; Hash table:
    ;;   key=  :: path to video
    ;;   value :: hours since video was watched
    nil)

(defvar *video-types* '("avi" "mp4" "mkv" "wmv"))

(defun tcleanup-files (&aux (initial-newline t) header)
  (flet ((announce (format-string &rest args)
	   (when initial-newline
	     (format t "~%")
	     (setq initial-newline nil))
	   (when header
	     (format t "~a~%" header)
	     (setq header nil))
	   (apply #'format t format-string args)))
    ;; Initialize a hash table of watched videos.
    ;;
    (initialize-watched)
  
    (dolist (thing *watch-directories*)
      (destructuring-bind (directory . label) thing
	(setq header (format nil "~a:" label))
	(map-over-directory
	 (lambda (p)
	   (cond
	    ((member (pathname-type p) *video-types* :test #'equalp)
	     (multiple-value-bind (ready-to-remove reason) (watchedp p)
	       (if* ready-to-remove
		  then (if* *remove-watched*
			  then (announce "rm ~a~%" p)
			       (delete-file p)
			  else (announce "YES ~a:~a~%"
					 reason (file-namestring p)))
		elseif (null reason)
		  thenret ;; not watched
		  else (with-verbosity 1
			 ;; Watched, but not ready to remove for some reason
			 (announce "NO ~a:~a~%" reason (file-namestring p))))))
	    ((equalp "srt" (pathname-type p))
	     (when (not (dolist (type *video-types*)
			  (when (probe-file (merge-pathnames
					     (make-pathname :type type)
					     p))
			    (return t))))
	       (if* *remove-watched*
		  then (announce "rm ~a~%" p)
		       (delete-file p)
		  else (announce "YES: ~a~%" (file-namestring p)))))
	    ((equalp "rar" (pathname-type p))
	     (announce "unwatchable rar file ~a~%" (file-namestring p)))
	    ((or (member (file-namestring p) '(".DS_Store") :test #'equalp)
		 (equalp "iso" (pathname-type p)))
	     ;; ignore files
	     )
	    (t (announce "unknown: ~a~%" (file-namestring p)))))

	 (pathname-as-directory directory)
	 :recurse t
	 :include-directories nil)))))

(defun initialize-watched (&aux (temp-file (sys:make-temp-file-name)))
  (or (probe-file *plex-db*)
      (error "Couldn't find PMS database."))
  
  (setq *watched-hash-table*
    (if* *watched-hash-table*
       then (clrhash *watched-hash-table*)
       else (make-hash-table :size 777 :test #'equal)))
  (unwind-protect
      (let* ((sqlite-cmd
	      ;; Use a vector so shell escaping isn't an issue
	      (vector "sqlite3" "sqlite3" "-line"
		      "-init" (namestring temp-file)
		      (namestring (truename *plex-db*))))
	     lines)
	(setf (file-contents temp-file)
	  (format nil "~
select p.file,s.last_viewed_at
from media_parts p, media_items mi, metadata_items md,metadata_item_settings s
where mi.id = p.media_item_id AND
      md.id = mi.metadata_item_id AND
      md.guid = s.guid AND
      s.view_count > 0;~%"))
	(multiple-value-bind (stdout stderr exit-code)
	    (command-output sqlite-cmd :input "" :whole t)
	  (if* (/= 0 exit-code)
	     then (error "~a." stderr)
	   elseif (or (null stdout) (string= "" stdout))
	     then ;; No watched shows??  I guess it's possible
		  (return-from initialize-watched nil))
	  
	  (when (not (setq lines (split-re "$" stdout :multiple-lines t)))
	    (error "could not split sqlite3 output."))

	  (dolist (line lines)
	    (when (=~ "^\s*$" line) (return))
	    (when (not (=~ "\\s*(.*)\\|(.*)\\s*" line))
	      (error "Could not parse sqlite3 output: ~a." line))
	    (let* ((file $1)
		   (date (or (excl:string-to-universal-time $2)
			     (error "couldn't parse date: ~a." $2)))
		   (hours (truncate (/ (- *now* date) 3600))))
	      (setf (gethash file *watched-hash-table*) hours))))))
    (ignore-errors (delete-file temp-file)))

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
    (when (seedingp (file-namestring file))
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

(defun seedingp (name)
  (let ((torrent (gethash name *torrents*)))
    (and torrent (null (torrent-removed torrent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition tcleanup (error) ())

(defun .error (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'tcleanup :format-control format-string
	 :format-arguments format-arguments))

(defun user::main ()
  (flet
      ((doit ()
	 (system:with-command-line-arguments
	     (("config" :long config-file :required-companion)
	      ("d" :short debug)
	      ("h" :short ignore-watched-within :required-companion)
	      ("info" :long info)
	      ("torrents-only" :long torrents-only)
	      ("q" :short quiet)
	      ("remove" :long remove-all)
	      ("remove-seeded" :long remove-seeded)
	      ("remove-watched" :long remove-watched)
	      ("v" :short verbose :allow-multiple-options))
	     (rest)
	   (and rest (.error "extra arguments:~{ ~a~}." rest))
	   (when info
	     (dolist (line (tm "-t" "all" "--info"))
	       (format t "~a~%" line))
	     (exit 0))
	   (when config-file (setq *config-file* (list config-file)))
	   (when debug (setq *debug* t))
	   (setq *verbose* (or verbose
			       (when quiet 0)
			       1))
	   (if* remove-all
	      then (setq *remove-seeded* t
			 *remove-watched* t)
	      else (when remove-seeded (setq *remove-seeded* t))
		   (when remove-watched (setq *remove-watched* t)))
    
	   (let ((*package* (load-time-value (find-package :tcleanup))))
	     (or (dolist (config-file *config-file*)
		   (when (probe-file config-file)
		     (progn (load config-file :verbose nil) 
			    (return t))))
		 (.error "Could not find config file.")))
	   
	   ;; Do after loading config file, so it can override the value
	   ;; there
	   (when ignore-watched-within
	     (or (=~ "^\\d+$" ignore-watched-within)
		 (.error "-h value should be a number: ~a."
			 ignore-watched-within))
	     (setq *ignore-watched-within*
	       (parse-integer ignore-watched-within)))
    
	   ;; Error checking on config file:
	   ;;
	   ;; Part I:
	   (and (null *minimum-seed-seconds*)
		(.error
		 "*minimum-seed-seconds* is not defined in config file."))
	   (or (numberp *minimum-seed-seconds*)
	       (.error "*minimum-seed-seconds* is not a number: ~s."
		      *minimum-seed-seconds*))
	   ;;
	   ;; Part II:
	   (and (null *ignore-watched-within*)
		(.error
		 "*ignore-watched-within* is not defined in config file."))
	   (or (numberp *ignore-watched-within*)
	       (.error "*ignore-watched-within* isnot a number: ~s."
		      *ignore-watched-within*))
	   (and (null *watch-directories*)
		(.error "*watch-directories* is not defined in config file."))

	   (tcleanup-transmission)
	   (when torrents-only (exit 0))
	   (tcleanup-files)
    
	   (exit 0))))
    
    (if* *debug* ;; -d on command line doesn't effect this test!
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (top-level.debug:with-auto-zoom-and-exit (*standard-output*)
	      (handler-case (doit)
		(tcleanup (c)
		  ;; 'tcleanup errors don't get a backtrace, since those are
		  ;; expected or, at least, planned for.  The unexpected
		  ;; ones get the zoom.
		  (format t "~&~a~&" c)
		  (exit 1 :quiet t)))))))
