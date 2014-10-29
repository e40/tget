
(eval-when (compile eval load)
  (require :anydate)
  (require :osi)
  (require :shell)
  (require :tget-utils "utils.fasl"))

(defpackage :seedstatus
  (:use #:common-lisp
	#:excl
	#:excl.osi
	#:excl.shell)
  (:import-from #:cl-user
		#:with-verbosity
		#:*verbose*))

(in-package :seedstatus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user tweakable variables

;; Minimum seed time in seconds: 3 days, for torrents that don't have a
;; tracker-specific rule that takes precedence over this value.
(defparameter *seedmin* (* 3600 24 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *remove* nil)
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
    (and *debug* (format t "~s line=~s~%" state words))
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
    
  (push (cons id torrent-data) res)
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
  tracker
  ;; calculated info
  seed-min-time				; nil or a number of seconds to seed
  seeded-for				; how much time seeded
  seed-for				; how much time left to seed
  series-name
  season				; season #
  episode				; episode # or nil
  seasonp				; (and season (not episode))
  )

(defvar *now* nil)

(defun seedstatus
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
	  btn
	  mma-tracker
	  print-done
	  print-other)
  (with-verbosity 2
    (format t "Default seed ratio limit: ~,2f~%" default-seed-ratio))    

  (setq *now* (get-universal-time))

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
	     :state (get-torrent-info "State" data))))
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
	  (setf (torrent-seasonp torrent) (and season (null episode)))))
      
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
	(setf (torrent-seeded-for torrent)
	  (- *now* (torrent-date-finished torrent))))
      
      (setf (torrent-tracker torrent)
	(user::transmission-filename-to-tracker (torrent-filename torrent)
						:debug *debug*))
      
      (when (torrent-tracker torrent)
	(setq btn (match-re "broadcasthe.net" (torrent-tracker torrent)))
	(setq mma-tracker
	  (match-re "mma-tracker.net" (torrent-tracker torrent))))

;;;; Use torrent data to determine status
      
      ;; Determine if this torrent is "done" and can be removed.
      ;; Usually this is if either 1) seeding is complete, or 2) we have
      ;; seeded the torrent for *seedmin* seconds, but there are exceptions
      ;; and complications.  See below for the exact rules.

      ;; Special seeding rules for some sites
      ;;
      (if* btn
	 then ;; The BTN tracker and Transmission often disagree on the
	      ;; amount I've uploaded, so set ratio-limit to 1.5 to be
	      ;; safe.
	      (setf (torrent-ratio-limit torrent) 1.50))
      
      (if* (and btn (torrent-seasonp torrent))
	 then ;; Seed for a week + slop 
	      (setf (torrent-seed-min-time torrent)
		(* 3600 24 9))
       elseif btn
	 then ;; The rules state you need to seed to 1:1 or 24 hours.  Give
	      ;; it some slop, to make sure I don't get a H&R
	      ;; NOTE: the BTN tracker is notorious for not counting seed
	      ;;       time, so use 3d as the minimum time here to make
	      ;;       sure we don't get a H&R
	      (setf (torrent-seed-min-time torrent) (* 3600 24 3))
       elseif mma-tracker
	 then ;; Hard to seed stuff here, so seed longer.
	      (setf (torrent-seed-min-time torrent) (* 3600 24 4))
	      (setf (torrent-ratio-limit torrent) 1.5))
      
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
	       *seedmin*))
	 then (push (cons torrent :complete-time) print-done)
	      (remove-torrent torrent)
	 else (with-verbosity 1
		(setf (torrent-seed-for torrent)
		  (- (or (torrent-seed-min-time torrent) *seedmin*)
		     (torrent-seeded-for torrent)))
		(push (cons torrent :seeding) print-other))))
   :next
    )

  (when print-done
    (setq print-done (nreverse print-done))
    (if* *remove*
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
  (and *debug* (format t "~a~%" torrent))
  (cond
   (brief
    (when header
      (format t "~36a~6a~6a~12a~12a~%"
	      "name" "%done" "ratio" "seeded" "left"))
    (format t "~36a~6a~@[~6a~]~@[~12a~]~@[~12@a~]~%"
	    name
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
  (when *remove*
    (if* (and (setq res (tm "-t" (torrent-id torrent) "-r"))
	      (=~ "success" (car res)))
       then t
       else (error "Failed to remove ~a."
		   (torrent-filename torrent)))))

(defun get-torrent-info (key hash &key missing-ok)
  (or (gethash key hash)
      (if* missing-ok
	 then nil
	 else (error "Couldn't find ~a in torrent data" key))))

(defun user::main ()
  (system:with-command-line-arguments
      (("info" :long info)
       ("v" :short verbose :allow-multiple-options)
       ("d" :short debug)
       ("q" :short quiet)
       ("r" :short remove))
      (rest)
    (declare (ignore rest))
    (when info
      (dolist (line (tm "-t" "all" "--info"))
	(format t "~a~%" line))
      (exit 0))
    (when debug (setq *debug* t))
    (setq *verbose* (or verbose
			(when quiet 0)
			1))
    (when remove (setq *remove* t))
    (handler-case (seedstatus)
      (error (c)
	(format t "Error: ~a" c)
	(exit 1)))
    (exit 0)))
