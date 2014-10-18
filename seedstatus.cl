
(eval-when (compile eval load)
  (require :anydate)
  (require :osi)
  (require :shell))

(defpackage :seedstatus
  (:use #:common-lisp
	#:excl
	#:excl.osi
	#:excl.shell))

(in-package :seedstatus)

(defvar *verbose* t)
(defvar *debug* nil)

;; user tweakable variables

;; Minimum seed time in seconds: 3 days
(defparameter *seedmin* (* 3600 24 3))

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
    ;;(format t "~s line=~s~%" state words)
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
  percent-done
  ratio
  ratio-limit
  date-finished
  tracker
  ;; calculated info
  seed-min-time				; nil or a number of seconds to seed
  seeded-for				; how much time seeded
  seed-for				; how much time left to seed
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
	  mma-tracker)
  (and *verbose*
       (format t "Default seed ratio limit: ~,2f~%"
	       default-seed-ratio))
  
  (setq *now* (get-universal-time))

  (dolist (info (init-torrent-data))
    (let* ((data (cdr info))
	   (torrent
	    (make-torrent
	     :id (car info)
	     :name (get-torrent-info "Name" data)
	     :percent-done (get-torrent-info "Percent Done" data)
	     :ratio (get-torrent-info "Ratio" data)
	     :ratio-limit (get-torrent-info "Ratio Limit" data)
	     :date-finished (get-torrent-info "Date finished" data
					      :missing-ok t)))) 
      (when (string/= "100%" (torrent-percent-done torrent))
	;; skip it since it's not done
	(and *verbose* (print-torrent torrent :incomplete))
	(go :next))
      
;;;; Setup torrent data
    
      (setf (torrent-ratio-limit torrent)
	(if* (string= "Default" (torrent-ratio-limit torrent))
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
	(user::transmission-filename-to-tracker (torrent-name torrent)
						:debug *debug*))
      
      (setq btn (match-re "broadcasthe.net" (torrent-tracker torrent)))
      (setq mma-tracker (match-re "mma-tracker.net" (torrent-tracker torrent)))

;;;; Use torrent data to determine status
      
      ;; Determine if this torrent is "done" and can be removed.
      ;; Usually this is if either 1) seeding is complete, or 2) we have
      ;; seeded the torrent for *seedmin* seconds, but there are exceptions
      ;; and complications.  See below for the exact rules.

      ;; Special seeding rules for some sites
      ;;
      (if* (and btn (= -1.0 (torrent-ratio-limit torrent)))
	 then ;; This torrent was downloaded with tget, so we know it's not
	      ;; a season pack (with different seeding rules), set to seed
	      ;; forever.  The BTN tracker and Transmission often disagree 
	      ;; on the amount I've uploaded, so set ratio-limit to 1.5 to
	      ;; be safe.
	      (setf (torrent-ratio-limit torrent) 1.50)
	      ;; The rules state you need to seed to 1:1 or 24 hours.  Give
	      ;; it some slop, to make sure I don't get a H&R
	      (setf (torrent-seed-min-time torrent) (* 3600 28))
       elseif btn
	 then ;; Torrent downloaded manually, and right now we have no way
	      ;; of telling if it's a season pack or not.  We could do that
	      ;; from the filename, but we'd need to be very careful to get
	      ;; it right.  So, set the min seed time to a week + slop.
	      (setf (torrent-seed-min-time torrent)
		(+ (* 3600 24 7)
		   (* 3600 6)))
       elseif mma-tracker
	 then ;; Hard to seed stuff here, so set the seed time to 4 days
	      ;; and the ratio-limit to 2.0, so I can get some credits
	      ;; built up, in case I can seed.
	      (setf (torrent-ratio-limit torrent) 2.0)
	      (setf (torrent-seed-min-time torrent) (* 3600 24 4)))
      
      ;; First, determine if seeding is complete.
      ;; transmission-remote doesn't give us a "seeding complete"
      ;; indication, so we use "Ratio" >= "Ratio Limit".
      (when (>= (torrent-ratio torrent)
		(torrent-ratio-limit torrent))
	(and *verbose* (print-torrent torrent :complete-ratio))
	(remove-torrent torrent)
	(go :next))

      ;; Seeding is not complete.  See if we've seeded for the minimum
      ;; amount of time.
      
      (if* (> (torrent-seeded-for torrent)
	      (or
	       ;; Prefer the torrent-specific value over the global one
	       (torrent-seed-min-time torrent)
	       *seedmin*))
	 then (print-torrent torrent :complete-time)
	      (remove-torrent torrent)
       elseif *verbose*
	 then (setf (torrent-seed-for torrent)
		(- (or (torrent-seed-min-time torrent) *seedmin*)
		   (torrent-seeded-for torrent)))
	      (print-torrent torrent :seeding)))
   :next
    ))

(defun relative-time-formatter (seconds)
  (if* (> seconds #.(* 3600 24))
     then ;; more than a day, included days
	  (universal-time-to-string
	   (+ *now* seconds)
	   :relative *now*
	   :format (ut-to-string-formatter "%D day%p, %2H:%2M:%2S"))
     else (universal-time-to-string
	   (+ *now* seconds)
	   :relative *now*
	   :format (ut-to-string-formatter "%2H:%2M:%2S"))))

(defun print-torrent (torrent status)
  (and *debug* (format t "~a~%" torrent))
  (format t "~%~a~%" (torrent-name torrent))
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
	     (relative-time-formatter (torrent-seed-for torrent))))))

(defparameter *remove* nil)

(defun remove-torrent (torrent)
  (when *remove*
    (error "remove-torrent: fix me")
    (if* (tm "-t" (torrent-id torrent) "-r")
       thenret
       else (warn "~a: removing exited with a non-zero status..."))
;;;;TODO: need to check for "success" in output of tm
    ))

(defun get-torrent-info (key hash &key missing-ok)
  (or (gethash key hash)
      (if* missing-ok
	 then nil
	 else (error "Couldn't find ~a in torrent data" key))))

(defun user::main ()
  (system:with-command-line-arguments
      ("dqr" debug quiet remove)
      (rest)
    (declare (ignore rest))
    (when debug (setq *debug* t))
    (when quiet (setq *verbose* nil))
    (when remove (setq *remove* t))
    (handler-case (seedstatus)
      (error (c)
	(format t "Error: ~a" c)
	(exit 1)))
    (exit 0)))
