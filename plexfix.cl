;; plexfix :: fix filenames for Plex Media Server

(eval-when (compile eval load)
  (require :tget-defs "defs.fasl")
  (require :tget-utils "utils.fasl"))

(in-package :user)

(defvar *debug* nil)
(defvar *no-execute* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plexfix (filename &key (stream t)
			      ((:no-execute *no-execute*) *no-execute*))
  (if* (not (probe-file filename))
     then (.error "~a does not exist." filename)
   elseif (file-directory-p filename)
     then (let ((files (directory (pathname-as-directory filename)))
		rar)
	    (if* (setq rar
		   (member "rar" files :key #'pathname-type :test #'string=))
	       then (setq rar (car rar))
		    (format stream "extracting ~a...~%" (file-namestring rar))
		    (excl.osi:command-output
		     (format nil "unrar e ~a ./" (file-namestring rar))
		     :directory (directory-namestring rar))
	       else (dolist (file files)
		      ;; handle mp4 and mkv files, for now, warn of the
		      ;; rest, ignoring some known files
		      (if* (member (pathname-type file) '("mp4" "mkv")
				   :test #'string=)
			 then (plexfix-1 stream file)
		       elseif (member (pathname-type file)
				      '("nfo" "txt" "srt")
				      :test #'string=)
			 thenret
			 else (format stream "Unknown file:~a.~%" file)))))
   elseif (ignore-file-p filename)
     thenret
     else (plexfix-1 stream filename)))

(defun ignore-file-p (filename)
  ;; don't fix anything in /tmp/... since they almost never need fixing
  ;; and plexfix would make more of a mess.  This makes all the previous
  ;; ignore rules obsolete.
  (match-re "/tmp/" (namestring filename) :return nil))

(defun plexfix-1 (stream filename &aux new-name type)
  (multiple-value-bind (series-name season episode pms-fail year month day)
      (parse-name-season-and-episode (file-namestring filename)
				     :episode-required t
				     :junk-allowed t)
    (when (not pms-fail) (return-from plexfix-1 nil))
    
    ;; PMS will not see this file, so make a symlink for it that it will
    ;; see.
    (setq type (pathname-type filename))
    (setq new-name
      (if* year
	 then ;; date-based name
	      (format nil "~a.~d.~2,'0d.~2,'0d.~a"
		      series-name year month day type)
       elseif (consp episode)
	 then (format nil "~a.S~2,'0dE~2,'0dE~2,'0d.~a"
		      series-name season (car episode) (cdr episode) type)
	 else (format nil "~a.S~2,'0dE~2,'0d.~a"
		      series-name season episode type)))
    (setq new-name (merge-pathnames new-name filename))
    (with-verbosity 1
      (format stream "OLD name: ~a~%" filename)
      (format stream "NEW name: ~a~%" new-name))

    (if* *no-execute*
       then (format stream "Would symlink ~a ~a~%" filename new-name)
       else (make-symlink filename new-name))
    
    ;; Look for .srt file with the same name, and make a symlink for that,
    ;; too.  The Roku can't use subtitles unless the file has the same
    ;; name!
    (let ((srt (merge-pathnames (make-pathname :type "srt")
				filename))
	  new-srt)
      (when (probe-file srt)
	(with-verbosity 1
	  (format stream "Making link for srt file, too.~%"))
	(setq new-srt (merge-pathnames (make-pathname :type "srt") new-name))
	(if* *no-execute*
	   then (format stream "Would symlink ~a ~a~%" srt new-srt)
	   else (make-symlink srt new-srt))))))

(defun make-symlink (target link-name)
  ;; like symlink, but the link-name is relative, meaning we need to change
  ;; to the directory first
  (chdir (path-pathname link-name))
  (symlink (file-namestring target) (file-namestring link-name)
	   :raw t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition plexfix (error) ())

(defun .error (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'plexfix :format-control format-string
	 :format-arguments format-arguments))

(defun user::main (&aux (stream t))
  (flet
      ((send-output ()
	 (when (not (eq 't stream))
	   (let ((body (get-output-stream-string stream)))
	     (when (string/= "" body)
	       (ignore-errors
		(send-letter
;;;;TODO: parameterize:
		 "192.168.0.1"
		 (sys:getenv "USER")
		 (sys:getenv "USER")
		 body
		 :subject "plexfix"))))))
       (doit (&aux torrent-dir torrent-name)
	 (system:with-command-line-arguments
	     (("d" :short debug)
	      ("m" :short no-email)
	      ("n" :short no-execute)
	      ("q" :short quiet)
	      ("v" :short verbose :allow-multiple-options))
	     (rest)
	   (if* (and (null rest)
		     (setq torrent-dir (sys:getenv "TR_TORRENT_DIR"))
		     (setq torrent-name (sys:getenv "TR_TORRENT_NAME")))
	      then ;; called from Transmission
		   (setq rest
		     (list 
		      (namestring
		       (merge-pathnames torrent-name
					(pathname-as-directory torrent-dir)))))
	    elseif (or (null rest) (/= 1 (length rest)))
	      then (.error "usage: plexfix file."))
	   (when debug (setq *debug* t))
	   (when no-execute (setq *no-execute* t))
	   (setq *verbose* (or verbose
			       (when quiet 0)
			       1))
	   (when (and (not debug) (not no-email))
	     (setq stream (make-string-output-stream)))
	   (plexfix (car rest) :stream stream))))

    (if* *debug* ;; -d on command line doesn't effect this test!
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (top-level.debug:with-auto-zoom-and-exit (*standard-output*)
	      (handler-case (doit)
		(plexfix (c)
		  ;; 'plexfix errors don't get a backtrace, since those are
		  ;; expected or, at least, planned for.  The unexpected
		  ;; ones get the zoom.
		  (format stream "~&~a~&" c)
		  (send-output)
		  (exit 1 :quiet t)))))
    (send-output)
    (exit 0 :quiet t)))
