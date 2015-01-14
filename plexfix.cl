;; plexfix :: fix filenames for Plex Media Server

(eval-when (compile eval load)
  ;;(require :anydate)
  (require :osi)
  (require :shell)
  (require :autozoom)
  (require :tget-utils "utils.fasl"))

(defpackage :cl-user
  (:use #:common-lisp
	#:excl
	#:excl.osi
	#:excl.shell))

(in-package :cl-user)

(defvar *debug* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plexfix (filename &aux new-name type)
  (multiple-value-bind (series-name season episode pms-fail year month day)
      (parse-name-season-and-episode filename :episode-required t
				     :junk-allowed t)
    (when (not pms-fail) (return-from plexfix nil))
    
    ;; PMS will not see this file, so make a symlink for it that it will
    ;; see.
    (setq type (pathname-type filename))
    (setq new-name
      (if* year
	 then ;; date-based name
	      (format nil "~a.~d.~2,'0d.~2,'0d.~a"
		      series-name year month day type)
	 else (format nil "~a.S~2,'0dE~2,'0d.~a"
		      series-name season episode type)))
    (with-verbosity 2
      (format t "OLD name: ~a~%" filename)
      (format t "NEW name: ~a~%" new-name))
    (if* (probe-file filename)
       then (symlink filename new-name)
       else (.error "~a does not exist." filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition plexfix (error) ())

(defun .error (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'plexfix :format-control format-string
	 :format-arguments format-arguments))

(defun user::main ()
  (flet
      ((doit (&aux torrent-dir torrent-name)
	 (system:with-command-line-arguments
	     (("d" :short debug)
	      ("q" :short quiet)
	      ("v" :short verbose :allow-multiple-options))
	     (rest)
	   (if* (and (null rest)
		     (setq torrent-dir (sys:getenv "TR_TORRENT_DIR"))
		     (setq torrent-name (sys:getenv "TR_TORRENT_NAME")))
	      then ;; called from Transmission
		   (setq rest
		     (list (format nil "~a~a" torrent-dir torrent-name)))
	    elseif (or (null rest) (/= 1 (length rest)))
	      then (.error "usage: plexfix file."))
	   (when debug (setq *debug* t))
	   (setq *verbose* (or verbose
			       (when quiet 0)
			       1))
	   (plexfix (car rest))
	   (exit 0))))
    
    (if* *debug* ;; -d on command line doesn't effect this test!
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (top-level.debug:with-auto-zoom-and-exit (*standard-output*)
	      (handler-case (doit)
		(plexfix (c)
		  ;; 'plexfix errors don't get a backtrace, since those are
		  ;; expected or, at least, planned for.  The unexpected
		  ;; ones get the zoom.
		  (format t "~&~a~&" c)
		  (exit 1 :quiet t)))))))
