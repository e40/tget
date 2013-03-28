;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;TODO:
;; PHASE 1: test mode
;; - test date parser to make sure timezone is correct
;; - way to dump out flexget series and learn from them??
;; - handle errors from read-feed (skip to next group)
;; - a test suite, especially for upgrades
;;
;; PHASE 2: production mode
;; - transmission interface via "transmission-remote"??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile eval load)
  (require :ssl)
  (require :ef-e-crcrlf)
  (require :rssreader)
  (require :datetime)
  (require :shell)
  (require :acache "acache-2.1.22.fasl"))

(defpackage :user
  (:use #:excl #:util.date-time #:excl.shell #:db.allegrocache)
  (:import-from #:db.allegrocache.utils
		#:defclass*))

(in-package :user)

(setq *global-gc-behavior* :auto)

(defvar *tget-version* "1.1")
(defvar *schema-version*
    ;; 1 == initial version
    ;; 2 == added `delay' slot
    ;; 3 == added schema versioning
    3)

(defvar *tget-data-directory* "~/.tget.d/")
(defvar *auto-backup*
    ;; There is a command line option to change this:
    t)
(defvar *database-name* nil)
(defvar *version-file* nil)
(defvar *config-file* nil)
(defvar *debug* nil)

(defvar *log-rss-stream* nil)
(defvar *log-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-settable variables (in config file)

(defvar *feed-interval* 14)
(defvar *log-rss*
    ;; If non-nil, a pathanme to log rss feed info
    nil)
(defvar *log-file*
    ;; If non-nil, a pathanme to log episode info
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; acache hackery

;; this should be part of AC!
(defun describe-persistent-clos-object (object stream)
  (let* ((class (class-of object))
	 (slotds (mop:class-slots class))
	 (max-slot-name-length 0)
	 (instance-slotds ()))
    (flet ((adjust-slot-name-length (name)
	     (setq max-slot-name-length
	       (max max-slot-name-length
		    (length (the string (prin1-to-string name))))))
	   (describe-slot (name &optional (allocation () alloc-p))
	     (let* ((boundp (slot-boundp object name))
		    (value (and boundp (slot-value object name))))
	       (if alloc-p
		   (format stream
			   "  ~@<~3I~S ~S~VT~_~:[<unbound>~*~;~S~]~:>~%"
			   name allocation (+ max-slot-name-length 7)
			   boundp value)
		 (format stream
			 "  ~@<~3I~S~VT~_~:[<unbound>~*~;~S~]~:>~%"
			 name max-slot-name-length
			 boundp value)))))
      ;; Figure out a good width for the slot-name column.

      (dolist (slotd slotds)
	(when (eq :persistent (excl::slotd-allocation slotd))
	  (adjust-slot-name-length (mop:slot-definition-name slotd))
	  (push slotd instance-slotds)))

      (setq max-slot-name-length
	(excl::first-column-width (+ max-slot-name-length 3)))
      
      (when instance-slotds
	(format stream " The '~a' object has the following persistent slots~%"
		(class-name class))
	(dolist (slotd (nreverse instance-slotds))
	  (describe-slot (mop:slot-definition-name slotd))))

      (values))))

(defun print-object-persistent-clos-object (obj stream name-func)
  (print-unreadable-object (obj stream :identity *print-escape* :type t)
    (let ((oid (db-object-oid obj)))
      (format stream "~s oid: ~s, ver ~s, trans: ~s, ~a"
;;;;addition:
	      (if* (funcall name-func obj)
		 thenret
		 else "-no name-")
;;;;...end addition.
	      oid
	      (db.ac::db-object-version obj)
	      (db.ac::db-object-trans obj)
	      (if* (and (slot-boundp obj 'db.ac::new-slots)
			(eq :dead (db.ac::db-object-new-slots obj)))
		 then " deleted"
	       elseif (and (slot-boundp obj 'db.ac::modified)
			   (null (db.ac::db-object-modified obj))
			   (if (typep obj 'db.ac::ac-map)
                               (null (db.ac::ac-map-modified obj))
			     t))
		 then " not modified" else " modified")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(defclass* quality (:conc-name t :print nil :init nil)
  ;; The name of the quality.  Users define this to be something like
  ;; `:normal'.
  (name :index :any-unique)
  source
  codec
  resolution)

(defmethod print-object ((obj quality) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (format nil "~a, source=~s, codec=~s, res=~s"
	       (quality-name obj)
	       (when (slot-boundp obj 'source) (quality-source obj))
	       (when (slot-boundp obj 'codec) (quality-codec obj))
	       (when (slot-boundp obj 'resolution) (quality-resolution obj))))))
   (t ;; print it for humans
    (format stream "#<quality ~s [~s,~s,~s]>"
	    (quality-name obj)
	    (quality-source obj)
	    (quality-codec obj)
	    (quality-resolution obj)))))

(defmethod describe-object ((object quality) stream)
  (describe-persistent-clos-object object stream))

(defparameter *valid-sources* '(:hdtv :unknown))
(defparameter *valid-codecs* '(:x264 :xvid))
(defparameter *valid-resolutions '(:<720p :720p :1080p))

;;transmission-remote ${TRANSMISSION_HOST}:${TRANSMISSION_PORT}
;;    --auth=${TRANSMISSION_USER}:${TRANSMISSION_PASS} -l
;; :ratio => -sr
;; :host/:port => see above
;; :add-paused => --start-paused & --no-start-paused
;; :remove-when-done => ????
;;
(defclass* transmission (:conc-name t :print nil :init nil)
  host
  port
  username
  password
  add-paused
  ratio
  remove-when-done)

(defmethod describe-object ((object transmission) stream)
  (describe-persistent-clos-object object stream))

(defclass* group (:conc-name t :print nil :init nil)
  ;;e.g. :kevin
  (name :index :any-unique)
  rss-url
  delay
  transmission-client
  ratio
  quality
  download-path)

(defmethod print-object ((obj group) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj) (if (slot-boundp obj 'name) (group-name obj)))))
   (t ;; print it for humans
    (format stream "#<group ~s>" (group-name obj)))))

(defmethod describe-object ((object group) stream)
  (describe-persistent-clos-object object stream))

(defclass* series (:conc-name t :print nil :init nil)
  ;;e.g. :kevin
  (group :index :any)
  ;;Downcased version of the series name.  e.g. "vikings"
  ;; ***must match episode class naming***
  (name :index :any-unique)
;;;;TODO: need to implement this:
  last-episode 
  ;; a list of episodes that have gaps after last-episode.  That
  ;; is, last-episode is the last contiguous episode, and (at least!) the
  ;; episode after that is missing.  NOTE: if last-episode is from a
  ;; different season, and the next episode after that is episode `1' from
  ;; the next season, then we assume the episodes are contiguous.
;;;;TODO: need to implement this:
  discontinuous-episodes
  delay
;;;; overrides for group:
  quality)

(defmethod print-object ((obj series) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj) (if (slot-boundp obj 'name) (series-name obj)))))
   (t ;; print it for humans
    (format stream "#<series ~a>" (series-name obj)))))

(defmethod describe-object ((object series) stream)
  (describe-persistent-clos-object object stream))

(defclass* episode (:conc-name t :print nil :init nil)
  series
  ;;Downcased version of the series name.  e.g. "vikings"
  ;; ***must match series class naming***
  (series-name :index :any)
  full-title				;e.g. "Vikings - 1x04 - Trial (.mp4)"
  title					;e.g. "Trial (.mp4)"
  torrent-url
  pub-date				;universal time
  ;;number
  (season :index :any)
  ;;number or :all
  (episode :index :any)
  ;; quality from torrent
  (source :index :any)
  (codec :index :any)
  (resolution :index :any)
;;;; optional:
  type					;mime type
  length				;length in bytes
  filename				;e.g. "Vikings.S01E04.HDTV.x264-2HD.mp4"
;;;;
  (transient :index :any))

(defmethod print-object ((obj episode) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (format
	nil
	"~a~@[ S~2,'0d~]~@[E~2,'0d~]~@[; quality:~a~]; transient=~s"
	(when (slot-boundp obj 'series-name)
	  (episode-series-name obj))
	(when (slot-boundp obj 'season)
	  (episode-season obj))
	(when (slot-boundp obj 'episode)
	  (episode-episode obj))
	(when (or (slot-boundp obj 'source)
		  (slot-boundp obj 'codec)
		  (slot-boundp obj 'resolution))
	  (format
	   nil "~@[ src=~s~]~@[ codec=~s~]~@[ res=~s~]"
	   (when (slot-boundp obj 'source) (episode-source obj))
	   (when (slot-boundp obj 'codec) (episode-codec obj))
	   (when (slot-boundp obj 'resolution)
	     (episode-resolution obj))))
	(if* (slot-boundp obj 'transient)
	   then (episode-transient obj)
	   else "-unbound-")))))
   (t ;; print it for humans
    (format stream "#<~s, ~@[S~2,'0d~]~@[E~2,'0d~] [~a,~a,~a]>"
	    (episode-series-name obj)
	    (episode-season obj)
	    (episode-episode obj)
	    (episode-source obj)
	    (episode-codec obj)
	    (episode-resolution obj)))))

(defmethod describe-object ((object episode) stream)
  (describe-persistent-clos-object object stream))

;;;; transient types not in the database:

(defstruct rss-item
  source ;; host part of the url from which the feed came
  title
  link
  guid
  comments
  pub-date
  description
  type
  length)

(defstruct schema
  version
  tget-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Macros

(defmacro defquality (name &key source codec resolution)
  `(make-quality
     :name ,name
     :source ,source
     :codec ,codec
     :resolution ,resolution))

(defun make-quality (&key name source codec resolution)
  (let ((old (retrieve-from-index 'quality 'name name)))
    (or (member source *valid-sources* :test #'eq)
	(error "Bad source: ~s." source))
    (or (member codec *valid-codecs* :test #'eq)
	(error "Bad codec: ~s." codec))
    (or (member resolution *valid-resolutions :test #'eq)
	(error "Bad resolution: ~s." resolution))
    (if* old
       then (setf (quality-source old) source)
	    (setf (quality-codec old) codec)
	    (setf (quality-resolution old) resolution)
	    old
       else (make-instance 'quality
	      :name name
	      :source source
	      :codec codec
	      :resolution resolution))))

(defmacro defgroup (name &key rss-url delay client ratio quality download-path)
  `(make-group
     :name ,name
     :rss-url ,rss-url
     :delay ,delay
     :client ,client
     :ratio ,ratio
     :quality ,quality
     :download-path ,download-path))

(defun make-group (&key name rss-url delay client ratio quality download-path)
  (let ((old (retrieve-from-index 'group 'name name)))
    (check-rss-url rss-url)
    (check-delay delay)
    (check-client client)
    (check-ratio ratio)
    (check-quality quality)
    (setq download-path (check-download-path download-path))
    (if* old
       then (setf (group-rss-url old) rss-url)
	    (setf (group-delay old) delay)
	    (setf (group-transmission-client old) client)
	    (setf (group-ratio old) ratio)
	    (setf (group-quality old) quality)
	    (setf (group-download-path old) download-path)
	    old
       else (make-instance 'group
	      :name name
	      :rss-url rss-url
	      :delay delay
;;;;TODO: change keyword for defgroup???
	      :transmission-client client
	      :ratio ratio
	      :quality quality
	      :download-path download-path))))

(defmacro defseries (name group &key delay quality)
  `(make-series
    :name ,name
    :group ,group
    ,@(when delay `(:delay ,delay))
    ,@(when quality `(:quality ,quality))))

(defun make-series (&key name group delay quality)
  (let ((old (retrieve-from-index 'series 'name name)))
    (check-delay delay)
    (check-quality quality)
;;;;TODO: check group
    (or (keywordp group)
	(error "Bad group: ~s." group))
    (or (stringp name)
	(error "Series name must be a string: ~s." name))
    (if* old
       then (setf (series-group old) group)
	    (setf (series-delay old) delay)
	    (setf (series-quality old) quality)
	    old
       else (make-instance 'series
	      :name (string-downcase name)
	      :group group
	      :delay delay
	      :quality quality))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-rss-url (rss-url)
  (and rss-url
       (or (symbolp rss-url)
	   (and (stringp rss-url)
		(match-re "^http" rss-url))
	   (error "Bad rss-url: ~s." rss-url))))

(defun check-delay (delay)
  (and delay
       (or (numberp delay)
	   (error "Bad delay, must be a number: ~s." delay))))

(defun check-ratio (ratio)
  (and ratio
       (or (and (stringp ratio)
		(match-re "^-?[0-9.]+$" ratio))
	   (error "Bad ratio: ~s." ratio))))

(defun check-quality (quality)
  (and quality
       (or (and (symbolp quality)
		(or (retrieve-from-index 'group 'name quality)
		    (keywordp quality)
		    (eq 't quality)
		    (symbol-function quality)
		    (error "Quality ~s does not exist." quality)))
	   (error "Bad quality: ~s." quality))))

(defun check-client (client)
;;;;TODO: check client
  client
  )

(defun check-download-path (download-path)
  #+not-yet ;; might be on a different machine?!
  (or (probe-file download-path)
      (error "download path does not exist: ~a." download-path))
  (namestring download-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(defvar *usage*
    "~
Usage: tget [--cron]
            [--reset] [--db database]
            [--learn] [--feed-interval ndays]
            [--auto-backup { program-update | schema-update | both } ]

--cron                :: quiet mode
--reset               :: reset database before beginning operation
--db database         :: path to database
--learn               :: don't download anything--useful in conjunction
                         with reset to wipe the database and start over
--feed-interval ndays :: set the feed interval to `ndays'.  Only useful
                         when a user-defined function of one argument is
                         given to defgroup's :rss-url option.
--auto-backup { program-update | schema-update | t }
                      :: automatically do a backup of the database when
                           1) the program is updated,
                           2) the database schema is updated, or
                           3) either of the above is true.
                         The default is \"schema-update\".

Examples:
# Toss current database and catch up on shows released in the last 180 days
# marking them all as `downloaded'
$ tget --reset --learn --feed-interval 180

# Usage from Cron:
$ tget --cron --auto-backup t
")

(defvar *verbose*
    ;; Be noisy.  `nil' is used for cron mode.
    t)

(defvar *learn*
    ;; Don't download anything, just learn
    nil)

(defun main ()
  (flet
      ((doit ()
	 (system:with-command-line-arguments
	     (("help" :long help)
	      ("debug" :long debug-mode)
	      ("config" :long config-file :required-companion)
	      ("cron" :long cron-mode)
	      ("learn" :long learn-mode)
	      ("reset" :long reset-database)
	      ("feed-interval" :long feed-interval :required-companion)
	      ("root" :long root :required-companion)
	      ("db" :long database :required-companion)
	      ("auto-backup" :long auto-backup :required-companion))
	     (extra-args :usage *usage*)
	   (when help
	     (format t "~a~&" *usage*)
	     (exit 0 :quiet t))
	   (when extra-args (error "extra arguments:~{ ~a~}." extra-args))

	   (when root
	     (or (probe-file root)
		 (error "-root directory does not exist: ~a." root))
	     (setq *tget-data-directory* (pathname-as-directory root)))
	   
	   (setq *database-name*
	     ;; This should *not* end in a slash:
	     (merge-pathnames "db" *tget-data-directory*))
	   (setq *version-file*
	     ;; Set in main, if *database-name* changed
	     (merge-pathnames "db/version.cl" *tget-data-directory*))
	   (setq *config-file*
	     ;; First one wins:
	     (list "sys:config.cl"
		   "~/.tget.cl"
		   (merge-pathnames "config.cl" *tget-data-directory*)))
	   
	   (when debug-mode (setq *debug* t))
	   (setq *verbose* (not cron-mode))
	   (setq *learn* learn-mode)
	   
	   (if* config-file
	      then (or (probe-file config-file)
		       (error "--config file does not exist: ~a." config-file))
	    elseif (consp *config-file*)
	      then (when (dolist (config *config-file* t)
			   (when (probe-file config)
			     (setq config-file config)
			     (return nil)))
		     (error "None of these config files exists:~{ ~a~}."
			    *config-file*))
	      else (error "Internal error: bad *config-file* value: ~s."
			  *config-file*))
	   
	   (when database
	     ;; Remove trailing slash, if there is one
	     (when (=~ "(.*)/$" database) (setq database $1))
	     (setq *database-name* (pathname database))
	     (setq *version-file*
	       (pathname (format nil "~a/version.cl" database))))
	   
	   (when feed-interval
	     (when (not (match-re "^\\d+$" feed-interval))
	       (error "Bad --feed-interval: ~s." feed-interval))
	     (setq *feed-interval* (parse-integer feed-interval)))
	   
	   (when auto-backup
	     (setq *auto-backup*
	       (cond ((string= "t" auto-backup) t)
		     ((string= "program-update" auto-backup) :program-update)
		     ((string= "schema-update" auto-backup) :schema-update)
		     (t
		      (error "Bad value for --auto-backup: ~a."
			     auto-backup)))))

	   (open-tget-database :if-exists (if* reset-database
					     then :supersede
					     else :open))
	   (load config-file)
	   (open-log-files)
	   (process-groups)
	   
	   (exit 0 :quiet t))))
    (if* *debug*
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (handler-case (doit)
	      (error (c)
		(format t "An error occurred: ~a~%" c)
		(exit 1 :quiet t))))))

(defun open-log-files (&key truncate)
  (when (and *log-file* (not *log-stream*))
    (and *verbose* (format t ";; Opening ~a log file...~%" *log-file*))
    (setq *log-stream*
      (open *log-file* :direction :output
	    :if-exists (if truncate :supersede :append)
	    :if-does-not-exist :create)))
	   
  (when (and *log-rss* (not *log-rss-stream*))
    (and *verbose* (format t ";; Opening ~a rss log file...~%" *log-rss*))
    (setq *log-rss-stream*
      (open *log-rss* :direction :output
	    :if-exists (if truncate :supersede :append)
	    :if-does-not-exist :create))))

(defun close-log-files ()
  (when *log-stream*
    (close *log-stream*)
    (setq *log-stream* nil))
  (when *log-rss-stream*
    (close *log-rss-stream*)
    (setq *log-rss-stream* nil)))

(push '(close-log-files) sys:*exit-cleanup-forms*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database routines

(defvar *lock-file* nil)

(defun acquire-database-lock-file ()
  (when (null *lock-file*)
    (setq *lock-file* (pathname (format nil "~a.lock" *database-name*))))
  (handler-case
      (with-open-file (s *lock-file* :direction :output
		       :if-exists :error
		       :if-does-not-exist :create)
	(format s "Lock created on ~a~%"
		(ut-to-date-time (get-universal-time))))
    (error (c)
      (declare (ignore c))
      (error "Could not obtain the lock file (~a)." *lock-file*))))

(defun release-database-lock-file ()
  (when (null *lock-file*)
    (error "Tried to release lock file before it was created."))
  (ignore-errors (delete-file *lock-file*)))

(defun open-tget-database (&key (if-does-not-exist :create)
				(if-exists :open)
			   &aux ok)
  (when db.allegrocache::*allegrocache*
    (close-tget-database))

  (ensure-directories-exist *database-name*)
  
  (acquire-database-lock-file)

  (unwind-protect
      ;; Do we need to upgrade this database?
      (let* ((stuff (if* (eq if-exists :supersede)
		       then (list :version *schema-version*
				  :tget-version *tget-version*)
		     elseif (probe-file *version-file*)
		       then (or (ignore-errors
				 (read-from-string
				  (file-contents *version-file*)))
				(error "Schema version file ~a is corrupt."
				       *version-file*))
		       else ;; The version prior to having versioning --
			    ;; rebuilds were necessary from version 1 to 2,
			    ;; but after that it's automatic.
			    '(:version 2 :tget-version "0.00")))
	     (schema (or (ignore-errors (apply #'make-schema stuff))
			 (error "Schema version data is corrupt: ~s." stuff)))
	     (schema-update (< (schema-version schema) *schema-version*))
	     (program-update (string/= (schema-tget-version schema)
				       *tget-version*))
	     backed-up)
    
	(when (probe-file *database-name*)
	  ;; Check for the need to backup
	  (when (and schema-update
		     (or (eq 't *auto-backup*)
			 (eq :schema-update *auto-backup*)))
	    (backup-database "for schema update")
	    (setq backed-up t))
	  (when (and (not backed-up)
		     program-update
		     (or (eq 't *auto-backup*)
			 (eq :program-update *auto-backup*)))
	    (backup-database "for program update")))
    
	(open-file-database *database-name*
			    :use :memory
			    :if-does-not-exist if-does-not-exist
			    :read-only nil
			    :if-exists if-exists)
	(when (not (probe-file *version-file*))
	  ;; New database, write it
	  (setf (file-contents *version-file*)
	    (format nil "~s~%" stuff)))
    
	(when (< (schema-version schema) *schema-version*)
	  ;; the database schema version is older than the program's
	  ;; schema version, upgrade necessary
	  (do* ((v (schema-version schema) (1+ v)))
	      ((= v *schema-version*))
	    ;; Upgrade each step
	    (handler-case (db-upgrade v)
	      (error (c)
		(error "Database upgrade to version ~d failed: ~a." v c)))
	    ;; Update *version-file* to new version
	    (setf (file-contents *version-file*)
	      (format nil "(:version ~d :tget-version ~s)~%"
		      (1+ v)
		      *tget-version*))))
	(setq ok t))
    ;; In the event of an error, make sure we release the lock:
    (when (not ok)
      (if* db.allegrocache::*allegrocache*
	 then (close-tget-database)
	 else ;; error before the database was opened, but the lock file
	      ;; was created, so remove it
	      (release-database-lock-file)))))

(defun backup-directory (dbname)
  ;; Find the first name that "matches" DBNAME by appending ``.N'' to the
  ;; name.  A match is a name that does not exist.
  (do* ((n 1 (1+ n))
	(backup #1=(format nil "~a.~d" dbname n) #1#))
      ((not (probe-file backup))
       backup)))

(defun backup-database (reason)
  ;; Backup the database given by *database-name*.  We haven't opened it
  ;; yet, so we're safe to copy the files.
  (format t ";; Backing up database ~a.~%" reason)
  (let* ((backup-directory
	  ;; Like *database-name*, no trailing slash
	  (backup-directory *database-name*))
	 (from (pathname-as-directory *database-name*))
	 (to (pathname-as-directory backup-directory)))
    (ensure-directories-exist to)
    (dolist (file (directory from))
      (sys:copy-file
       file
       (merge-pathnames (enough-pathname file from) to)
       :preserve-time t))
    (format t ";;  Copy is in ~a.~%" to)))

(defmethod db-upgrade ((version (eql 2)))
  ;; The change from 2 to 3: added the tget-admin class.  Just need to add
  ;; an instance to the database for that.
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version)))

;;;;TODO: add this as an exit hook?
(defun close-tget-database ()
  (when *verbose* (format t ";; closing database...~%"))
  (when db.allegrocache::*allegrocache*
    (close-database)
    (setq db.allegrocache::*allegrocache* nil)
    (release-database-lock-file)))

(push '(close-tget-database) sys:*exit-cleanup-forms*)

(defun query-series-name-to-series (name)
  (retrieve-from-index 'series 'name name))

(defun make-episode (&key transient
			  full-title
			  torrent-url
			  pub-date
			  type
			  length
			  series-name
			  title
			  season
			  episode
			  filename
			  source
			  codec
			  resolution
		     &aux temp)
  ;; Only make a new episode instance if one does not already exist.
  
  (if* (setq temp
	 (query-episode :series-name series-name
			:season season
			:ep-number episode
			:source source
			:codec codec
			:resolution resolution))
     then #+ignore
	  (when (cdr temp)
;;;;TODO: sometimes there's an Indi and a non-Indi... I'd really like to
;;;;      only download the non-Indi... so need some way to tag Indi's
	    (warn "more than one! ~s" temp))
	  (car temp)
     else (make-instance 'episode 
	   :transient transient
	   :full-title full-title
	   :torrent-url torrent-url
	   :pub-date pub-date
	   :type type
	   :length length
	   :series-name series-name
	   :title title
	   :season season
	   :episode episode
	   :filename filename
	   :source source
	   :codec codec
	   :resolution resolution)))

(defun query-episode (&key series-name
			   season
			   ep-number
			   source
			   codec
			   resolution
			   ;;
			   episode
			   quality
			   ;;
			   (transient nil transient-given))
  ;; There are three states for :transient
  ;;  - :transient nil -- consider only non-transient episodes
  ;;  - :transient t -- consider only transient episodes
  ;;  - :transient not given -- cosndier all episodes 
  (cond
   (series-name
    (let ((cursor (create-expression-cursor
		   'episode
		   `(and 
		     ,@(when transient-given
			 `((= transient ,transient)))
		     (= series-name ,series-name)
		     ,@(when season `((= season ,season)))
		     ,@(when ep-number `((= episode ,ep-number)))
		     ,@(when source `((= source ,source)))
		     ,@(when codec `((= codec ,codec)))
		     ,@(when resolution `((= resolution ,resolution))))))
	  (episodes '()))
      (when cursor
	(loop
	  (let ((e (next-index-cursor cursor)))
	    (when (null e)
	      (free-index-cursor cursor)
	      (return))
	    (push e episodes))))
      episodes))
   (quality
    (when (null episode)
      (error "Must have an episode to query for quality."))
    (let* ((ep episode)
	   (q (if* (eq 't quality)
		 then nil
		 else (or (retrieve-from-index 'quality 'name quality)
			  (error "No quality named ~s." quality))))
	   (cursor
	    (create-expression-cursor
	     'episode
	     `(and 
	       ,@(when transient-given
		   `((= transient ,transient)))
	       (= series-name ,(episode-series-name ep))
	       (= season ,(episode-season ep))
	       (= episode ,(episode-episode ep))
	       ,@(when q `((= source ,(quality-source q))))
	       ,@(when q `((= codec ,(quality-codec q))))
	       ,@(when q `((= resolution ,(quality-resolution q)))))))
	   (episodes '()))
      (when cursor
	(loop
	  (let ((e (next-index-cursor cursor)))
	    (when (null e)
	      (free-index-cursor cursor)
	      (return))
	    (push e episodes))))
      episodes))
   (t (error "No keywords for episode selection where given"))))

(defun query-group-to-series (group)
  ;; Return a list of all series instances that are in group GROUP-NAME.
  (retrieve-from-index 'series 'group (group-name group)
		       :all t))

;;;TODO: cache the lookups... mostly will be just a few of them
(defun quality (thing)
  (if* (keywordp thing)
     then (retrieve-from-index 'quality 'name thing)
     else ;; user-defined function or `t', just return it
	  thing))

(defun episode-quality (ep)
  ;; Given an episode, return the name of the quality
  (let ((cursor (create-expression-cursor
		 'quality
		 `(and (= source ,(episode-source ep))
		       (= codec ,(episode-codec ep))
		       (= resolution ,(episode-resolution ep)))))
	(res '()))
    (loop
      (let ((q (next-index-cursor cursor)))
	(when (null q)
	  (free-index-cursor cursor)
	  (return))
	(push q res)))
    (when (cdr res)
      (error "There is more than one quality that matches ~s." ep))
    (when res
      (quality-name (car res)))))

(defun hours-available (episode)
  ;; Return the number of hours EPISODE has been available in the feed in
  ;; which it was found.
  (values
   (floor (- (get-universal-time) (episode-pub-date episode))
	  3600)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the meat

(defun process-groups (&aux ep-printer)
  (commit)
  (doclass (group (find-class 'group))

    ;; Setup the printer for this group.  The idea is to have no output
    ;; unless there are matches.
    (setf ep-printer
      (let ((first t))
	(lambda (ep)
	  (when first
	    (format t "~&;; Processing group ~s~%" (group-name group))
	    (setq first nil))
	  (format t "~&~a~%" ep))))
    
    ;; Convert the rss objects to episodes and process them.

    ;; Get all the episodes from the feed into the database, so we can
    ;; query against it.  When I removed non-interesting instances I got
    ;; errors due to accessing slots of deleted objects.  :(
    ;;
    (mapcar 'rss-to-episode
	    (maybe-log-rss (fetch-feed (group-rss-url group))))
    (commit)
    
    ;; Now, the newly added objects in the database have their transient
    ;; slot set to `t'.  We can query against them, to see if we
    ;; need to download any of them, and and easily find them to remove
    ;; them when we're done.

    ;; Now, we comb through the series for this group and look for matches
    ;; to download
    (dolist (series (query-group-to-series group))
      ;; Find any transient episodes that match the series name
      ;;
      ;; series-defined quality trumps group-defined quality.
      ;;
      ;; series-defined quality can be a keyword naming a defined quality
      ;; type.
      ;;
      ;; group-defined quality can be:
      ;;   t :: any quality is fine
      ;;   a keyword :: defined by the keyword naming the quality
      ;;   a function :: defined by calling the function on the episode,
      ;;                 such functions need to take care to use the
      ;;                 `transient' slot to narrow their searches, if
      ;;                 they do them -- the function should return `t'
      ;;                 if the quality is acceptable, `nil' otherwise.
      ;;
      ;; So, let's say we have 3 episodes available, with qualities ranging
      ;; from undefined to :normal to :high (and the latter two qualities
      ;; were defined by the user in their config file).  We don't want the
      ;; order of processing these 3 episodes to change the outcome of
      ;; which episode is selected.
      ;;
      ;; For any episodes that we select for downloading, we need to turn
      ;; off the `transient' flag.
      ;;      
      (let* ((new-series-episodes
	      ;; The episodes we are considering downloading
	      (query-episode :series-name (series-name series)
			     :transient t))
	     matching-episodes)
	(when (setq matching-episodes
		(matching-episodes group
				   series
				   new-series-episodes
				   (or (series-quality series)
				       (group-quality group))))
	  (download-episodes
	   (if* (null (cdr matching-episodes))
	      then ;; easy, only one
		   matching-episodes
	      else (lowest-quality matching-episodes))
	   ep-printer))))
    (commit)
  
    ;; Remove the remaining transient episodes
    (dolist (ep (retrieve-from-index 'episode 'transient t :all t))
      (delete-instance ep))
    (commit)))

(defun matching-episodes (group series episodes quality)
  ;; Return a list of episodes, from the EPISODES list, that has minimum
  ;; quality given by QUALITY, which can be t (any quality), symbol (naming
  ;; a function) or keyword (naming a quality definition).  SERIES is used
  ;; to set the episode-series slot, if we care about the episode.
  ;;
  ;; NOTE: there are all seasons and ep#'s in EPISODES.  We must take that
  ;; into account when processing them.
  ;;
  ;; EPISODES are transient episodes which we have not yet decided to
  ;; download.
  ;;
  ;; QUALITY is the minimum acceptable quality we allow.  In the case of a
  ;; symbol naming a user-defined function, 
  ;;
  (@log "matching for ~a and ~a"  group series)
  (do* ((quality
	 ;; If it's a keyword, convert to quality instance
	 (quality quality))
	(eps episodes (cdr eps))
	(ep (car eps) (car eps))
	(res '()))
      ((null eps) res)
    (@log "matching: ~a" ep)
    (when (and
	   ;; Ignore whole seasons
	   (not (eq :all (episode-episode ep)))
	   ;; Ignore episode 0's
	   (not (eql 0 (episode-episode ep)))
	   (@log "  not a full season or ep 0")
	   
	   ;; Make sure we don't already have it
	   (not
	    (query-episode :series-name (episode-series-name ep)
			   :season (episode-season ep)
			   :ep-number (episode-episode ep)
			   :transient nil))
	   (@log "  we don't have it already")
	     
	   ;; acceptable quality:
	   (episode-quality>= ep quality)
	   (@log "  acceptable quality: ~a" quality)
	   
	   ;; Check that we don't have a delay for this series or group.
	   (let ((delay (or (series-delay series)
			    (group-delay group))))
	     (@log "  delay is ~a" delay)
	     (@log "  hours available ~a" (hours-available ep))
	     (or (null delay)		;No
		 (= 0 delay)		; ...delay
		 
		 ;; The hours this episode has been available is larger
		 ;; than the delay specified, so we're OK.
		 (>= (hours-available ep) delay))))
      ;; A good time to make sure the `series' slot in the episode
      ;; has a meaningful value.  (This change is not important enough to
      ;; warrant a commit.)
      (@log "  => matching episode")
      (setf (episode-series ep) series)
      (push ep res))))

(defun episode-quality>= (episode quality)
  ;; Return T iff the quality of EPISODE is >= than that given by QUALITY.
  (when (keywordp quality)
    (error "didn't expect a keyword here: ~s" quality))
  (when (symbolp quality)
    (setq quality (quality (funcall quality episode))))
  (@log "episode-quality>=:")
  (@log "  episode=~a" episode)
  (@log "  quality=~a" quality)
  (let ((source (episode-source episode))
	(codec (episode-codec episode))
	(resolution (episode-resolution episode)))
    (and (eq source (quality-source quality))
	 (eq codec (quality-codec quality))
	 (resolution>= resolution (quality-resolution quality)))))

(defun resolution>= (r1 r2
		     &aux (res-alist '((:<720p . 0)
				       ( :720p . 1)
				       (:1080p . 2))))
  (let ((r1-val (cdr (or (assoc r1 res-alist :test #'eq)
			 (error "Couldn't find resolution ~s in alist." r1))))
	(r2-val (cdr (or (assoc r2 res-alist :test #'eq)
			 (error "Couldn't find resolution ~s in alist." r2)))))
    (>= r1-val r2-val)))

(defun lowest-quality (episodes)
  ;; In the list of EPISODES, all from the same series, weed out dups that
  ;; differ only by quality and always choose the lower quality (and the
  ;; smaller file).
  ;;
  ;; Typical input is:
  ;;   vikings S01E01; quality: src=:hdtv codec=:x264 res=:720p
  ;;   vikings S01E01; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E02; quality: src=:hdtv codec=:x264 res=:720p
  ;;   vikings S01E02; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E03; quality: src=:hdtv codec=:x264 res=:720p
  ;;   vikings S01E03; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E04; quality: src=:hdtv codec=:x264 res=:720p
  ;;   vikings S01E04; quality: src=:hdtv codec=:x264 res=:<720p
  ;; The correct output is:
  ;;   vikings S01E01; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E02; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E03; quality: src=:hdtv codec=:x264 res=:<720p
  ;;   vikings S01E04; quality: src=:hdtv codec=:x264 res=:<720p
  ;;
  ;; strategy:
  ;;  - sort based on series-name, season, ep#, and res
  ;;  - iterate through skipping the dups
  (do* ((last nil)
	(eps (sort
	      (copy-list episodes)
	      (lambda (e1 e2)
		(if* (= (episode-season e1)
			(episode-season e2))
		   then (if* (= (episode-episode e1)
				(episode-episode e2))
			   then (not (resolution>=
				      (episode-resolution e1)
				      (episode-resolution e2)))
			   else (< (episode-episode e1)
				   (episode-episode e2)))
		   else (< (episode-season e1)
			   (episode-season e2)))))
	     (cdr eps))
	(this (car eps) (car eps))
	(res '()))
      ((null eps) res)
    (when (or (null last)
	      (string/= (episode-series-name last)
			(episode-series-name this))
	      (not (and (= (episode-season last)
			   (episode-season this))
			(= (episode-episode last)
			   (episode-episode this)))))
      (push this res))
    (setq last this)))

(defun download-episodes (episodes print-func)
  (dolist (episode episodes)
    (@log "download: ~a" episode)
    (funcall print-func episode)
    (setf (episode-transient episode) nil)
    (when (not *learn*)
;;;;TODO: actually download episodes -- use *learn*, too
      ))
  (commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed processing

(defvar *cached-feeds*
    ;; An alist of (feed-url . rss-objects)
    nil)

(defun fetch-feed (thing &aux temp url)
  ;; Retrieve the rss feed from URL and return a list of net.rss:item's
  ;;
  ;; Cache the feed results.  We don't want to hit the feed 5-10 times in a
  ;; few seconds, since it's unlikely to change in that time.
  ;;
  
  (if* (stringp thing)
     then (setq url thing)
   elseif (symbolp thing)
     then (setf url (funcall thing *feed-interval*))
     else (error "Bad url: ~s." thing))
  
  (if* (and *cached-feeds*
	    (setq temp (assoc url *cached-feeds* :test #'string=)))
     then (@log "using cached feed for ~a" url)
	  (cdr temp)
     else (let ((res
		 (if* *debug*
		    then ;; while debugging, use a static version of the
			 ;; feed, so we don't hammer the server.
			 (@log "using static feed for ~a" url)
			 (feed-to-rss-objects :file "tvt.xml")
		    else (@log "read feed for ~a" url)
			 (feed-to-rss-objects :url url))))
	    (push (cons url res) *cached-feeds*)
	    res)))

(defun feed-to-rss-objects (&key url content file)
  #+ignore
  (net.rss:rss
   ...
   (net.rss:link "http://www.tvtorrents.com")
   ...
   (net.rss:channel
    ...
    (net.rss:all-items
     (net.rss:item ...)
     (net.rss:item ...)
     ...
     (net.rss:item ...))))
  (let* ((lxml (if* url
		  then (net.rss:read-feed url)
		elseif file
		  then (net.rss::parse-feed (file-contents file))
		  else (assert content)
		       (net.rss::parse-feed content)))
	 (channel (cdr (find 'net.rss:channel (cdr lxml) :key #'car)))
	 (source (cadr (find 'net.rss:link channel :key #'car)))
	 (items (cdr (find 'net.rss:all-items channel :key #'car))))
    (multiple-value-bind (match whole source-name)
	(match-re "(tvtorrents\\.com|broadcasthe\\.net|ezrss\\.it)" source)
      (declare (ignore whole))
      (when (not match) (error "don't grok the feed source: ~s." source))
      (setq source (intern source-name (find-package :keyword))))
    (feed-to-rss-objects-1 source items)))

(defun feed-to-rss-objects-1 (source items)
  (let ((res '()))
    (dolist (item items)
      (do* ((elements (cdr item) (cdr elements))
	    (element (car elements) (car elements))
	    (constructor '())
	    sym)
	  ((null elements)
	   (setq constructor (nreverse constructor))
	   (push (apply #'make-rss-item :source source constructor) res))
	;;(format t "element=~s~%" element)
	(cond
	 ;; elements of the form of (sym value)
	 ((and (consp element)
	       (symbolp (setq sym (car element)))
	       (member sym '(net.rss::comments
			     net.rss:description
			     net.rss:link
			     net.rss:pubDate
			     net.rss:title)
		       :test #'eq))
	  (when (eq 'net.rss:pubDate sym) (setq sym 'pub-date))
	  (push (intern (symbol-name sym)
			(load-time-value (find-package :keyword)))
		constructor)
	  (push (second element) constructor))
	 ;; elements of the form of ((sym value1 value2...))
	 ;; we only care about one of them, net.rss::enclosure
	 ((and (consp element)
	       (consp (car element))
	       (symbolp (setq sym (caar element)))
	       (eq 'net.rss::enclosure sym))
	  (destructuring-bind (&key ((type type))
				    ((length length))
				    ((net.rss::url url)))
	      (cdar element)
	    (declare (ignore url))
	    (push :type constructor)
	    (push type constructor)
	    (push :length constructor)
	    (push length constructor))))))
    res))

(defun maybe-log-rss (items)
  (when *log-rss-stream*
    (format *log-rss-stream*
	    "~%;; ~a~%" (ut-to-date-time (get-universal-time)))
    (dolist (rss items)
      (pprint rss *log-rss-stream*))
    (terpri *log-rss-stream*))
  items)

(defun rss-to-episode (rss)
  ;; Turn the RSS defstruct instance into a persisten episode object.
  (convert-rss-to-episode (rss-item-source rss) rss))

(defun extract-episode-info-from-filename (filename)
  ;; Extract season, episode, source, codec and resolution from the
  ;; filename and return multiple values.  Return `nil' for any that we
  ;; cannot determine.  In the case of `source', return `:unknown' for XviD
  ;; when we cannot determine the source.
  (let (series-name season episode source codec resolution uncut
	match whole year month day)
    (declare (ignore-if-unused whole))
    
    (setq source
      (or (when (or (match-re "(hdtv|720p)" filename :case-fold t)
		    (match-re "x264" filename :case-fold t))
	    :hdtv)
	  (when (match-re "xvid" filename :case-fold t)
	    :unknown)))
    
    (setq codec
      (or (when (match-re "(divx|xvid)" filename :case-fold t)
	    ;; always use :xvid... :divx is little used and no reason to
	    ;; make config files deal with it.
	    :xvid)
	  (when (or (match-re "720p.*mkv" filename :case-fold t)
		    (match-re "\\.mp4" filename :case-fold t)
		    (match-re "x264" filename :case-fold t)
		    (match-re "hdtv.*indi" filename :case-fold t))
	    :x264)))
      
    (setq resolution
      ;; Crude, but the best we can do, it seems
      (if* (match-re "\\.720p\\." filename :case-fold t)
	 then :720p
       elseif (match-re "\\.1080p\\." filename :case-fold t)
	 then :1080p
	 else :<720p))

    (multiple-value-setq (match whole series-name season episode uncut)
      (match-re "^(.*)S([0-9]{2,3})E([0-9]{2,3}).*(\\.UNCUT\\.)" filename))
    (when match
      (setq season (parse-integer season))
      (setq episode (parse-integer episode))
      
      ;; cleanup series-name
      (multiple-value-bind (match whole sname)
	  (match-re "(.*)\\.$" series-name)
	(declare (ignore whole))
	(when match (setq series-name sname)))
      (setq series-name (replace-re series-name "\\." " "))
      (multiple-value-bind (match whole sname tail)
	  (match-re "(.*)(US|\\d\\d\\d\\d)$" series-name)
	(declare (ignore whole))
	;; Some series have an `uncut' version, which for the purposes of
	;; downloading is a completely different show.  If we don't note it
	;; in the series name, then it's just another ep to be downloaded,
	;; and we don't want that.
	(if* match
	   then (setq series-name
		  (format nil "~a~@[~*(uncut) ~](~a)" sname uncut tail))
	 elseif uncut
	   then (setq series-name
		  (concatenate 'simple-string series-name " (uncut)")))))
    
    (when (not season)
      ;; Now try The Daily Show style episodes:
      ;;   YYYYxMM.DD or YYYY.MM.DD
      (multiple-value-setq (match whole year month day)
	(match-re "(\\d\\d\\d\\d)[.x](\\d\\d)\\.(\\d\\d|all)" filename
		  :case-fold t))
      (when match
	(setq season (parse-integer year))
	
	(setq episode
	  (if* (equalp "all" day)
	     then :all
	     else ;; use the ordinal day of the year
		  (date-time-yd-day
		   (date-time (format nil "~a-~a-~a" year month day)))))))

    (values series-name season episode source codec resolution)))

(defmethod convert-rss-to-episode ((type (eql :tvtorrents.com)) rss)
  (let ((des (rss-item-description rss))
	series-name season episode source codec resolution uncut)
    (multiple-value-bind (match whole des-series-name title des-season
			  des-episode filename)
	(match-re
	 #.(concatenate 'simple-string
	     "Show Name:\\s*([^;]+)\\s*;\\s*"
	     "Show Title:\\s*([^;]+)\\s*;\\s*"
	     "Season:\\s*([0-9]+)\\s*;\\s*"
	     "Episode:\\s*("
	     "[0-9]+|"
	     "all|"
	     "\\d\\d\\.\\d\\d|"
	     "\\d\\d\\.all"
	     ")\\s*;\\s*"
	     "Filename:\\s*([^;]+);")
	 des
	 :case-fold t)
      (declare (ignore whole))
      
      ;; ".UNCUT." is in the "Filename" but not the "Show Name", but
      ;; "(Uncut)" is in the "Show Title"!  We need to put it into the
      ;; series-name...
      (setq uncut (match-re "\\(Uncut\\)" title))
      
      (when (not match) (error "couldn't parse rss description: ~s." des))
      
;;;;TODO: early on, we need to lookup the series-name and see if we know
;;;;      about it.  If not, then we should bail and return `nil'.  No need
;;;;      to process something we don't care about.
  
      (setq des-season (parse-integer des-season))
      (setq des-episode
	(if* (match-re "all" des-episode :case-fold t)
	   then ;; The above is wrong, since it matches "all" and "01.all",
		;; but it doesn't matter since I don't download seasons
		:all
	 elseif (= 5 (length des-episode))
	   then (or (=~ "(\\d\\d)\\.(\\d\\d)" des-episode)
		    (error "can't parse episode: ~s" des-episode))
		(date-time-yd-day
		 (date-time (format nil "~a-~a-~a" des-season $1 $2)))
	   else (parse-integer des-episode)))

      (multiple-value-setq (series-name season episode source codec resolution)
	(extract-episode-info-from-filename filename))
      
      ;; Canonicalize the series name, so "Tosh.0" becomes "Tosh 0".
      (setq des-series-name (replace-re des-series-name "\\." " "))
      (when uncut
	(setq des-series-name
	  (concatenate 'simple-string des-series-name " (uncut)")))

      (when (and des-season season
		 (/= des-season season))
	(error "Description and filename season do not agree: ~s, ~s."
	       des-season season))
      (when (or (and (numberp des-episode)
		     des-episode episode
		     (/= des-episode episode))
		(and (symbolp des-episode)
		     des-episode episode
		     (not (eq des-episode episode))))
	(error "Description and filename episode do not agree: ~s, ~s."
	       des-episode episode))
      (when (and des-series-name series-name
		 (not (equalp des-series-name series-name)))
	(warn "Description and filename series name do not agree (using ~
the second one): ~s, ~s."
	      des-series-name series-name))
      
      (make-episode
	:transient t
	:full-title (rss-item-title rss)
	:torrent-url (rss-item-link rss)
	:pub-date (parse-rss20-date (rss-item-pub-date rss))
	:type (rss-item-type rss)
	:length (parse-integer (rss-item-length rss))
     
	:series-name (string-downcase
		      (or
		       ;; filename version is more reliable
		       series-name
		       des-series-name))
	:title title
	:season (or des-season season)
	:episode (or des-episode episode)
	:filename filename
	:source (or source
		    (match-re "\\.mp4" (rss-item-title rss) :case-fold t)
		    (error "can't find source: ~s" rss))
	:codec (or codec
		   (match-re "\\.mp4" (rss-item-title rss) :case-fold t)
		   (error "can't parse codec: ~s" rss))
	;; for TVT it's always in the filename, but others it's in the title
	:resolution resolution))))

(defmethod convert-rss-to-episode ((type (eql :broadcasthe.net)) rss)
  (let ((des (rss-item-description rss))
	series-name)
    
    (when (not des)
      ;; some sporting events have no description and only a title.  Ignore
      ;; these.
      (return-from convert-rss-to-episode nil))
    
    ;; Show name must be extracted from the rss-item-title
    (multiple-value-bind (found whole show-name)
	(match-re "^(.*) - " (rss-item-title rss))
      (declare (ignore whole))
      (when (not found)
	(error "Couldn't find show name from title: ~s." (rss-item-title rss)))
      ;; Canonicalize the series name, so "Tosh.0" becomes "Tosh 0".
      (setq series-name (replace-re show-name "\\." " ")))
    
    ;; The rest of the things we're looking for are in the
    ;; rss-item-description, but weirdly, delimited by <br >/\n
    (multiple-value-bind (found whole title season episode)
	(match-re
	 "Episode Name:\\s*([^<]+)?<br />
Season:\\s*(\\d+)?<br />
Episode:\\s*(\\d+)?"
	 des
	 :case-fold t
	 :multiple-lines t)
      (declare (ignore found whole))

      (make-episode
	:transient t
	:full-title (rss-item-title rss)
	:torrent-url (rss-item-link rss)
	:pub-date (and (rss-item-pub-date rss)
		       (parse-rss20-date (rss-item-pub-date rss)))
	;;both `nil' for BTN!
	:type (rss-item-type rss)
	:length (and (rss-item-length rss)
		     (parse-integer (rss-item-length rss)))
     
	:series-name (string-downcase series-name)
	:title title
	:season (when (and season (string/= "" season)) (parse-integer season))
	:episode (when (and episode (string/= "" episode))
		   (parse-integer episode))
	;; no :filename in BTN feed!
	))))

;;;; The eztv.it RSS feeds are really, really old.  While I can parse what
;;;; they have there, they seem to be defunct.  Leaving this code here as
;;;; an example, for possible future resurrection.
(defmethod convert-rss-to-episode ((type (eql :ezrss.it)) rss)
  (let ((des (rss-item-description rss)))
    (multiple-value-bind (match whole series-name title season episode)
	(match-re
	 "Show Name:\\s*([^;]+)\\s*;\\s*Episode Title:\\s*([^;]+)\\s*;\\s*Season:\\s*([0-9]+)\\s*;\\s*Episode:\\s*([0-9]+)"
	 des
	 :case-fold t)
      (declare (ignore whole))

      (when (not match) (error "couldn't parse rss description: ~s." des))

      ;; Canonicalize the series name, so "Tosh.0" becomes "Tosh 0".
      (setq series-name (replace-re series-name "\\." " "))
      
      (make-episode
	:transient t
	:full-title (rss-item-title rss)
	:torrent-url (rss-item-link rss)
	:pub-date (parse-rss20-date (rss-item-pub-date rss))
	:type (rss-item-type rss)
	:length (parse-integer (rss-item-length rss))
     
	:series-name (string-downcase series-name)
	:title title
	:season (parse-integer season)
	:episode (parse-integer episode)
	;; no filename in this feed
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dates -- this will go away when I get string-to-universal-time done and
;; patched into the lisp

;; ripped off from aserve/main.cl:
(defun parse-rss20-date (date)
  (declare (optimize (speed 3)))
  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    ;; check preferred type first (rfc1123 (formerly rfc822)):
    ;;  	Sun, 06 Nov 1994 08:49:37 GMT
    ;; Some RSS 2.0 feeds use this broken format:
    ;;  	Sun, 06 Nov 1994 08:49:37 +0000
    (multiple-value-bind (matched whole day month year hour minute second tz)
	(match-re 
	 "[A-Za-z]+, ([0-9]+) ([A-Za-z]+) ([0-9]+) ([0-9]+):([0-9]+):([0-9]+) (GMT|[-+]\\d\\d\\d\\d)"
	 date
	 :return :index)
      (declare (ignore whole))
      (when matched
	(return-from parse-rss20-date
	  (encode-universal-time
	   (cvt date second)
	   (cvt date minute)
	   (cvt date hour)
	   (cvt date day)
	   (compute-month date (car month))
	   (cvt date year)
	   (if* (char= #\G (schar date (car tz)))
	      then 0
	      else (compute-tz date (car tz)))))))
    
    (error "couldn't parse date: ~s." date)))

(defun compute-tz (str start)
  ;; 4 digis, first two the hour, second two the minute.  Assume last 2 are
  ;; 0.
  (parse-integer str :start start :end (+ start 2)))

(defun compute-month (str start)
  ;; return the month number given a 3char rep of the string
  
  (case (schar str start)
    (#\A  
     (if* (eq (schar str (1+ start)) #\p)
	then 4 ; april
	else 8 ; august
	     ))
    (#\D 12) ; dec
    (#\F 2 ) ; feb
    (#\J
     (if* (eq (schar str (1+ start)) #\a)
	then 1 ; jan
      elseif (eq (schar str (+ 2 start)) #\l)
	then 7 ; july
	else 6 ; june
	     ))
    (#\M
     (if* (eq (schar str (+ 2 start)) #\r)
	then 3 ; march
	else 5 ; may
	     ))
    (#\N 11) ; nov
    (#\O 10)  ;oct
    (#\S 9) ; sept
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(defvar *log-prefix* nil)

(defun @log (format-string &rest args)
  (when (and *verbose* *log-stream*)
    (when (null *log-prefix*)
      (setq *log-prefix* (format nil "~a: " (excl.osi:getpid))))
    (princ *log-prefix* *log-stream*)
    (apply #'format *log-stream* format-string args)
    (fresh-line *log-stream*))
  ;; Return t so this function can be used in logic chains.
  t)
