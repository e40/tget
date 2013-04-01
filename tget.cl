;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;TODO:
;; - test date parser to make sure timezone is correct
;; - way to dump out flexget series and learn from them??
;; - handle errors from read-feed (skip to next group)
;; - a test suite, especially for upgrades
;; - episode-series slot is never used, but the episode-series-name slot is
;;   used.... we should remove the episode-series-name slot and have
;;   everyone use the episode-series slot??  Seems wasteful.
;; - For shows like The Daily Show that have their episodes canonicalized,
;;   I really should save the original episode name when printing, so the
;;   user doesn't get confused.  That is S2013E86 isn't as recognizable as
;;   2013.03.28
;; - have a mode where the episodes *not* downloaded (but from the same
;;   series) are printed
;; - cache the episode to quality lookup -- this happens a lot and could
;;   speed things up
;; - put a time limit on repacks????
;; - series dups not detected at config load time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile eval load)
  (require :ssl)
  (require :ef-e-crcrlf)
  (require :rssreader)
  (require :datetime)
  (require :shell)
  (require :acache "acache-2.1.22.fasl")
  (require :autozoom))

(defpackage :user
  (:use #:excl #:util.date-time #:excl.shell #:db.allegrocache)
  (:import-from #:db.allegrocache.utils
		#:defclass*))

(in-package :user)

(setq *global-gc-behavior* :auto)

(defvar *tget-version* "1.12")
(defvar *schema-version*
    ;; 1 == initial version
    ;; 2 == added `delay' slot
    ;; 3 == added schema versioning
    ;; 4 == added `container' slots to episode and quality
    ;;      added `priority' to quality
    ;;      added `repack' to episode
    ;; **** changed the name of an unused slot here, didn't need to change
    ;; **** schema based on my testing
    4)

(defvar *tget-data-directory* "~/.tget.d/")
(defvar *auto-backup* t)
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
  priority
  container
  source
  codec
  resolution)

(defmethod print-object ((obj quality) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (format nil "~a, priority=~s, container=~s, source=~s, codec=~s, res=~s"
	       (quality-name obj)
	       (when (slot-boundp obj 'priority) (quality-priority obj))
	       (when (slot-boundp obj 'container) (quality-container obj))
	       (when (slot-boundp obj 'source) (quality-source obj))
	       (when (slot-boundp obj 'codec) (quality-codec obj))
	       (when (slot-boundp obj 'resolution) (quality-resolution obj))))))
   (t ;; print it for humans
    (format stream "#<quality ~s/~d [~@[~s,~]~s,~s,~s]>"
	    (quality-name obj)
	    (quality-priority obj)
	    (quality-container obj)
	    (quality-source obj)
	    (quality-codec obj)
	    (quality-resolution obj)))))

(defmethod describe-object ((object quality) stream)
  (describe-persistent-clos-object object stream))

(eval-when (compile eval load)
(defparameter *valid-containers* '(:avi :mkv :vob :mpeg :mp4 :iso :wmv :ts
				   :m4v :m2ts))
(defparameter *valid-sources* '(:pdtv :hdtv :dsr :dvdrip :tvrip :vhsrip
				:bluray :bdrip :brrip :dvd5 :dvd9 :hddvd
				:web-dl))
(defparameter *valid-codecs* '(:x264 :h.264 :xvid :mpeg2 :divx :dvdr :vc-1
			       :wmv :bd))
(defparameter *valid-resolutions* '(:sd :720p :1080p :1080i))
)

(defclass* group (:conc-name t :print nil :init nil)
  ;;e.g. :kevin
  (name :index :any-unique)
  rss-url
  delay
  debug-feed ;; formerly the unused `transmission-client'
  ratio
  quality
  download-path
  )

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
  ;; Only used for presentation to the user
  pretty-name
  ;;canonicalized version of the series name
  ;; ***must match episode class naming***
  (name :index :any-unique)
;;;;TODO: need to implement this:
  last-episode 
  ;; a list of episodes that have gaps after last-episode.  That
  ;; is, last-episode is the last contiguous episode, and (at least!) the
  ;; episode after that is missing.  NOTE: if last-episode is from a
  ;; different season, and the next episode after that is episode `1' from
  ;; the next season, then we assume the episodes are contiguous.
  ;; Also, we need this so we don't download old episodes that may become
  ;; available.
;;;;TODO: I don't think this is needed....
  discontinuous-episodes
  delay
;;;; overrides for group:
  quality)

(defmethod print-object ((obj series) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (if (slot-boundp obj 'pretty-name) (series-pretty-name obj)))))
   (t ;; print it for humans
    (format stream "#<series ~a>" (series-pretty-name obj)))))

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
  repack					;repack or proper?
  ;; quality from torrent
  (container :index :any)
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
	"~a~@[, REPACK~*~]~@[ S~2,'0d~]~@[E~2,'0d~]~@[; quality=~a~]; transient=~s"
	(when (slot-boundp obj 'series-name) (episode-series-name obj))
	(when (slot-boundp obj 'repack) (episode-repack obj))
	(when (slot-boundp obj 'season) (episode-season obj))
	(when (slot-boundp obj 'episode) (episode-episode obj))
	(pretty-episode-quality obj)
	(if* (slot-boundp obj 'transient)
	   then (episode-transient obj)
	   else "-unbound-")))))
   (t ;; print it for humans
    (format stream "#<~s~@[, REPACK~*~], ~@[S~2,'0d~]~@[E~2,'0d~] [~a]>"
	    (episode-series-name obj)
	    (episode-repack obj)
	    (episode-season obj)
	    (episode-episode obj)
	    (pretty-episode-quality obj)))))

(defun pretty-episode-quality (ep &aux name all-bound)
  ;; Ignore priority of quality.
  (if* (and (slot-boundp ep 'container)
	    (slot-boundp ep 'source)
	    (slot-boundp ep 'codec)
	    (slot-boundp ep 'resolution)
	    (setq all-bound t)
	    (setq name (episode-quality ep)))
     then (format nil "~s" name)
   elseif (and all-bound
	       (null (episode-container ep))
	       (null (episode-source ep))
	       (null (episode-codec ep))
	       (null (episode-resolution ep)))
     then "unknown"
   elseif (or (slot-boundp ep 'container)
	      (slot-boundp ep 'source)
	      (slot-boundp ep 'codec)
	      (slot-boundp ep 'resolution))
     then (format
	   nil "~@[~a,~]~a,~a,~a"
	   (when (slot-boundp ep 'container) (episode-container ep))
	   (when (slot-boundp ep 'source) (episode-source ep))
	   (when (slot-boundp ep 'codec) (episode-codec ep))
	   (when (slot-boundp ep 'resolution) (episode-resolution ep)))
     else "undefined"))

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

(defstruct (transmission
	    (:constructor .make-transmission 
			  (&key host port username password add-paused
				trash-torrent-file ratio)))
  host
  port
  username
  password
  add-paused
  trash-torrent-file
  ratio)

(defvar *transmission-remote* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Macros

(defmacro defquality (name &key priority container source codec resolution)
  `(make-quality
     :name ,name
     :priority ,priority
     :container ,container
     :source ,source
     :codec ,codec
     :resolution ,resolution))

(defun check-atom-or-list (atom-or-list allowed-values what)
  ;; `nil' is allowed and ignored
  (when atom-or-list
    (when (not (consp atom-or-list))
      (setq atom-or-list (list atom-or-list)))
    (dolist (atom atom-or-list)
      (or (member atom allowed-values :test #'eq)
	  (error "Bad ~a value: ~s." what atom)))
    atom-or-list))

(defun make-quality (&key name priority container source codec resolution)
  (let ((old (retrieve-from-index 'quality 'name name)))

    (or (and (null priority)
	     (setq priority 1))
	(numberp priority)
	(error "Priority must be a number: ~s." priority))
    (setq container
      (check-atom-or-list container *valid-containers* 'container))
    (setq source (check-atom-or-list source *valid-sources* 'source))
    (setq codec (check-atom-or-list codec *valid-codecs* 'codec))
    (setq resolution
      (check-atom-or-list resolution *valid-resolutions* 'resolution))

    (if* old
       then (setf (quality-priority old) priority)
	    (setf (quality-container old) container)
	    (setf (quality-source old) source)
	    (setf (quality-codec old) codec)
	    (setf (quality-resolution old) resolution)
	    old
       else (make-instance 'quality
	      :name name
	      :priority priority
	      :container container
	      :source source
	      :codec codec
	      :resolution resolution))))

(defmacro defgroup (name &key rss-url debug-feed delay client ratio quality
			      download-path)
  `(make-group
     :name ,name
     :rss-url ,rss-url
     :debug-feed ,debug-feed
     :delay ,delay
     :client ,client
     :ratio ,ratio
     :quality ,quality
     :download-path ,download-path))

(defun make-group (&key name rss-url debug-feed delay client ratio quality
			download-path)
  (let ((old (retrieve-from-index 'group 'name name)))
    (check-rss-url rss-url)
    ;; don't check debug-feed
    (check-delay delay)
    (check-client client)
    (check-ratio ratio)
    (check-quality quality)
    (setq download-path (check-download-path download-path))
    (if* old
       then (setf (group-rss-url old) rss-url)
	    (setf (group-debug-feed old) debug-feed)
	    (setf (group-delay old) delay)
	    (setf (group-ratio old) ratio)
	    (setf (group-quality old) quality)
	    (setf (group-download-path old) download-path)
	    old
       else (make-instance 'group
	      :name name
	      :rss-url rss-url
	      :debug-feed debug-feed
	      :delay delay
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
  (let* ((pretty-name name)
	 (name (canonicalize-series-name name))
	 (old (retrieve-from-index 'series 'name name)))
    (check-delay delay)
    (check-quality quality)
;;;;TODO: check group
    (or (keywordp group)
	(error "Bad group: ~s." group))
    (or (stringp name)
	(error "Series name must be a string: ~s." name))
    (if* old
       then (when (string/= (series-name old) pretty-name)
	      (setf (series-pretty-name old) pretty-name))
	    (setf (series-group old) group)
	    (setf (series-delay old) delay)
	    (setf (series-quality old) quality)
	    old
       else (make-instance 'series
	      :pretty-name pretty-name
	      :name name
	      :group group
	      :delay delay
	      :quality quality))))

(defmacro deftransmission (options &key host port username password
					add-paused trash-torrent-file
					ratio)
  (declare (ignore options)) ;; just for indentation
  (let ((g-host (gensym "host"))
	(g-port (gensym "port"))
	(g-username (gensym "username"))
	(g-password (gensym "passwd"))
	(g-add-paused (gensym "addpaused"))
	(g-trash-torrent-file (gensym "trash"))
	(g-ratio (gensym "ratio")))
    `(let* ((,g-host ,host)
	    (,g-port ,port)
	    (,g-username ,username)
	    (,g-password ,password)
	    (,g-add-paused ,add-paused)
	    (,g-trash-torrent-file ,trash-torrent-file)
	    (,g-ratio ,ratio))
       (make-transmission
	:host ,g-host
	:port ,g-port
	:username ,g-username
	:password ,g-password
	:add-paused ,g-add-paused
	:trash-torrent-file ,g-trash-torrent-file
	:ratio ,g-ratio))))

(defun make-transmission (&key host port username password add-paused
			       trash-torrent-file ratio)
  (when *transmission-remote*
    (error "Multiple deftransmission definitions in config file."))
  
  (or (stringp host)
      (error "transmission host is not a string: ~s." host))
  (or (numberp port)
      (and (stringp port)
	   (numberp (setq port
		      (ignore-errors
		       (parse-integer port :junk-allowed nil)))))
      (error "transmission port is not a number or string: ~s." port))
  (or (stringp username)
      (error "transmission username is not a string: ~s." username))
  (or (stringp password)
      (error "transmission password is not a string: ~s." password))
  (check-ratio ratio)
  
  (setq *transmission-remote*
    (.make-transmission
     :host host
     :port port
     :username username
     :password password
     :add-paused add-paused
     :trash-torrent-file trash-torrent-file
     :ratio ratio)))

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
Usage:
$ tget [--help] [--debug] [--cron] [--db database] [--auto-backup condition]
       [--root data-directory] [--reset] [--learn] [--feed-interval ndays]
       [--dump-series] [--dump-episodes] [--delete-episodes]

--help                 :: print this help text
--debug                :: debug mode (recommended for developers)
--cron                 :: quiet mode
--db database          :: path to database
--auto-backup { reset | program-update | schema-update | force | never }
                       :: automatically do a backup of the database:
                          'reset' when the database is being reset,
                          'program-update' when the program is version is
                          different than the last time the database was
                          updated, 'schema-update' the database schema is
                          changes, 'force' causes a database update, and
                          'never' to never make a backup.  The default is
                          to make backups under all
                          conditions above.
--root data-directory  :: change the data directory, the defaults is
                          $HOME/.tget.d/
--reset                :: reset database before beginning operation--this
                          removes *all* data from the database, so it is
                          recommended that you use --auto-backup force
                          before executing this.
--learn                :: don't download anything--useful in conjunction
                          with reset to wipe the database and start over,
                          or when starting to use tget for the first time.
--feed-interval ndays  :: set the feed interval to `ndays'.  Only useful
                          when a user-defined function of one argument is
                          given to defgroup's :rss-url option.

NOTE: each of the `name' arguments below, naming series, are canonicalized
      by removing single quotes (') and converting to lower case.

--dump-series name     :: dump all `series' objects matching series name `name'
--dump-episodes name   :: dump all `episode' objects matching series name `name'
--delete-episodes name :: delete episodes with series name matching `name'.
                          This is permanant!  Using --auto-backup force is
                          recommended.

Examples:
# Toss current database and catch up on shows released in the last 180 days
# marking them all as `downloaded'
$ tget --reset --learn --feed-interval 180
# Same, but to a `temp' database in the current directory:
$ tget --reset --learn --feed-interval 180 --root $PWD --db test.db

# Usage from Cron:
$ tget --cron

# Let's see what the series object for \"Regular Show\" looks like
# (the series name is not case sensitive):
$ tget --dump-series \"regular show\"
# these all refer to the same series:
$ tget --dump-series \"james mays man lab\"
$ tget --dump-series \"James Mays Man Lab\"
$ tget --dump-series \"James May's Man Lab\"

# To see the episodes of the above, you would:
$ tget --dump-episodes \"James May's Man Lab\"
")

(defvar *verbose*
    ;; Be noisy.  `nil' is used for cron mode.
    t)

(defvar *learn*
    ;; Don't download anything, just learn
    nil)

(defvar *kw-package* (find-package :keyword))

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
	      ("auto-backup" :long auto-backup :required-companion)
	      
	      ("dump-series" :long dump-series :required-companion)
	      ("dump-episodes" :long dump-episodes :required-companion)
	      ("delete-episodes" :long delete-episodes :required-companion))
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
	     (setq *database-name*
	       (merge-pathnames (pathname database)
				*tget-data-directory*))
	     (setq *version-file*
	       (pathname (format nil "~a/version.cl" database))))
	   
	   (when feed-interval
	     (when (not (match-re "^\\d+$" feed-interval))
	       (error "Bad --feed-interval: ~s." feed-interval))
	     (setq *feed-interval* (parse-integer feed-interval)))
	   
	   (when auto-backup
	     (setq *auto-backup*
	       (cond ((string= "never" auto-backup) nil)
		     ((string= "reset" auto-backup) :reset)
		     ((string= "program-update" auto-backup) :program-update)
		     ((string= "schema-update" auto-backup) :schema-update)
		     ((string= "force" auto-backup) :force)
		     (t
		      (error "Bad value for --auto-backup: ~a."
			     auto-backup)))))

	   (open-tget-database :if-exists (if* reset-database
					     then :supersede
					     else :open))
	   (load config-file :verbose *verbose*)
	   (open-log-files)
	   
	   (if* dump-series
	      then (let* ((series-name (canonicalize-series-name dump-series))
			  (series (query-series-name-to-series series-name)))
		     (if* series
			then (describe series)
			else (format t "No series named ~s.~%" dump-series)))
	    elseif dump-episodes
	      then (dolist (ep (query-episode
				:series-name
				(canonicalize-series-name dump-episodes)))
		     (describe ep))
	    elseif delete-episodes
	      then (dolist (ep (query-episode
				:series-name
				(canonicalize-series-name delete-episodes)))
		     (format t "removing ~a~%" ep)
		     (delete-instance ep))
		   (commit)
	      else (process-groups))
	   
	   (exit 0 :quiet t))))
    (if* *debug* ;; --debug doesn't effect this test!
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (top-level.debug:with-auto-zoom-and-exit (*standard-output*)
	      (doit)))))

(defun open-log-files (&key truncate)
  (when (and *log-file* (not *log-stream*))
    (and *verbose* (format t ";; Opening ~a log file...~%" *log-file*))
    (setq *log-stream*
      (open *log-file* :direction :output
	    :if-exists (if truncate :supersede :append)
	    :if-does-not-exist :create))
    (format *log-stream* "~%;; ~a~%~%" (ut-to-date-time (get-universal-time))))
	   
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
	     write-version-file
	     backed-up)
    
	(when (probe-file *database-name*)
	  ;; Check for the need to backup
	  (when (and (not backed-up)
		     schema-update
		     (or (eq 't *auto-backup*)
			 (eq :schema-update *auto-backup*)))
	    (backup-database "for schema update")
	    (setq backed-up t))
	  (when (and (not backed-up)
		     program-update
		     (or (eq 't *auto-backup*)
			 (eq :program-update *auto-backup*)))
	    (backup-database "for program update")
	    (setq write-version-file t)
	    (setq backed-up t))
	  (when (and (not backed-up)
		     (eq if-exists :supersede)
		     (or (eq 't *auto-backup*)
			 (eq :reset *auto-backup*)))
	    (backup-database "for database reset")
	    (setq backed-up t))
	  (when (and (not backed-up)
		     (eq :force *auto-backup*))
	    (backup-database "because requested")
	    (setq backed-up t)))
    
	(open-file-database *database-name*
			    :use :memory
			    :if-does-not-exist if-does-not-exist
			    :read-only nil
			    :if-exists if-exists)
	
	(if* (not (probe-file *version-file*))
	   then ;; New database, write it
		(setf (file-contents *version-file*)
		  (format nil "~s~%" stuff))
	 elseif (< (schema-version schema) *schema-version*)
	   then ;; the database schema version is older than the program's
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
			    *tget-version*)))
	 elseif write-version-file
	   then (setf (file-contents *version-file*)
		  (format nil "(:version ~d :tget-version ~s)~%"
			  *schema-version* *tget-version*)))

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

(defmethod db-upgrade ((version (eql 3)))
  ;; The change from 3 to 4: ...
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version)))

#+ignore
(defmethod db-upgrade ((version (eql 4)))
  ;; The change from 4 to 5: removed transmission-client slot of group
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (doclass (group (find-class 'group))
   group ;; quiet compiler
    )
  (commit))

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
			  repack
			  filename
			  container
			  source
			  codec
			  resolution
		     &aux temp)
  ;; Only make a new episode instance if one does not already exist.
  
  (if* (setq temp
	 (query-episode :series-name series-name
			:season season
			:ep-number episode
			:container container
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
	   :repack repack
	   :filename filename
	   :container container
	   :source source
	   :codec codec
	   :resolution resolution)))

(defun query-episode (&key series-name
			   season
			   ep-number
			   container
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
		     ,@(when container `((= container ,container)))
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
	       (= episode ,(episode-episode ep)))))
	   (episodes '()))
      (when cursor
	(loop
	  (let ((e (next-index-cursor cursor)))
	    (when (null e)
	      (free-index-cursor cursor)
	      (return))
	    ;; The quality slots we are comparing against are lists of
	    ;; items, so can't do that in the express-cursor
	    (when (quality-match-p e q) (push e episodes)))))
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

(defun episode-quality (ep &key priority)
  ;; Given an episode, return the name of the quality
  (doclass (q (find-class 'quality))
    (when (quality-match-p ep q)
      (return
	(if* priority
	   then (quality-priority q)
	   else (quality-name q))))))

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
    (tagbody
      ;; Setup the printer for this group.  The idea is to have no output
      ;; unless there are matches.
      (setf ep-printer
	(let ((first t))
	  (lambda (ep)
	    (when first
	      (format t "~&;; Processing group ~s~%" (group-name group))
	      (setq first nil))
	    (format t "~&~a [~d hours]~%"
		    ep (hours-available ep)))))
    
      ;; Convert the rss objects to episodes and process them.

      ;; Get all the episodes from the feed into the database, so we can
      ;; query against it.  When I removed non-interesting instances I got
      ;; errors due to accessing slots of deleted objects.  :(
      ;;
      (handler-case
	  (mapcar 'rss-to-episode
		  (maybe-log-rss (fetch-feed group)))
	(net.rss:feed-error (c)
	  (format t "~a~%" c)
	  (go next-feed)))
      (commit)
    
      ;; Now, the newly added objects in the database have their transient
      ;; slot set to `t'.  We can query against them, to see if we
      ;; need to download any of them, and and easily find them to remove
      ;; them when we're done.

      (process-transient-objects group ep-printer)
  
      ;; Remove the remaining transient episodes
      (dolist (ep (retrieve-from-index 'episode 'transient t :all t))
	(delete-instance ep))
      (commit)
     next-feed
      )))

(defun process-transient-objects (group ep-printer)
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
	 group
	 (if* (null (cdr matching-episodes))
	    then ;; easy, only one
		 matching-episodes
	    else (select-episodes matching-episodes))
	 ep-printer))))
  (commit))

(defun matching-episodes (group series episodes quality)
  ;; Return a list of episodes, from the EPISODES list, which are transient
  ;; episodes we have not yet decided to download, that have the quality
  ;; given by QUALITY, which can be:
  ;;   t (any quality)
  ;;   symbol (naming a function)
  ;;   keyword (naming a user-defined quality).
  ;;
  ;; NOTE: there are complete seasons and individual episodes in EPISODES.
  ;;
  (do* ((quality
	 ;; If it's a keyword, convert to quality instance
	 (quality quality))
	(eps episodes (cdr eps))
	(ep (car eps) (car eps))
	(res '())
	temp)
      ((null eps) res)
    (@log "matching: ~a" ep)
    (when (and
	   ;; Ignore whole seasons
	   (if* (eq :all (episode-episode ep))
	      then (@log "  ignore: full season")
		   nil
	      else t)
	   
	   ;; Ignore episode 0's
	   (if* (eql 0 (episode-episode ep))
	      then (@log "  ignore: ep 0")
		   nil
	      else t)
	   
	   ;; Make sure we don't already have it
	   (if* (setq temp
		  (query-episode :series-name (episode-series-name ep)
				 :season (episode-season ep)
				 :ep-number (episode-episode ep)
				 :transient nil))
	      then (if* (episode-repack ep)
		      then (cond
			    ((dolist (old temp nil)
			       (@log "  repack compare to old: ~a" old)
			       (when (and
				      (equal (episode-container old)
					     (episode-container ep))
				      (equal (episode-source old)
					     (episode-source ep))
				      (equal (episode-codec old)
					     (episode-codec ep))
				      (equal (episode-resolution old)
					     (episode-resolution ep)))
				 (return t)))
			     ;; Repack of an ep of the same quality as one
			     ;; in the database.  We might want it.
			     (@log "  is a repack")
			     t)
			    (t
			     (@log "  ignore: not our repack")
			     nil))
		      else (@log "  ignore: already have ep")
			   nil)
	      else (@log "  don't have ep")
		   t)

	   (if* (series-quality series)
	      then ;; We have a series quality override.  Only accept that.
		   (if* (eq (episode-quality ep)
			    (series-quality series))
		      then (@log "  quality == series override")
		      else (@log "  quality != series override")
			   nil)
	    elseif (quality-acceptable-p ep quality)
	      then (@log "  quality is good")
	      else (@log "  ignore: quality not good")
		   nil)
	   
	   ;; Check that we don't have a delay for this series or group.
	   (let ((delay (or (series-delay series)
			    (group-delay group)))
		 hours)
	     (if* (or (null delay) (= 0 delay))
		then (@log "  no delay")
	      elseif (>= (setq hours (hours-available ep)) delay)
		then (@log "  hours available (~d) >= delay (~d)" hours delay)
		else (@log "  ignore: hours available (~d) < delay (~d) "
			   hours delay)
		     nil)))
      ;; A good time to make sure the `series' slot in the episode
      ;; has a meaningful value.  (This change is not important enough to
      ;; warrant a commit.)
      (setf (episode-series ep) series)
      (@log "  => matching episode")
      (push ep res))))

(defun quality-acceptable-p (episode quality)
  ;; Return T if the quality of EPISODE is the same as that given by
  ;; QUALITY.
  ;;
  ;; This is a little more complex than just comparing the quality in the
  ;; arguments, since QUALITY can be a user-defined function which returns
  ;; a different quality based on time (since EPISODE was published).
  ;;
  (when (keywordp quality) (error "didn't expect a keyword here: ~s" quality))
  
  (when (eq 't quality)
    (@log "  quality-acceptable-p: quality=t, returning")
    (return-from quality-acceptable-p t))

  (when (symbolp quality)
    (@log "  user-defined quality function: ~s" quality)
    (setq quality (quality (funcall quality episode)))
    (@log "    => ~s" (or (quality-name quality)
			  quality)))
  
  (@log "  quality-acceptable-p: comparing")
  (@log "    episode=~a" episode)
  (@log "    quality=~a" quality)
  (if* (quality-match-p episode quality)
     then (@log "    matches")
	  t
     else (@log "    no match")
	  nil))

(defun quality-match-p (ep q &aux temp)
  ;; Match the quality in EP to Q.  Return T if there is a match.
  ;; For container, source, codec, or resolution, if any are `nil' in Q,
  ;; then that is the same as "any" and that slot `matches'.  Otherwise,
  ;; it's an `eq' test.  If Q is nil, that's a match for everything.
  ;;
  ;;NOTE: do NOT invoke the printer mechanism for EP here, since that would
  ;;      cause infinite recursion.
  (or (null q)
      (and (or (null (setq temp (quality-container q)))
	       (member (episode-container ep) temp :test #'eq))
	   (or (null (setq temp (quality-source q)))
	       (member (episode-source ep) temp :test #'eq))
	   (or (null (setq temp (quality-codec q)))
	       (member (episode-codec ep) temp :test #'eq))
	   (or (null (setq temp (quality-resolution q)))
	       (member (episode-resolution ep) temp
		       :test #'eq)))))

(defun select-episodes (episodes)
  ;; In the list of EPISODES, all from the same series, weed out dups that
  ;; differ only by quality and always choose the highest priority quality.
  ;;
  ;; strategy:
  ;;  - sort based on series-name, season, ep#, and priority (high to low)
  ;;  - iterate through skipping the dups
  (do* ((last nil)
	(eps (sort
	      (copy-list episodes)
	      (lambda (e1 e2)
		(if* (= (episode-season e1)
			(episode-season e2))
		   then (if* (episode-number= e1 e2)
			   then ;; sort priority high to low
				(let ((p1 (or (episode-quality e1 :priority t)
					      1))
				      (p2 (or (episode-quality e2 :priority t)
					      1)))
				  (> p1 p2))
			   else (episode-number< e1 e2))
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
			(episode-number= last this))))
      (push this res))
    (setq last this)))

(defun episode-number= (e1 e2)
  (equal (episode-episode e1) (episode-episode e2)))

(defun episode-number< (e1 e2)
  (let ((n1 (episode-episode e1))
	(n2 (episode-episode e2)))
    (if* (and (numberp n1) (numberp n2))
       then (< n1 n2)
     elseif (and (consp n1) (consp n2))
       then (let ((start1 (car n1))
		  (start2 (car n2)))
	      (< start1 start2))
     elseif (consp n1)
       then (< (car n1) n2)
     elseif (consp n2)
       then (< n1 (car n2))
       else (error "internal error: episode-number<: ~s ~s." n1 n2))))

(defun download-episodes (group episodes print-func)
  (dolist (episode episodes)
    (@log "download: ~a" episode)
    (funcall print-func episode)
    (setf (episode-transient episode) nil)
    (transmission-remote-download episode group))
  (commit))

(defun transmission-remote-download (episode group)
  (let* ((cmd
	  (format nil "~
transmission-remote ~a:~a ~
  --auth=~a:~a ~
  -a '~a' ~
  ~a ~
  -sr ~a ~@[--trash-torrent~*~] ~
  --download-dir '~a'"
		  (transmission-host *transmission-remote*)
		  (transmission-port *transmission-remote*)
		  (transmission-username *transmission-remote*)
		  (transmission-password *transmission-remote*)
		  (episode-torrent-url episode)
		  (if* (transmission-add-paused *transmission-remote*)
		     then "--start-paused"
		     else "--no-start-paused")
		  (or (group-ratio group)
		      (transmission-ratio *transmission-remote*))
		  (transmission-trash-torrent-file *transmission-remote*)
		  (group-download-path group))))
    (cond
     (*learn*
      (@log "cmd[not executed]: ~a" cmd))
     (t
      (multiple-value-bind (stdout stderr exit-status)
	  (excl.osi:command-output cmd :whole t)
	(@log "cmd: ~a" cmd)
	(@log "  exit status: ~a" exit-status)
	(when (/= 0 exit-status)
	  (@log "  stdout: ~a" stdout)
	  (@log "  stderr: ~a" stderr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed processing

(defvar *cached-feeds*
    ;; An alist of (feed-url . rss-objects)
    nil)

(defun fetch-feed (group
		   &aux (thing (group-rss-url group))
			temp url)
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
		 (if* (and *debug* (group-debug-feed group))
		    then ;; while debugging, use a static version of the
			 ;; feed, so we don't hammer the server.
			 (@log "using static feed for ~a" url)
			 (feed-to-rss-objects :file (group-debug-feed group))
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
	(match-re "(tvtorrents\\.com|broadcasthe\\.net)" source)
      (declare (ignore whole))
      (when (not match) (error "don't grok the feed source: ~s." source))
      (setq source (intern source-name *kw-package*)))
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
	  (push (intern (symbol-name sym) *kw-package*)
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
  (let (series-name season episode repack container source codec resolution
	uncut match whole year month day)
    (declare (ignore-if-unused whole))
    
    (when (=~ ".*\\.([A-Za-z0-9]+)$" filename)
      (setq container (intern $1 *kw-package*)))

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
	 else :sd))

    (multiple-value-setq (match whole series-name season episode)
      (match-re "^(.*)\\.s([0-9]{2,3})e([0-9]{2,3})" filename
		:case-fold t))
    (setq uncut (match-re "\\.uncut\\." filename :case-fold t))
    (setq repack (match-re "\\.repack\\." filename :case-fold t))
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

    (when series-name
      (setq series-name (canonicalize-series-name series-name)))
    
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

    (values series-name season episode repack container source codec
	    resolution)))

(defmethod convert-rss-to-episode ((type (eql :tvtorrents.com)) rss)
  (let ((des (rss-item-description rss))
	series-name season episode repack container source codec resolution
	uncut)
    (multiple-value-bind (match whole des-series-name title des-season
			  des-episode filename)
	(match-re
	 #.(concatenate 'simple-string
	     "Show Name:\\s*([^;]+)\\s*;\\s*"
	     "Show Title:\\s*(.+)\\s*;\\s*"
	     "Season:\\s*([0-9]+)\\s*;\\s*"
	     "Episode:\\s*("
	     "[0-9]+|"
	     "all|"
	     "complete|"
	     "special|"
	     "\\d\\d\\.\\d|"
	     "\\d\\d\\.\\d\\d|"
	     "\\d\\d\\.all|"
	     "\\d\\d\\d?-\\d\\d\\d?"
	     ")\\s*;\\s*"
	     "Filename:\\s*([^;]+);")
	 des
	 :case-fold t)
      (declare (ignore whole))
      (when (not match)
	(warn "TVT: couldn't parse rss description: ~s." des)
	(return-from convert-rss-to-episode nil))
      
      ;; ".UNCUT." is in the "Filename" but not the "Show Name", but
      ;; "(Uncut)" is in the "Show Title"!  We need to put it into the
      ;; series-name...
      (setq uncut (match-re "uncut" title :case-fold t))
      
;;;;TODO: early on, we need to lookup the series-name and see if we know
;;;;      about it.  If not, then we should bail and return `nil'.  No need
;;;;      to process something we don't care about.
  
      (setq des-season (parse-integer des-season))
      (setq des-episode
	(if* (or (equalp "all" des-episode)
		 (equalp "complete" des-episode))
	   then ;; The above is wrong, since it matches "all" and "01.all",
		;; but it doesn't matter since I don't download seasons
		:all
	 elseif (equalp "special" des-episode)
	   then :special
	 elseif (=~ "^(\\d\\d)\\.(\\d)$" des-episode)
	   then ;; e.g., 08.1 => return `8'
		(parse-integer $1)
	 elseif (and (= 6 (length des-episode))
		     (=~ "^(\\d\\d)\\.(?i:all)$" des-episode))
	   then ;; \\d\\d is the month -- this is a lie, but I don't care
		;; about these types of downloads:
		:all
	 elseif (=~ "^(\\d\\d\\d?)-(\\d\\d\\d?)$" des-episode)
	   then ;; a range of episodes
		(cons (parse-integer $1) (parse-integer $2))
	 elseif (= 5 (length des-episode))
	   then (if* (=~ "^(\\d\\d)\\.(\\d\\d)$" des-episode)
		   then (date-time-yd-day
			 (date-time (format nil "~a-~a-~a" des-season $1 $2)))
		   else (error "can't parse episode: ~s" des-episode))
	   else (parse-integer des-episode)))

      (multiple-value-setq (series-name season episode repack container
			    source codec resolution)
	(extract-episode-info-from-filename filename))

      (setq des-series-name (canonicalize-series-name des-series-name))

      ;; "(uncut)" is added if it was in the title
      (when uncut
	(setq des-series-name
	  (concatenate 'simple-string des-series-name " (uncut)")))
      (or repack
	  ;; See if it's a repack/proper from the title
	  (setq repack
	    (match-re "\\((repack|proper)\\)" (rss-item-title rss))))

      (when (and des-season season
		 (/= des-season season))
	(@log "Description and filename season do not agree (use 2nd): ~s, ~s."
	      des-season season))
      (when (or (and (numberp des-episode)
		     des-episode episode
		     (/= des-episode episode))
		(and (symbolp des-episode)
		     des-episode episode
		     (not (eq des-episode episode))))
	(@log "Description and filename episode do not agree (use 2nd): ~s, ~s."
	      des-episode episode))
      
      ;; sanity check
      (when (and des-series-name series-name
		 (not (equalp des-series-name series-name))
		 (not (equalp (strip-trailing-year des-series-name)
			      (strip-trailing-year series-name))))
	(@log "Description and filename series name do not agree (using ~
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
	:season (or season
		    ;; description version is wrong more often
		    des-season)
	:episode (or episode
		    ;; description version is wrong more often
		     des-episode)
	:repack repack
	:filename filename
	:container container
	:source (or source
		    (match-re "\\.mp4" (rss-item-title rss) :case-fold t)
		    :unknown)
	:codec (or codec
		   (match-re "\\.mp4" (rss-item-title rss) :case-fold t)
		   :unknown)
	;; for TVT it's always in the filename, but others it's in the title
	:resolution resolution))))

(defun strip-trailing-year (name)
  ;; Input:  "the americans (2013)"
  ;; Output: "the americans"
  (multiple-value-bind (match whole name-sans-year)
      (match-re "^(.*) \\(\\d\\d\\d\\d\\)$" name)
    (declare (ignore whole))
    (if match name-sans-year name)))

(defun canonicalize-series-name (name)
  ;; Canonicalize the series name
  ;;
  ;; downcase:
  (setq name (string-downcase name))
  ;; ...so "Tosh.0" becomes "Tosh 0"
  (setq name (replace-re name "\\." " "))
  ;; ...so "James May's Man Lab" becomes "James Mays Man Lab"
  (replace-re name "[']" ""))

(defmethod convert-rss-to-episode ((type (eql :broadcasthe.net)) rss)
  (let ((rss-des (rss-item-description rss))
	(rss-title (rss-item-title rss))
	series-name repack
	container source codec resolution)
    
    (when (not rss-des)
      ;; some sporting events have no description and only a title.  Ignore
      ;; these.
      (return-from convert-rss-to-episode nil))
    
    ;; Show name must be extracted from the rss-item-title
    (multiple-value-bind (found whole show-name)
	(match-re "^(.*) - " rss-title)
      (declare (ignore whole))
      (when (not found)
	(error "Couldn't find show name from title: ~s." rss-title))
      ;; Canonicalize the series name, so "Tosh.0" becomes "Tosh 0".
      (setq series-name (replace-re show-name "\\." " ")))
    
    (setq repack (match-re "\\.repack\\." rss-title :case-fold t))
    
    (when (=~ #.(concatenate 'simple-string
		  " ("
		  (excl:list-to-delimited-string *valid-containers* "|")
		  ") ")
	      rss-title :case-fold t)
      (setq container (intern (string-downcase $1) *kw-package*)))
    
    (when (=~ #.(concatenate 'simple-string
		  " (?i:("
		  (excl:list-to-delimited-string *valid-containers* "|")
		  ")) ")
	      rss-title #|:case-fold t|# ;; doesn't work
	      )
      (setq source (intern (string-downcase $1) *kw-package*)))
    
    (when (=~ #.(concatenate 'simple-string
		  " (?i:("
		  (excl:list-to-delimited-string *valid-codecs* "|")
		  ")) ")
	      rss-title #|:case-fold t|# ;; doesn't work
	      )
      (setq codec (intern (string-downcase $1) *kw-package*)))
    
    (when (=~ #.(concatenate 'simple-string
		  " (?i:("
		  (excl:list-to-delimited-string *valid-resolutions* "|")
		  ")) ")
	      rss-title #|:case-fold t|# ;; doesn't work
	      )
      (setq resolution (intern (string-downcase $1) *kw-package*)))

    ;; The rest of the things we're looking for are in the
    ;; rss-item-description, but weirdly, delimited by <br >/\n
    (multiple-value-bind (found whole des-title season episode)
	(match-re
	 "Episode Name:\\s*([^<]+)?<br />\\s*
Season:\\s*(\\d+)?<br />\\s*
Episode:\\s*(\\d+)?"
	 rss-des
	 :case-fold t
	 :multiple-lines t)
      (declare (ignore whole))
      (cond
       ((not found)
	;; If we can't parse the description, then we have nothing.
	;; Log it and move on.
	#+ignore ;; too many to log.  :(
	(@log "BTN: couldn't parse description: ~s." rss-des))
       (t
	(make-episode
	 :transient t
	 :full-title rss-title
	 :torrent-url (rss-item-link rss)
	 :pub-date (and (rss-item-pub-date rss)
			(parse-rss20-date (rss-item-pub-date rss)))
	 ;;both `nil' for BTN!
	 :type (rss-item-type rss)
	 :length (and (rss-item-length rss)
		      (parse-integer (rss-item-length rss)))
     
	 :series-name (string-downcase series-name)
	 :title des-title
	 :season (when (and season (string/= "" season)) (parse-integer season))
	 :episode (when (and episode (string/= "" episode))
		    (parse-integer episode))
	 :repack repack
	 ;; no :filename in BTN feed!
	 :container container
	 :source source
	 :codec codec
	 :resolution resolution))))))

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
  (cond
   (*log-stream*
    (when (null *log-prefix*)
      (setq *log-prefix* (format nil "~a: " (excl.osi:getpid))))
    (princ *log-prefix* *log-stream*)
    (apply #'format *log-stream* format-string args)
    (fresh-line *log-stream*))
   (*verbose*
    (apply #'format t format-string args)
    (fresh-line t)))
  ;; Return t so this function can be used in logic chains.
  t)
