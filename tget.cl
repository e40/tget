;; tget.cl -- download files from rss feeds for tv shows
;;   This program was inspired by flexget.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Only a few sites and their RSS feeds are supported.  TVT and BTN, for
;; now.  Some others are in development.  The problem is the RSS feeds from
;; many sites are completely worthly.  kickass torrents, for example.  You
;; can search and get an RSS feed on a search, but the individual results
;; for the search point not to .torrent files, but to other web pages.
;; This means tget would have to have a specialized parse for kickass
;; torrent torrent HTML pages.  Ugh.
;;
;; Here's how tget works:
;;
;; An object database, AllegroCache, is used to store series, episode and
;; other information.  Every episode downloaded by tget is stored in the
;; database.
;;
;; At program startup, the config file is loaded, and it can add or
;; substract series from the database.  The config file also defines
;; `groups', for people or combinations of people, though this is just how
;; the author uses them.  For each group defined in the config file, we:
;;
;; - Fetch the RSS feed, which exists in memory as a list of RSS objects.
;;
;; - RSS objects are turned into episode objects, by looking at the
;;   source of the feed (TVT, BTN, etc) and calling a specialized parser on
;;   each rss object.  The result is an episode object in the database,
;;   which is created with is marked as `transient'.  Transient episodes
;;   haven't been downloaded yet, but they are for series we have specified
;;   in the config file.  Episodes for series not in the config file are
;;   never turned into RSS objects.
;;
;; - Now, we iterate over each series in this group and find `matches' for
;;   transient episodes we just added to the database.  (See
;;   process-transient-objects for information on how the matching is done.)
;;   Matching episodes are downloaded and marked as non-transient. All
;;   remaining transient objects are deleted from the database.
;;
;; Problem:
;;  The creation of so many transient episodes, many of which are removed
;;  at the end of the group iteration, is very stressful on the database.
;;  Over time, the number of deleted objects grows so large that queries
;;  on the database take a very long time.  A typical run of tget on a
;;  compacted database takes a couple of seconds, at most.  After a month,
;;  it could take a minute or more.
;;
;; Possible solutions:
;;  1. Store the transient objects in a different database and only
;;     instantiate episodes in the main database once they are downloaded.
;;  2. In function matching-episodes, there is a query-episode call to see
;;     if a non-transient ep exists.  This shouldn't be necessary, because
;;     the series object has all the information about what we have
;;     downloaded.
;;       PROBLEM: shows like The Daily Show present a unique problem, since
;;          there is no real way to store the downloaded state in the
;;          series object.  Gaps are normal and sometimes large.
;;  3. A hybrid of the above two.  Normal series wouldn't need to store the
;;     the episodes.  How to identify the abnormal series, though?
;;
;; Solution #1 was implemented in version 2.0.
;; #3 might be OK, and would potentially shrink the database size a lot.
;; #2 is attractive, but I can't see how to make it work in all cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile eval load)
  (require :ssl)
  (require :ef-e-crcrlf)
  (require :rssreader)
  (require :aserve)
  ;; Just changing acache from 2.2.2 to 3.0.0,
  ;; timing for "make test" went from
  ;;   real	0m38.629s
  ;; to
  ;;   real	1m3.279s
  ;; 2.2.3's time is:
  ;;   real	0m37.939s
  ;;(require :acache "acache-2.2.3.fasl")
  (require :acache "acache-3.0.5.fasl")
  (require :autozoom))

(defpackage :user
  (:use #:excl #:util.date-time #:excl.shell)
  (:import-from #:db.allegrocache
		;; Specify each symbol instead of using the package,
		;; because we need to review the :db argument usage for
		;; each new function/macro.
		#:*allegrocache*
		#:close-database
		#:commit
		#:create-expression-cursor
		#:db-object-oid
		#:delete-instance
		#:doclass
		#:free-index-cursor
		#:next-index-cursor
		#:open-file-database
		#:restore-database
		#:retrieve-from-index
		#:save-database
		)
  (:import-from #:db.allegrocache.utils
		#:defclass*
		#:delete-persistent-class))

(in-package :user)

(eval-when (compile eval load)
(defvar *tget-version* "4.6")
)
(defvar *schema-version*
    ;; 1 == initial version
    ;; 2 == added `delay' slot
    ;; 3 == added schema versioning
    ;; 4 == added `container' slots to episode and quality
    ;;      added `priority' to quality
    ;;      added `repack' to episode
    ;; **** changed the name of an unused slot here, didn't need to change
    ;; **** schema based on my testing
    ;; 5 == changed series `last-episode' to `complete-to'
    ;; 6 == fix `container' and `repack' slots (in episode class)
    ;; 7 == add `pretty-epnum' to episode class
    ;; 8 == Fix `episode' & `pretty-epnum' for range episodes like for
    ;;      "Mad.Men.S06E01-E02.PROPER.HDTV.x264-2HD.mp4"
    ;; 9 == added `subdir' slot to series
    ;; 10 == added `date-based' slot to series
    ;; 11 == `group-rss-url' is now a list
    ;; 12 == new series `private' slot; episode `tracker' slot; class
    ;;       `group' is no longer persistent
    ;;
    ;; see the db-upgrade methods for details.
    12)

;; Adding this because I've noticed problems with aserve AND
;; transmission-remote using https.  Grrrrr.
(defvar *avoid-https* t)

;; Non-nil if we're using --add command line argument
(defvar *manual-add-mode* nil)

(defvar *tget-data-directory* "~/.tget.d/")
(defvar *auto-backup* t)
(defvar *main* nil)			; main acache database
(defvar *temp* nil)			; temp acache database
(defvar *database-main-name* nil)
(defvar *database-temp-name* nil)
(defvar *version-file* nil)
(defvar *config-file* nil)
(defvar *debug* nil)
(defvar *test* nil)
(defvar *init-forms* nil)

(defvar *log-rss-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [some] user-settable variables (in config file)

(defvar *log-rss*
    ;; if non-nil, a pathanme to log rss feed info
    nil)

(defvar *log-file*
    ;; if non-nil, a pathanme to log episode info
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; acache hackery

#+(version>= 9 0)
(eval-when (compile eval load) (push :clos-describe-hack *features*))

;; this should be part of AC!
#+clos-describe-hack
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

#+clos-describe-hack
(defmethod describe-object ((object quality) stream)
  (describe-persistent-clos-object object stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;THIS IS A DUMMY CLASS DEFINITION--the real group objects is a defstruct
;;;;and is defined below.
(defclass* group (:print nil :init nil)
  (name :index :any-unique)
  ;; rest of the slots removed
  )

(defmethod print-object ((obj group) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj) (if (slot-boundp obj 'name) (group-name obj)))))
   (t ;; print it for humans                                                    
    (format stream "#<OLDgroup ~s>" (group-name obj)))))

#+clos-describe-hack
(defmethod describe-object ((object group) stream)
  (describe-persistent-clos-object object stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* series (:conc-name t :print nil :init nil)
  ;; The group name is used to find all the series objects which are in a
  ;; group.  It is a keyword.
  (group :index :any)
  ;; Only used for presentation to the user
  pretty-name
  ;;canonicalized version of the series name
  ;; ***must match episode class naming***
  (name :index :any-unique)
  ;; The series is considered complete to this season/episode or season.
  ;; Format is `(season . episode)' or `season'.
  complete-to 
  ;; episodes that came in after complete-to but there are gaps
  discontinuous-episodes
  delay
;;;; override for group:
  quality
;;;; added in schema version 9:
  ;; Put episodes for this series in a subdirectory, because Plex Media
  ;; Service is really stupid about scanning and matching episodes.
  subdir
  ;; This series is date based, and as such cannot use the complete-to
  ;; mechanism, since we can't really tell what the sequence of episodes
  ;; will be named.  The Daily Show is like this.
  date-based
  ;; Non-nil if this series should only be downloaded from a private
  ;; tracker.
  private)

(defmethod print-object ((obj series) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (if (slot-boundp obj 'pretty-name) (series-pretty-name obj)))))
   (t ;; print it for humans
    (format stream "#<series ~a>" (series-pretty-name obj)))))

#+clos-describe-hack
(defmethod describe-object ((object series) stream)
  (describe-persistent-clos-object object stream))

(defclass* episode (:conc-name t :print nil :init nil)
  series				; the series object for this ep
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
  pretty-epnum				; for human consumption
  (repack :index :any)
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
  (transient :index :any)
;;;; non-persistent slots:
  ;; The tracker for this episode.  It's not persistent because we only
  ;; need it while making the decision to download the object, and after
  ;; we exit it could come from a different tracker next time (if we don't
  ;; download it).
  (tracker :allocation :instance))

(defmethod episode-p ((obj t)) nil)
(defmethod episode-p ((obj episode)) t)

(defmethod print-object ((obj episode) stream)
  (cond
   (*print-escape*
    (print-object-persistent-clos-object
     obj stream
     (lambda (obj)
       (format
	nil
	"~a~@[@~a~]~@[, REPACK~*~]~@[ ~a~]~@[; quality=~a~]; transient=~s"
	(when (slot-boundp obj 'series-name) (episode-series-name obj))
	(when (slot-boundp obj 'tracker)
	  (when (episode-tracker obj)
	    (tracker-name (episode-tracker obj))))	
	(when (slot-boundp obj 'repack) (episode-repack obj))
	(when (slot-boundp obj 'pretty-epnum) (episode-pretty-epnum obj))
	(pretty-episode-quality obj)
	(if* (slot-boundp obj 'transient)
	   then (episode-transient obj)
	   else "-unbound-")))))
   (t ;; print it for humans
    (format stream "#<~s~@[@~a~]~@[, REPACK~*~], ~a [~a]>"
	    (episode-series-name obj)
	    (when (slot-boundp obj 'tracker)
	      (when (episode-tracker obj)
		(tracker-name (episode-tracker obj))))
	    (episode-repack obj)
	    (episode-pretty-epnum obj)
	    (pretty-episode-quality obj)))))

(defun pretty-episode-quality (ep &key (separator #\,) &aux name all-bound)
  ;; Ignore priority of quality.
  (if* (and (slot-boundp ep 'container)
	    (slot-boundp ep 'source)
	    (slot-boundp ep 'codec)
	    (slot-boundp ep 'resolution)
	    (setq all-bound t)
	    (setq name (episode-quality ep)))
     then (princ-to-string name)
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
     then (list-to-delimited-string
	   (append
	    (when (slot-boundp ep 'container)
	      (list (princ-to-string (episode-container ep))))
	    (when (slot-boundp ep 'source)
	      (list (princ-to-string (episode-source ep))))
	    (when (slot-boundp ep 'codec)
	      (list (princ-to-string (episode-codec ep))))
	    (when (slot-boundp ep 'resolution)
	      (list (princ-to-string (episode-resolution ep)))))
	   separator)
     else "undefined"))

#+clos-describe-hack
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
  length
  fileName)

(defstruct schema
  version
  tget-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tracker
  name
  url
  debug-feed
  public
  download-delay
  disabled
  ratio
  upload-limit)

(defvar *tracker-name-to-tracker* (make-hash-table :size 777 :test #'eq))
(push '(clrhash *tracker-name-to-tracker*)
      *init-forms*)

(defun tracker-name-to-tracker (tracker-name)
  (gethash tracker-name *tracker-name-to-tracker*))

(defun (setf tracker-name-to-tracker) (tracker tracker-name)
  (setf (gethash tracker-name *tracker-name-to-tracker*) tracker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used to be a class called `group', can't use name
(defstruct (xgroup (:conc-name group-))
  name					; a keyword
;;;; one of the next to
  ;; rss-url is legacy and kept for compatibility
  rss-url				; a list of URLs
  trackers
;;;;
  delay
  ratio
  quality
  download-path)



(defvar *group-name-to-group* (make-hash-table :size 777 :test #'eq))
(push '(clrhash *group-name-to-group*)
      *init-forms*)

(defun group-name-to-group (group-name)
  (gethash group-name *group-name-to-group*))

(defun (setf group-name-to-group) (group group-name)
  (setf (gethash group-name *group-name-to-group*) group))

(defmethod print-object ((obj xgroup) stream)
  (format stream "#<group ~s>" (group-name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (transmission
	    (:constructor .make-transmission 
			  (&key host port username password add-paused
				trash-torrent-file ratio
				docker-container-name docker-user
				ssh-identity ssh-user)))
  host
  port
  username
  password
  add-paused
  trash-torrent-file
  ratio
  docker-container-name
  docker-user
  ssh-identity
  ssh-user)

(defvar *torrent-handler* nil)
(push '(setq *torrent-handler* nil)
      *init-forms*)

(define-condition tget (error) ())

(defun .error (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'tget :format-control format-string
	 :format-arguments format-arguments))

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
	  (.error "Bad ~a value: ~s." what atom)))
    atom-or-list))

(defun make-quality (&key name priority container source codec resolution)
  (let ((old (retrieve-from-index 'quality 'name name :db *main*)))

    (or (and (null priority)
	     (setq priority 1))
	(numberp priority)
	(.error "Priority must be a number: ~s." priority))
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
       else (let ((*allegrocache* *main*))
	      (make-instance 'quality
		:name name
		:priority priority
		:container container
		:source source
		:codec codec
		:resolution resolution)))))

(defmacro deftracker (name &key url debug-feed public download-delay
				disabled ratio upload-limit)
  `(.make-tracker
    :name ,name
    :url ,url
    :debug-feed ,debug-feed
    :public ,public
    :download-delay ,download-delay
    :disabled ,disabled
    :ratio ,ratio
    :upload-limit ,upload-limit))

(defmacro .make-tracker (&key name url debug-feed public download-delay
			      disabled ratio upload-limit)
  (when (tracker-name-to-tracker name)
    (.error "Tracker ~s defined more than once in config file." name))
  (check-url "Tracker :url" url)
  ;; Don't check debug-feed because it's a function
  (check-integer "Tracker :download-delay" download-delay)
  (check-integer "Tracker :upload-limit" upload-limit)
  (check-ratio ratio)
  (setf (tracker-name-to-tracker name)
    (make-tracker
     :name name
     :url url
     :debug-feed debug-feed
     :public public
     :download-delay download-delay
     :disabled disabled
     :ratio ratio
     :upload-limit upload-limit)))

(defmacro defgroup (name &key trackers rss-url delay ratio quality
			      download-path)
  `(.make-group
    :name ,name
    :trackers ,trackers
    :rss-url ,rss-url
    :delay ,delay
    :ratio ,ratio
    :quality ,quality
    :download-path ,download-path))

(defun .make-group (&key name trackers rss-url delay ratio quality
			download-path)
  (let ((old (group-name-to-group name)))
    (setq trackers (check-trackers trackers))
    (check-rss-url rss-url)
    (check-delay delay)
    (check-ratio ratio)
    (check-quality quality)
    (setq download-path (namestring download-path))
    (if* old
       then (setf (group-trackers old) trackers)
	    (setf (group-rss-url old) rss-url)
	    (setf (group-delay old) delay)
	    (setf (group-ratio old) ratio)
	    (setf (group-quality old) quality)
	    (setf (group-download-path old) download-path)
	    old
       else (setf (group-name-to-group name)
	      (make-xgroup
	       :name name
	       :trackers trackers
	       :rss-url (if* (consp rss-url)
			   then rss-url
			   else (list rss-url))
	       :delay delay
	       :ratio ratio
	       :quality quality
	       :download-path download-path)))))

(defmacro defseries (name group &key delay quality remove catch-up subdir
				     date-based aliases private)
  (if* remove
     then `(forget-series ,name :noisy nil)
     else `(make-series
	    :name ,name
	    :group ,group
	    ,@(when subdir `(:subdir ,subdir))
	    ,@(when date-based `(:date-based ,date-based))
	    ,@(when catch-up `(:catch-up ,catch-up))
	    ,@(when delay `(:delay ,delay))
	    ,@(when quality `(:quality ,quality))
	    ,@(when aliases `(:aliases ',aliases))
	    ,@(when private `(:private ',private)))))

(defun forget-series (name &key (noisy t))
  (let* ((series-name (canonicalize-series-name name))
	 (s (query-series-name-to-series series-name)))
    (if* s
       then (format t "removing series ~a~%" s)
	    (delete-instance s)
	    (tget-commit *main*)
     elseif noisy
       then (warn "Could not find series: ~s." name))))

;; The following are not persistent and don't need to be.
;; They are initialized by loading the config file and are
;; used while processing episodes from the RSS feeds.
(defvar *all-series-names* (make-hash-table :size 777 :test #'equal))
(push '(clrhash *all-series-names*)
      *init-forms*)

(defvar *series-name-aliases*
    ;; Key is a string naming a series, the value is the real name of the
    ;; series.
    (make-hash-table :size 777 :test #'equal))
(push '(clrhash *series-name-aliases*)
      *init-forms*)

(defun make-series (&key name group delay quality catch-up subdir
			 date-based aliases private
		    &aux series)
  (let* ((pretty-name name)
	 (name (canonicalize-series-name name))
	 (old (query-series-name-to-series name)))
    (check-delay delay)
    (check-quality quality)
    (check-group group)
    (or (stringp name)
	(.error "Series name must be a string: ~s." name))
    (when catch-up
      (or (stringp catch-up)
	  (.error "Series catch-up must be a string: ~s." catch-up)))
    (when subdir
      (or (stringp subdir)
	  (.error "Series subdir must be a string: ~s." subdir)))
    (check-boolean "Series :date-based option" date-based)
    (check-boolean "Series :private option" private)
    (when aliases
      (or (null aliases)
	  (dolist (alias aliases)
	    (or (stringp alias)
		(.error "Non-string series alias: ~s." alias)))))
    (setq series
      (if* old
	 then (when (not (eq group (series-group old)))
                (warn "Series ~s moved groups (~a to ~a)."
		      pretty-name (series-group old) group))
	      (when (string/= (series-name old) pretty-name)
		(setf (series-pretty-name old) pretty-name))
	      (setf (series-group old) group)
	      (setf (series-delay old) delay)
	      (setf (series-quality old) quality)
	      (setf (series-subdir old) subdir)
	      (setf (series-date-based old) date-based)
	      (setf (series-private old) private)
	      old
	 else (let ((*allegrocache* *main*))
		(make-instance 'series
		  :pretty-name pretty-name
		  :name name
		  :complete-to nil
		  :discontinuous-episodes nil
		  :group group
		  :delay delay
		  :quality quality
		  :subdir subdir
		  :date-based date-based
		  :private private))))
    (setf (gethash pretty-name *all-series-names*) t)
    (dolist (alias aliases)
      ;; Enter the aliases to be used during RSS to EPISODE conversion.
      (setf (gethash (canonicalize-series-name alias) *series-name-aliases*)
	name))
    (when catch-up
      (catch-up-series (concatenate 'simple-string name " " catch-up)
		       :series series))
    series))

(defun dump-orphaned-series (delete)
  (doclass (series (find-class 'series) :db *main*)
    (let ((name (series-pretty-name series)))
      (when (not (gethash name *all-series-names*))
	(format t "Orphaned series: ~a" name)
	(when delete
	  (format t "...removing...")
	  (forget-series name :noisy nil)
	  (format t "done."))
	(format t "~%"))))
  (doclass (ep (find-class 'episode) :db *main*)
    (when (null (episode-series ep))
      (let ((s (query-series-name-to-series (episode-series-name ep))))
	(if* s
	   then ;; Got lost somehow... fix it
		(format t ";; Fixing series slot of ~a~%" ep)
		(setf (episode-series ep) s)
	   else (format t "Orphan episode: ~a" ep)
		(when delete
		  (format t "...removing...")
		  (delete-instance ep)
		  (format t "done."))
		(format t "~%")))))
  (tget-commit *main*))

#+ignore ;; not used nor documented
(defmacro deftransmission (options &key host port username password
					add-paused trash-torrent-file
					ratio docker-container-name
					docker-user ssh-identity ssh-user)
  (declare (ignore options)) ;; just for indentation
  (let ((g-host (gensym "host"))
	(g-port (gensym "port"))
	(g-username (gensym "username"))
	(g-password (gensym "passwd"))
	(g-add-paused (gensym "addpaused"))
	(g-trash-torrent-file (gensym "trash"))
	(g-ratio (gensym "ratio"))
	(g-docker-container-name (gensym "dockername"))
	(g-docker-user (gensym "dockeruser"))
	(g-ssh-user (gensym "sshuser"))
	(g-ssh-identity (gensym "sshid")))
    `(let* ((,g-host ,host)
	    (,g-port ,port)
	    (,g-username ,username)
	    (,g-password ,password)
	    (,g-add-paused ,add-paused)
	    (,g-trash-torrent-file ,trash-torrent-file)
	    (,g-ratio ,ratio)
	    (,g-docker-container-name ,docker-container-name)
	    (,g-docker-user ,docker-user)
	    (,g-ssh-user ,ssh-user)
	    (,g-ssh-identity ,ssh-identity))
       (set-torrent-handler
	(make-transmission-remote-handler
	 :host ,g-host
	 :port ,g-port
	 :username ,g-username
	 :password ,g-password
	 :add-paused ,g-add-paused
	 :trash-torrent-file ,g-trash-torrent-file
	 :ratio ,g-ratio
	 :docker-container-name ,g-docker-container-name
	 :docker-user ,g-docker-user
	 :ssh-user ,g-ssh-user
	 :ssh-identity ,g-ssh-identity)))))

(defun make-transmission-remote-handler
    (&key host port username password add-paused trash-torrent-file ratio
	  docker-container-name docker-user ssh-identity ssh-user)
  (or (stringp host)
      (.error "transmission host is not a string: ~s." host))
  (or (numberp port)
      (and (stringp port)
	   (numberp (setq port
		      (ignore-errors
		       (parse-integer port :junk-allowed nil)))))
      (.error "transmission port is not a number or string: ~s."
	      port))
  (or (stringp username)
      (.error "transmission username is not a string: ~s."
	      username))
  (or (stringp password)
      (.error "transmission password is not a string: ~s."
	      password))
  (check-ratio ratio)
  
  (or (null docker-container-name)
      (stringp docker-container-name)
      (.error "docker-container-name is not a string: ~s." docker-container-name))
  (or (null docker-user)
      (stringp docker-user)
      (.error "docker-user is not a string: ~s." docker-user))
  
  (or (null ssh-identity)
      (stringp ssh-identity)
      (.error "ssh-identity is not a string: ~s." ssh-identity))
  (or (null ssh-user)
      (stringp ssh-user)
      (.error "ssh-user is not a string: ~s." ssh-user))
  
  (.make-transmission
   :host host
   :port port
   :username username
   :password password
   :add-paused add-paused
   :trash-torrent-file trash-torrent-file
   :ratio ratio
   :docker-container-name docker-container-name
   :docker-user docker-user
   :ssh-user ssh-user
   :ssh-identity ssh-identity))

;; for compatibility
(setf (symbol-function 'make-transmission-remote)
  (symbol-function 'make-transmission-remote-handler))

(defun set-torrent-handler (thing)
  (when *torrent-handler*
    (.error "There are multiple calls to set-torrent-handler in config file."))

  (cond
   ((transmission-p thing)
    ;; checking is in make-transmission-remote-handler
    )
   ((pathnamep thing)
    (when (not (probe-file thing))
      (.error "torrent handler points to non-existent directory: ~a."
	      thing)))
   (t (.error "unknown torrent handler: ~s." thing)))
  (setq *torrent-handler* thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-url (what item)
  (when item
    (or (and (stringp item) (=~ "^http" item))
	(.error "~a must be a URL: ~s." what item))))

(defun check-filename (what item)
  (when item
    (or (and (or (stringp item)
		 (pathnamep item))
	     (probe-file item))
	(.error "~a must be the name of an existing file: ~s." what item))))

(defun check-integer (what item)
  (when item
    (or (integerp item)
	(.error "~a must be an integer: ~s." what item))))

(defun check-boolean (what item)
  (when item
    (or (eq 't item)
	(.error "~a must be `t' or `nil': ~s." what item))))

(defun check-rss-url (rss-url)
  (and rss-url
       (and (dolist (u (if (consp rss-url) rss-url (list rss-url)))
	      (if* (or (symbolp u)
		       (and (stringp u)
			    (match-re "^http" u)))
		 thenret
		 else (return t)))
	    (.error "Bad rss-url: ~s." rss-url))))

(defun check-trackers (tracker-thing &aux tracker (trackers '()))
  (and tracker-thing
       (dolist (tracker-name (if* (consp tracker-thing)
			     then tracker-thing
			     else (list tracker-thing))
		 (nreverse trackers))
	 (when (not (symbolp tracker-name))
	   (.error "Bad tracker name: ~s." tracker-name))
	 (when (not (setq tracker (tracker-name-to-tracker tracker-name)))
	   (.error "Unknown tracker: ~s." tracker-name))
	 (push tracker trackers))))

(defun check-delay (delay)
  (and delay
       (or (numberp delay)
	   (.error "Bad delay, must be a number: ~s." delay))))

(defun check-ratio (ratio)
  (and ratio
       (or (and (stringp ratio)
		(match-re "^-?[0-9.]+$" ratio))
	   (floatp ratio)
	   (.error "Bad ratio: ~s." ratio))))

(defun check-group (group-name)
  (and group-name
       (or (keywordp group-name)
	   (.error "Bad group: ~s." group-name))))

(defun check-quality (quality)
  (and quality
       (or (and (symbolp quality)
		(or (retrieve-from-index 'quality 'name quality :db *main*)
		    (keywordp quality)
		    (eq 't quality)
		    (symbol-function quality)
		    (.error "Quality ~s does not exist."
			    quality)))
	   (.error "Bad quality: ~s." quality))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(eval-when (compile eval load)
(defvar *usage*
    "
## Usage

Primary behavior determining arguments (one of these must be given):

    --run
    --add file-or-directory
    --catch-up   
    --catch-up-series series-episode-name
    --check-database
    --clean-database
    --compact-database
    --delete-episodes series-name
    --delete-episode episode-description
    --delete-orphans
    --delete-series series-name
    --dump-all
    --dump-complete-to
    --dump-episodes series-name
    --dump-orphans
    --dump-series series-name
    --dump-stats
    --skip

Behavior modifying arguments:

    --auto-backup condition
    --config file
    --cron
    --db database-name
    --debug
    --force
    --learn
    --reset
    --root data-directory
    --verbose or -v
")

(defvar *help*
    "
### Usage details

The tget options are below.  When there is an argument naming series,
these are canonicalized by removing single quotes and converting to lower
case.  However, the series names presented to you will be stored in their
original form.

* `--help`

  Print full help text and exit.

The following are arguments controlling primary behavior:

* `--run`

  The primary mode of operation, whereby RSS feeds are retrieved, searched
  for new episodes and those episode torrents downloaded.

* `--add thing`

  If `thing` is a file, it should be a `.torrent` file, which is 
  manually added.  If `thing` is a directory, then all the `.torrent` files
  in the directory are added.  This circumvents any matching and assumes
  the episodes should be downloaded.  The series name,episode and
  season numbers are taken directly from the file name.  If the information
  cannot be extracted, you can rename the files to suit `tget`.

  If the episode has already been downloaded, then `--force` is required
  to make `tget` download it again.

* `--catch-up`

  Go through the database and make the newest episode of each series the
  oldest episode that will ever be downloaded for that series; this
  prevents old episodes, which are released from time to time, from being
  downloaded.

* `--catch-up-series series-ep`

  Catch series up to the episode given in the companion argument.
  See examples below.

* `--check-database`

  Report on items in the database which can be cleaned up.

* `--clean-database`

  Remove items reported by `--check-database`.

* `--compact-database`

  This saves and restores the database, compacting it at the same time.

* `--delete-episodes series-name`

  Delete episodes with series name matching `series-name`.  This is permanent!
  Using this option with --auto-backup force is recommended.

* `--delete-episode episode-description`

  Delete the episode matching `episode-description`.  This is permanent!
  Using this option with --auto-backup force is recommended.  Example:

    --delete-episode \"I Love Lucy S05E01\"

* `--delete-orphans`

  Delete orphaned series and episodes from the database.  See
  `--dump-orphans` for more information.

* `--delete-series series-name`

  Delete series with series name matching `series-name`.  This is permanent!
  Using this option with --auto-backup force is recommended.

* `--dump-all`

  Dump all `episode` objects to stdout.

* `--dump-complete-to`

  Dump a table of series and last downloaded information for all series in
  the database to stdout.  See --catch-up-series.

* `--dump-episodes series-name`

  Dump all episode objects matching series name `series-name` to stdout.

* `--dump-orphans`

  Dump orphaned series and episodes.  Orphaned series are those which do
  not appear in the config file but exist in the database.  Orphaned
  episodes are those are in the database but have no corresponding series
  object.

* `--dump-series series-name`

  Dump all series objects matching series name `series-name` to stdout.

* `--dump-stats`

  Dump information about the database to stdout.

* `--skip series-name`

  Skip the next episode of `series-name`.  It does so by using the last
  downloaded episode and incrementing it by 1.

  Note: this has no effect on date-based series.  See the `:date-based`
  option to `defseries`.

The following options augment the options above or have the stated side
effects:

* `--auto-backup {reset|program-update|schema-update|restore|force|never}`

  Perform a backup of the database under specific conditions given by the
  companion argument:

  * `compact` - when the database is being compacted
  * `force` - always
  * `never` - never
  * `program-update` - when the program changed since the last db update
  * `reset` - when the database is being reset
  * `schema-update` - when the schema changes

  The default is to make backups for all conditions above.

* `--config file`

  Load `file` as the configuration file instead of one of the built-in
  defaults.  The default list (searched in this order) is to load the first
  file found in the list:

  * `config.cl` in the same directory as the program
  * `$HOME/.tget.cl`
  * `config.cl` in the tget data directory (see the --root option)

* `--cron`

  Quiet mode, for cron jobs.

* `--db database`

  The name of the database.  The default is `db` in the tget data
  directory (see the --root).  This name will itself become a directory.

* `--debug`

  Run in debug mode.  In this mode, torrents are not downloaded and the
  debug feed defined by the configuration file is used.  Also, the program
  is more verbose.  This is for testing and is not recommended.  It implies
  `--learn`.

* `--force`

  The meaning of this argument depends on the other arguments and context
  in which it is given.

* `--learn`

  Mark episodes which match the configuration as _downloaded_.  This is
  useful when using tget for the first time.  See the examples below.

* `--reset`

  Reset database before beginning operation--this removes all data
  from the database.  The default --auto-backup settings will cause a
  backup to be performed before the reset.

* `--root data-directory`

  Change the data directory, the defaults is $HOME/.tget.d/

* `--verbose` or `-v`

  Verbose mode.  For now, it makes `--dump-episodes` print detailed
  information on episodes.

Examples:

Toss current database and catch up on shows released in the last 180 days
marking them all as `downloaded'

    $ tget --reset --learn

The usefulness of this is highly dependent on how far the feed for the site
you are using goes back.  Many sites do not have deep feeds, but some sites
have parameters that allow you to go back in time.  Sadly, this feature is
rare these days, as it is expensive to support.

Same, but to a `temp' database in the current directory:

    $ tget --reset --learn --root $PWD --db test.db

Let's see what the series object for \"Regular Show\" looks like.
The series name is not case sensitive:

    $ tget --dump-series \"regular show\"

These all refer to the same series:

    $ tget --dump-series \"james mays man lab\"
    $ tget --dump-series \"James Mays Man Lab\"
    $ tget --dump-series \"James May's Man Lab\"

To see the episodes of the above, you could use any of the variations on
the names given, for example:

    $ tget --dump-episodes \"James May's Man Lab\"

Compact the database:

    $ tget --compact-database

Catch up series to a specific episode:

    #   note that episodes before s04e21 have been downloaded:
    $ tget --catch-up-series \"regular show s04e20\"
    #   note that all of season 4 has been downloaded:
    $ tget --catch-up-series \"breaking bad s04\"
")
)

(eval-when (compile)
  (let ((temp-file (sys:make-temp-file-name))
	(readme-md "README.md"))
    (format t ";; Maybe make ~a...~%" readme-md)
    (unwind-protect
	(progn
	  (with-open-file (s temp-file :direction :output
			   :if-exists :supersede)
	    (with-open-file (ug "userguide.md")
	      (let (line)
		(loop
		  (setq line (read-line ug nil ug))
		  (when (eq line ug) (return))
		  (cond
		   ((=~ "^(.*)(?:\\$\\{([^}]+)\\})(.*)$" line)
		    (let ((sym (intern $2 *package*)))
		      (when (not (boundp sym))
			(error "user::~a does not have a value." $2))
		      (format s "~a~a~a~%" $1 (symbol-value sym) $3)))
		   ((=~ "^%%VALUE:\\s+(.*)\\s*$" line)
		    (let ((sym (intern $1 *package*)))
		      (when (not (boundp sym))
			(error "user::~a does not have a value." $1))
		      (let ((v (symbol-value sym)))
			(if* (consp v)
			   then (format s "    ~{~s~^, ~}~%" v)
			   else (format s "    ~s~%" v)))))
		   (t
		    (write line :stream s :escape nil)
		    (terpri s))))))
      
	    (format s *usage*)
	    (format s *help*)
    
	    (with-open-file (cfg "config.cl" :direction :input)
	      (format s "~%## Example configuration file~%~%")
	      (let (line)
		(loop
		  (setq line (read-line cfg nil cfg))
		  (when (eq line cfg) (return))
		  (write "    " :stream s :escape nil)
		  (write line :stream s :escape nil)
		  (terpri s))))
	    
	    (sys:copy-file "userguide_refs.md" s))
	  (when (not (excl::compare-files temp-file readme-md))
	    (format t ";; Updating ~a...~%" readme-md)
	    (delete-file readme-md)
	    (rename-file-raw temp-file readme-md)))
      (when (probe-file temp-file) (delete-file temp-file)))))

(defvar *learn*
    ;; Don't download anything, just learn
    nil)

(defvar *kw-package* (find-package :keyword))

(defun usage (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'tget
	 :format-control (concatenate 'simple-string
			   "Error: "
			   format-string
			   "~%~a~%")
	 :format-arguments (nconc format-arguments (list *usage*))))

(defun reset-program-state ()
  (dolist (form *init-forms*)
    (eval form)))

(defun main ()
  (setq *global-gc-behavior* :auto)
  (labels
      ((done () (exit 0 :quiet t))
       (doit (&aux verbose)
	 (system:with-command-line-arguments
	     (("help" :long help)
	      
;;;; primary arguments (determine behavior)
	      ("add" :long add-mode :required-companion)
	      ("run" :long run-mode)
	      ("catch-up" :long catch-up-mode)
	      ("catch-up-series" :long catch-up-series :required-companion)
	      ("check-database" :long check-database)
	      ("clean-database" :long clean-database)
	      ("compact-database" :long compact)
	      ("delete-episodes" :long delete-episodes :required-companion)
	      ("delete-episode" :long delete-episode :required-companion)
	      ("delete-orphans" :long delete-orphans)
	      ("delete-series" :long delete-series :required-companion)
	      ("dump-all" :long dump-all)
	      ("dump-complete-to" :long dump-complete-to)
	      ("dump-episodes" :long dump-episodes :required-companion)
	      ("dump-orphans" :long dump-orphans)
	      ("dump-series" :long dump-series :required-companion)
	      ("dump-stats" :long dump-stats)
	      ("skip" :long skip-next :required-companion)

;;;; modifiers (change primary behavior)
	      ("auto-backup" :long auto-backup :required-companion)
	      ("config" :long config-file :required-companion)
	      ("cron" :long quiet)
	      ("db" :long database :required-companion)
	      ("debug" :long debug-mode)
	      ("feed-interval" :long feed-interval :required-companion)
	      ("force" :long force-mode)
	      ("learn" :long learn-mode)
	      ("reset" :long reset-database)
	      ("root" :long root :required-companion)
	      ("verbose" :long verbose-long)
	      ("v" :short verbose-short :allow-multiple-options))
	     (extra-args :usage *usage*)
	   (setq verbose (or verbose-short verbose-long))
	   (when (and verbose (not (numberp verbose)))
	     (setq verbose 1))
	   (when help
	     (format t "~a~&" *usage*)
	     (format t "~a~&" *help*)
	     (done))
	   (when extra-args
	     (usage "extra arguments:~{ ~a~}." extra-args))
	   
	   (when root
	     (or (probe-file root)
		 (usage "--root directory does not exist: ~a." root))
	     (setq *tget-data-directory* (pathname-as-directory root)))
	   
	   (setq *database-main-name*
	     ;; This should *not* end in a slash:
	     (merge-pathnames "db" *tget-data-directory*))
	   (setq *database-temp-name*
	     ;; This should *not* end in a slash:
	     (merge-pathnames "temp" *tget-data-directory*))
	   (setq *version-file*
	     ;; Set in main, if *database-main-name* changed
	     (merge-pathnames "db/version.cl" *tget-data-directory*))
	   (setq *config-file*
	     ;; First one wins:
	     (list "sys:config.cl"
		   "~/.tget.cl"
		   (merge-pathnames "config.cl" *tget-data-directory*)))
	   
	   (setq *debug* debug-mode)
	   (setq *verbose* (or verbose
			       (when quiet 0)
			       1))
	   (setq *learn* learn-mode)
	   
	   (if* config-file
	      then (or (probe-file config-file)
		       (usage "--config file does not exist: ~a." config-file))
	    elseif (consp *config-file*)
	      then (when (dolist (config *config-file* t)
			   (when (probe-file config)
			     (setq config-file config)
			     (return nil)))
		     (usage "None of these config files exists:~{ ~a~}."
			    *config-file*))
	      else (usage "Internal error: bad *config-file* value: ~s."
			  *config-file*))
	   
	   (when database
	     ;; Remove trailing slash, if there is one
	     (when (=~ "(.*)/$" database) (setq database $1))
	     (setq *database-main-name*
	       (merge-pathnames (pathname database) *tget-data-directory*))
	     (setq *version-file*
	       (pathname (format nil "~a/version.cl" database))))
	   
	   (when feed-interval
	     (warn "The --feed-interval argument has been removed."))
	   
	   (when auto-backup
	     (setq *auto-backup*
	       (cond
		((string= "compact" auto-backup) :compact)
		((string= "force" auto-backup) :force)
		((string= "never" auto-backup) nil)
		((string= "program-update" auto-backup) :program-update)
		((string= "reset" auto-backup) :reset)
		((string= "schema-update" auto-backup) :schema-update)
		(t (usage "Bad value for --auto-backup: ~a." auto-backup)))))

	   (handler-case
	       (open-tget-database :if-exists (if* reset-database
						 then :supersede
						 else :open)
				   :if-does-not-exist
				   (if* (or dump-all dump-complete-to
					    dump-stats dump-series
					    delete-orphans dump-orphans
					    dump-episodes
					    delete-episodes delete-episode
					    delete-series
					    catch-up-mode catch-up-series
					    check-database clean-database)
				      then :error
				      else :create)
				   :compact compact)
	     (error (c)
	       ;; no backtrace for this one
	       (format t "~a~&" c)
	       (exit 1 :quiet t)))
	   ;; Must be done before the loading of the config file!
	   (reset-program-state)
	   (load config-file :verbose (> *verbose* 0))
	   (when (not *torrent-handler*)
	     (usage "*torrent-handler* is not defined in config file."))
	   (open-log-files)

	   (if* dump-all
	      then (doclass (ep (find-class 'episode) :db *main*)
		     (if* verbose
			then (describe ep)
			else (format t "~a~%" ep)))
		   (done)
	    elseif dump-complete-to
	      then (let ((res '()))
		     (doclass (series (find-class 'series) :db *main*)
		       (push
			(format nil "~55a: ~a~%"
				(series-pretty-name series)
				(if* (series-complete-to series)
				   then (pretty-season-and-episode
					 (car (series-complete-to series))
					 (cdr (series-complete-to series)))
				   else "--"))
			res))
		     (setq res (sort res #'string<))
		     (dolist (line res) (write line :escape nil)))
		   (done)
	    elseif (or dump-orphans delete-orphans)
	      then (dump-orphaned-series delete-orphans)
		   (done)
	    elseif dump-stats
	      then (let ((series 0)
			 (groups 0)
			 (qualities 0)
			 (episodes 0))
		     (doclass (o (find-class 'series) :db *main*)
		       (declare (ignorable o))
		       (incf series))
		     (doclass (o (find-class 'quality) :db *main*)
		       (declare (ignorable o))
		       (incf qualities))
		     (doclass (o (find-class 'episode) :db *main*)
		       (declare (ignorable o))
		       (incf episodes))
		     (format t "~
;; series: ~d
;; groups: ~d
;; qualities: ~d
;; episodes: ~d
"
			     series groups qualities episodes))
		   (done)
	    elseif dump-series
	      then (let* ((series-name (canonicalize-series-name dump-series))
			  (series (query-series-name-to-series series-name)))
		     (if* series
			then (describe series)
			else (format t "No series named ~s.~%" dump-series)))
		   (done)
	    elseif dump-episodes
	      then (dolist (ep (query-episode
				:series-name
				(canonicalize-series-name dump-episodes)))
		     (if* verbose
			then (describe ep)
			else (format t "~a~%" ep)))
		   (done)
	    elseif delete-episodes
	      then (dolist (ep (query-episode
				:series-name
				(canonicalize-series-name delete-episodes)))
		     (format t "removing ~a~%" ep)
		     (delete-instance ep))
		   (tget-commit *main*)
		   (done)
	    elseif delete-episode
	      then (delete-episode delete-episode)
		   (tget-commit *main*)
		   (done)
	    elseif delete-series
	      then (forget-series delete-series)
		   (done)
	    elseif catch-up-mode
	      then (catch-up)
		   (done)
	    elseif catch-up-series
	      then (catch-up-series catch-up-series)
		   (done)
	    elseif check-database
	      then (clean-database :check t)
		   (done)
	    elseif clean-database
	      then (clean-database)
		   (done)
	    elseif skip-next
	      then (skip-next skip-next)
		   (done)
	    elseif compact
	      then ;; already did that, just exit
		   (done)
	    elseif add-mode
	      then (manual-add add-mode force-mode)
		   (tget-commit *temp*)
		   (tget-commit *main*)
	    elseif run-mode
	      then (process-groups)
	      else (usage "no primary arguments given."))
	   
	   (done))))

    (if* *debug* ;; --debug on command line doesn't effect this test!
       then (format t ";;;NOTE: debugging mode is on~%")
	    (doit)
       else (top-level.debug:with-auto-zoom-and-exit (*standard-output*)
	      (handler-case (doit)
		(tget (c)
		  ;; 'tget errors don't get a backtrace, since those are
		  ;; expected or, at least, planned for.  The unexpected
		  ;; ones get the zoom.
		  (format t "~&~a~&" c)
		  (exit 1 :quiet t)))))))

(defun open-log-files (&key truncate)
  (when (and *log-file* (not *log-stream*))
    (with-verbosity 1
      (format t ";; Opening ~a log file...~%" *log-file*))
    (setq *log-stream*
      (open *log-file* :direction :output
	    :if-exists (if truncate :supersede :append)
	    :if-does-not-exist :create))
    (format *log-stream* "~%;; ~a~%~%" (ut-to-date-time (get-universal-time))))
	   
  (when (and *log-rss* (not *log-rss-stream*))
    (with-verbosity 1 (format t ";; Opening ~a rss log file...~%" *log-rss*))
    (setq *log-rss-stream*
      (open *log-rss* :direction :output
	    ;; rss feed is too large to append
	    :if-exists :supersede ;;(if truncate :supersede :append)
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
    (setq *lock-file* (pathname (format nil "~a.lock" *database-main-name*))))
  (handler-case
      (with-open-file (s *lock-file* :direction :output
		       :if-exists :error
		       :if-does-not-exist :create)
	(format s "Lock created on ~a~%"
		(ut-to-date-time (get-universal-time))))
    (error (c)
      (declare (ignore c))
      (.error "Could not obtain the lock file (~a)." *lock-file*))))

(defun release-database-lock-file ()
  (when (null *lock-file*)
    (.error "Tried to release lock file before it was created."))
  (ignore-errors (delete-file *lock-file*)))

(defun open-tget-database (&key compact
				(if-does-not-exist :create)
				(if-exists :open)
			   &aux ok backed-up)
  (when *main* (close-tget-database))

  (ensure-directories-exist *database-main-name*)
  
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
				(.error "Schema version file ~a is corrupt."
				       *version-file*))
		       else ;; The version prior to having versioning --
			    ;; rebuilds were necessary from version 1 to 2,
			    ;; but after that it's automatic.
			    '(:version 2 :tget-version "0.00")))
	     (schema (or (ignore-errors (apply #'make-schema stuff))
			 (.error "Schema version data is corrupt: ~s." stuff)))
	     (schema-update (< (schema-version schema) *schema-version*))
	     (program-update (string/= (schema-tget-version schema)
				       *tget-version*))
	     write-version-file)
    
	(when (probe-file *database-main-name*)
	  ;; Check for the need to backup
	  (when (and (not backed-up)
		     compact
		     (or (eq 't *auto-backup*)
			 (eq :compact *auto-backup*)))
	    (setq backed-up (backup-database "for an archive restore")))
	  (when (and (not backed-up)
		     schema-update
		     (or (eq 't *auto-backup*)
			 (eq :schema-update *auto-backup*)))
	    (setq backed-up (backup-database "for schema update")))
	  (when (and (not backed-up)
		     program-update
		     (or (eq 't *auto-backup*)
			 (eq :program-update *auto-backup*)))
	    (setq backed-up (backup-database "for program update"))
	    (setq write-version-file t))
	  (when (and (not backed-up)
		     (eq if-exists :supersede)
		     (or (eq 't *auto-backup*)
			 (eq :reset *auto-backup*)))
	    (setq backed-up (backup-database "for database reset")))
	  (when (and (not backed-up)
		     (eq :force *auto-backup*))
	    (setq backed-up (backup-database "because requested"))))

	;; Do this first, so the `default' database is *main*.  This should
	;; only matter for the test suite
	(when *debug*
	  (format t "~&;; Open temp db: ~a~%" *database-temp-name*)
	  (format t "~&;; Open main db: ~a~%" *database-main-name*))
	(setq *temp*
	  (open-file-database *database-temp-name*
			      :use :memory
			      :if-does-not-exist :create
			      :read-only nil
			      :if-exists :supersede))
	(setq *main*
	  (open-file-database *database-main-name*
			      :use :memory
			      :if-does-not-exist if-does-not-exist
			      :read-only nil
			      :if-exists if-exists))
	
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
		  (if* *debug*
		     then (db-upgrade v)
		     else (handler-case (db-upgrade v)
			    (error (c)
			      (.error
			       "Database upgrade to version ~d failed: ~a."
			       v c))))
		  ;; Update *version-file* to new version
		  (setf (file-contents *version-file*)
		    (format nil "(:version ~d :tget-version ~s)~%"
			    (1+ v)
			    *tget-version*)))
	 elseif write-version-file
	   then (setf (file-contents *version-file*)
		  (format nil "(:version ~d :tget-version ~s)~%"
			  *schema-version* *tget-version*)))
	
	(when compact
	  ;; To compact the database we close, save, restore and reopen
	  ;; it.
	  (close-database :db *main*)
	  (let ((xml-file (sys:make-temp-file-name "tgetcompact"))
		(version-file (sys:make-temp-file-name "tgetversion")))
	    (format t ";; Saving database to temp file (~a)..." xml-file)
	    (force-output t)
	    (save-database xml-file :file *database-main-name* :verbose nil
			   :db *main*)
	    (sys:copy-file *version-file* version-file)
	    (format t "done.~%")
	    ;; this already prints what it does:
	    (restore-database xml-file *database-main-name*)
	    (sys:copy-file version-file *version-file*)
	    (delete-file xml-file)
	    (delete-file version-file))
	  (when *debug*
	    (format t "~&;; Open main db: ~a~%" *database-main-name*))
	  (setq *main*
	    (open-file-database *database-main-name* :use :memory
				:read-only nil))
	  ;; Don't need *temp* in this case
	  )

	(setq ok t))
    ;; In the event of an error, make sure we release the lock:
    (when (not ok)
      (when backed-up
	(format t ";; Error during database open, copy is here: ~a.~%"
		backed-up))
      (if* *main*
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
  ;; Backup the database given by *database-main-name*.  We haven't opened it
  ;; yet, so we're safe to copy the files.
  (format t ";; Backing up database ~a.~%" reason)
  (when (not (probe-file *database-main-name*))
    (.error "Database ~a does not exist." *database-main-name*))
  (let* ((backup-directory (backup-directory *database-main-name*))
	 (from (pathname-as-directory *database-main-name*))
	 (to (pathname-as-directory backup-directory)))
    (ensure-directories-exist to)
    (dolist (file (directory from))
      (sys:copy-file file
		     (merge-pathnames (enough-pathname file from) to)
		     :preserve-time t))
    (format t ";;  Copy is in ~a.~%" to)
    ;; Like *database-main-name*, no trailing slash
    backup-directory))

(defmethod db-upgrade ((version (eql 2)))
  ;; The change from 2 to 3: added the tget-admin class.  Just need to add
  ;; an instance to the database for that.
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version)))

(defmethod db-upgrade ((version (eql 3)))
  ;; The change from 3 to 4: ...
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version)))

(defmethod db-upgrade ((version (eql 4)))
  ;; The change from 4 to 5: series slot name change, `last-episode' to
  ;; `complete-to', and it and `discontinuous-episodes' needs to be
  ;; initialized to `nil'.
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Initializing new slots of series instances.~%")
  (doclass (series (find-class 'series) :db *main*)
    ;; Weirdly, if I don't print the series objects, the slots aren't
    ;; updated!  What's up with that?!?
    (format t ";; updating ~a...~%" series)
    (setf (series-complete-to series) nil)
    (setf (series-discontinuous-episodes series) nil))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 5)))
  ;; The change from 5 to 6: series slot name change, `last-episode' to
  ;; `complete-to', and it and `discontinuous-episodes' needs to be
  ;; initialized to `nil'.
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Fix container and repack slots of episodes.~%")
  (doclass (ep (find-class 'episode) :db *main*)
    (when (not (slot-boundp ep 'container))
      (setf (episode-container ep) nil))
    (when (not (slot-boundp ep 'repack))
      (setf (episode-repack ep) nil)))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 6)))
  ;; The change from 6 to 7: added pretty-epnum, so need to initialize it
  ;; here.
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Update pretty-epnum for each episode.~%")
  (doclass (ep (find-class 'episode) :db *main*)
    (when (not (slot-boundp ep 'pretty-epnum))
      (let ((new (season-and-episode-to-pretty-epnum (episode-season ep)
						     (episode-episode ep))))
	#+ignore ;; debugging only
	(format t "pretty-epnum => ~10a (for ~s)~%"
		new (series-pretty-name (episode-series ep)))
	(setf (episode-pretty-epnum ep) new))))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 7)))
  ;; The change from 7 to 8: no change, but fix episode and pretty-epnum
  ;; for range episodes, like that for
  ;;    Mad.Men.S06E01-E02.PROPER.HDTV.x264-2HD.mp4
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Update episode/pretty-epnum for range episodes.~%")
  (doclass (ep (find-class 'episode) :db *main*)
    (when (episode-filename ep)
      (let ((new-episode (nth-value 2
				    (extract-episode-info-from-filename
				     (episode-filename ep))))
	    new-pretty-epnum)
	(when (not (equal new-episode (episode-episode ep)))
	  (format t ";; Fix episode ~a~%" ep)
	  (format t ";;   new epnum = ~a~%" new-episode)
	  (setf (episode-episode ep) new-episode)
	  (setq new-pretty-epnum
	    (season-and-episode-to-pretty-epnum (episode-season ep)
						new-episode))
	  (format t ";;   new pretty-epnum = ~a~%" new-pretty-epnum)
	  (setf (episode-pretty-epnum ep) new-pretty-epnum)
	  
	  (update-complete-to (episode-series ep)
			      (episode-season ep)
			      new-episode)
	  (format t ";;   complete-to is now ~a...~%"
		  (series-complete-to (episode-series ep)))))))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 8)))
  ;; The change from 8 to 9: added `subdir' slot to series objects
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Update series objects to include 'subdir slot...~%")
  (doclass (s (find-class 'series) :db *main*)
    ;; When we load the config file, we'll overwrite the ones that have
    ;; values.
    (when (not (slot-boundp s 'subdir))
      (setf (series-subdir s) nil)))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 9)))
  ;; The change from 9 to 10: added `date-based' slot to series objects
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))
  (format t ";;   Update series objects to include 'date-based slot...~%")
  (doclass (s (find-class 'series) :db *main*)
    ;; When we load the config file, we'll overwrite the ones that have
    ;; values.
    (when (not (slot-boundp s 'date-based))
      (setf (series-date-based s) nil))
    (when (series-date-based s)
;;;; what about discontinuous-episodes???  They can cause trouble, too.
      ;; If series is date based, then warn the user there is a
      ;; complete-to, in case it's wrong.
      (format t ";;    Warning: complete-to for date-based series:~%     ~a~~%"
	      s)))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 10)))
  ;; The change from 10 to 11: made `group-rss-url' a list
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))

  (format t ";;   OBSOLETE...~%")
  #+ignore
  (doclass (g (find-class 'group) :db *main*)
    (when (and (slot-boundp g 'rss-url)
	       (not (consp (group-rss-url g))))
      (setf (group-rss-url g)
	(list (group-rss-url g)))))
  (tget-commit *main*))

(defmethod db-upgrade ((version (eql 11)))
  ;; The change from 11 to 12:
  ;;  - added `private' to series class
  ;;  - added `tracker' to episode class
  ;;  - remove persistent class `group'
  
  (format t ";; Upgrading database from version ~d to ~d...~%"
	  version (1+ version))

  (doclass (s (find-class 'series) :db *main*)
    (when (slot-boundp s 'private)
      (setf (series-private s) nil)))
  
  (doclass (e (find-class 'episode) :db *main*)
    (when (slot-boundp e 'tracker)
      (setf (episode-tracker e) nil)))

  (doclass (g (find-class 'group) :db *main*)
    (delete-instance g))
  ;; No point to this because we have a chicken and egg situation with the
  ;; class: I need the class definition to delete the instances.
  #+ignore
  (delete-persistent-class (find-class 'group))

  (tget-commit *main*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-database (&key check)
  ;; clean database:
  ;; - all eps are not part of a known series
  (doclass (ep (find-class 'episode) :db *main*)
    (when (not (query-series-name-to-series (episode-series-name ep)))
      (if* check
	 then (format t "Orphan: ~a~%" ep)
	 else (format t "Delete orphan: ~a~%" ep)
	      (delete-instance ep)))))

(defun tget-commit (db)
  (commit :db db))

(defun close-tget-database ()
  (when *main*
    (with-verbosity 1 (format t ";; closing database...~%"))
    (commit :db *main*)
    (close-database :db *main*)
    (setq *main* nil)
    (when *temp*
      (commit :db *temp*)
      (close-database :db *temp*)
      (setq *temp* nil))
    (release-database-lock-file)))

(push '(close-tget-database) sys:*exit-cleanup-forms*)

(defun query-series-name-to-series (name &aux real-name)
  (with-verbosity 3
    (format t "query-series-name-to-series: have ~s~%" name))
  (setq real-name (gethash name *series-name-aliases*))
  (when real-name
    (with-verbosity 2
      (format t "Found alias ~s for ~s.~%" name real-name))
    (setq name real-name))
  (let ((s (retrieve-from-index 'series 'name name :db *main*)))
    (with-verbosity 3
      (format t "query-series-name-to-series: returning ~s~%" s))
    (values s name)))

(defun make-episode (&key transient
			  full-title
			  torrent-url
			  pub-date
			  type
			  length
			  series
			  series-name
			  title
			  season
			  episode
			  pretty-epnum
			  repack
			  filename
			  container
			  source
			  codec
			  resolution
			  tracker
		     &aux temp)
  ;; Only make a new episode instance if one does not already exist.
  
  (if* (setq temp
	 (query-episode :series-name series-name
			:season season
			:ep-number episode
			:container container
			:source source
			:codec codec
			:resolution resolution
			:repack repack))
     then #+ignore
	  (when (cdr temp)
	    ;; sometimes there's an Indi and a non-Indi... I'd really like
	    ;; to only download the non-Indi... so need some way to tag
	    ;; Indi's.  1/18/15: this really isn't an issue anymore.
	    (warn "more than one! ~s" temp))
	  (car temp)
     else (let ((*allegrocache* (if* transient
				   then *temp*
				   else *main*)))
	    (make-instance 'episode 
	       :transient transient
	       :full-title full-title
	       :torrent-url torrent-url
	       :pub-date pub-date
	       :type type
	       :length length
	       :series series
	       :series-name series-name
	       :title title
	       :season season
	       :episode episode
	       :pretty-epnum pretty-epnum
	       :repack repack
	       :filename filename
	       :container container
	       :source source
	       :codec codec
	       :resolution resolution
	       :tracker tracker))))

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
			   (repack nil repack-given)
			   (transient nil transient-given)
		      &aux (db
			    (if* (and transient-given transient)
			       then *temp*
			       else *main*)))
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
		     ,@(when repack-given
			 `((= repack ,repack)))
		     (= series-name ,series-name)
		     ,@(when season `((= season ,season)))
		     ,@(when ep-number `((= episode ,ep-number)))
		     ,@(when container `((= container ,container)))
		     ,@(when source `((= source ,source)))
		     ,@(when codec `((= codec ,codec)))
		     ,@(when resolution `((= resolution ,resolution))))
		   :db db))
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
      (.error "Must have an episode to query for quality."))
    (let* ((ep episode)
	   (q (if* (eq 't quality)
		 then nil
		 else (or (retrieve-from-index 'quality 'name quality
					       :db *main*)
			  (.error "No quality named ~s." quality))))
	   (cursor
	    (create-expression-cursor
	     'episode
	     `(and 
	       ,@(when transient-given
		   `((= transient ,transient)))
	       (= series-name ,(episode-series-name ep))
	       (= season ,(episode-season ep))
	       (= episode ,(episode-episode ep)))
	     :db db))
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
   (t (.error "No keywords for episode selection were given"))))

(defun query-group-to-series (group)
  ;; Return a list of all series instances that are in group GROUP-NAME.
  (retrieve-from-index 'series 'group (group-name group) :all t :db *main*))

;;; cache the lookups??? mostly will be just a few of them
(defun quality (thing)
  (if* (keywordp thing)
     then (retrieve-from-index 'quality 'name thing :db *main*)
     else ;; user-defined function or `t', just return it
	  thing))

(defun episode-quality (ep &key priority)
  ;; Given an episode, return the name of the quality
  (doclass (q (find-class 'quality) :db *main*)
    (when (quality-match-p ep q)
      (return
	(if* priority
	   then (quality-priority q)
	   else (quality-name q))))))

(defvar *now* nil) ;; used by test suite

(defun hours-available (episode)
  ;; Return the number of hours EPISODE has been available in the feed in
  ;; which it was found.
  (let ((hours (values
		(floor (- (or *now* (get-universal-time))
			  (episode-pub-date episode))
		       3600))))
    (if* (>= hours 0)
       then hours
       else (with-verbosity 2
	      (format t "~&negative hours-available ~d, now ~s, pub date ~s~%"
		      hours
		      (get-universal-time)
		      (episode-pub-date episode)))
	    0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the meat

(defvar *http-timeout*
    ;; If it won't respond in this number of seconds, give up
    120)

;; If a feed gets this many errors, then we skip it
(defvar *error-skip-count* 2)

(defvar *tracker* nil)

(defun process-groups (&aux (*http-timeout* *http-timeout*)
			    ep-printer
			    group-processed
			    temp
			    (skip-count
			     ;; (feed . count)
			     '())
			    *tracker*)
  (tget-commit *main*)
  (tget-commit *temp*)
  (dolist (group (collect-hash-values *group-name-to-group*))
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
    (setq group-processed nil)
    (setq *tracker* nil)
    (cond
     ((group-trackers group)		; THE NEW WAY
      (dolist (tracker (group-trackers group))
	(when (or (tracker-disabled tracker)
		  (null (tracker-url tracker)))
	  ;; Disabled or no URL, so ignore it.
	  (go :skip))
	(setq *tracker* tracker)
	
	(when (null (setq temp (assoc (tracker-url tracker) skip-count
				      :test #'string=)))
	  (push (setq temp (cons (tracker-url tracker) 0)) skip-count))
	(handler-case
	    (when (< (cdr temp) *error-skip-count*)
	      (prog1
		  (mapcar
		   'rss-to-episode
		   (fetch-feed (tracker-url tracker)
			       (if* (and (symbolp (tracker-debug-feed tracker))
					 (fboundp 'debug-feed))
				  then (funcall 'debug-feed
						(tracker-debug-feed tracker))
				elseif (stringp (tracker-debug-feed tracker))
				  then (tracker-debug-feed tracker))))
		(setq group-processed t)))
	  (net.rss:feed-error-ignore ()
	    (setq *http-timeout* 30)
	    (incf (cdr temp)))
	  (net.rss:feed-error (c)
	    (format t "~&~a~%" c)
	    ;; reduce *http-timeout* dramatically, since we already got an
	    ;; error
	    (setq *http-timeout* 30)
	    ;; incf skip-count
	    (incf (cdr temp))))
       :skip
	))
     (t					; LEGACY
      (dolist (rss-url (if* (consp (group-rss-url group))
			  then (group-rss-url group)
			  else (list (group-rss-url group))))
	(when (null rss-url)
	  ;; A group with a null :rss-url, which could be a :manual feed.
	  ;; Ignore it.
	  (go :skip))
	
	(when (null (setq temp (assoc rss-url skip-count :test #'string=)))
	  (push (setq temp (cons rss-url 0)) skip-count))
	(handler-case
	    (when (< (cdr temp) *error-skip-count*)
	      (prog1 (mapcar 'rss-to-episode
			     (fetch-feed rss-url
					 ;; no debug feed in this case
					 nil))
		(setq group-processed t)))
	  (net.rss:feed-error (c)
	    (format t "~&~a~%" c)
	    ;; reduce *http-timeout* dramatically, since we already got an
	    ;; error
	    (setq *http-timeout* 30)
	    ;; incf skip-count
	    (incf (cdr temp))))
       :skip
	)))

    (when (not group-processed)
      ;; None of the feeds for this group worked, so skip to next
      (go next-group))
      
    ;; Process the eps we just read from the feed.
    (tget-commit *main*)
    (tget-commit *temp*)
    
    ;; Now, the newly added objects in the database have their transient
    ;; slot set to `t'.  We can query against them, to see if we
    ;; need to download any of them, and and easily find them to remove
    ;; them when we're done.

    (process-transient-objects group ep-printer)
  
    ;; Remove the remaining transient episodes
    (dolist (ep (retrieve-from-index 'episode 'transient t :all t :db *temp*))
      (delete-instance ep))
    (tget-commit *temp*)
    (tget-commit *main*)
   next-group
    ))

(defun process-transient-objects (group ep-printer)
  ;; Now, we comb through the series for this group and look for matches
  ;; to download
  (dolist (series (query-group-to-series group))
    (with-verbosity 2 (format t "~&processing series ~a...~%" series))
    
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
  (tget-commit *temp*))

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
	temp hours tracker after-complete-to)
      ((null eps) res)
    (@log "matching: ~a" ep)
    (when (and
	   ;; Ignore whole seasons, months, specials, etc
	   (if* (keywordp (episode-episode ep))
	      then (@log "  ignore: ep: ~s" (episode-episode ep))
		   nil
	      else t)
	   
	   ;; Ignore episode 0's
	   (if* (eql 0 (episode-episode ep))
	      then (@log "  ignore: ep 0")
		   nil
	      else t)
	   
	   ;; Make sure it's not before our cutoff in the series
	   ;; complete-to
	   (if* (or (setq after-complete-to
		      (episode-after-complete-to ep series))
		    ;; unless it's a repack
		    (episode-repack ep))
	      thenret
	      else (@log "  ignore: before complete-to of ~a"
			 (pretty-season-and-episode
			  (car (series-complete-to series))
			  (cdr (series-complete-to series))))
		   nil)
	   
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
					     (episode-resolution ep))
				      (< (- (episode-pub-date ep)
					    (episode-pub-date old))
					 *repack-window-seconds*))
				 (return t)))
			     ;; Repack of an ep of the same quality as one
			     ;; in the database.  We might want it.
			     (@log "  is a repack we want")
			     t)
			    (t
			     (@log "  ignore: not our repack")
			     nil))
		      else (@log "  ignore: already have ep")
			   nil)
	    elseif (episode-repack ep)
	      then (if* after-complete-to
		      then (@log "  don't have ep, want it [REPACK]")
		      else (@log "  don't have ep, before complete-to [REPACK]")
			   nil)
	      else (@log "  don't have ep")
		   (@log "  ep quality: container=~s src=~s codec=~s res=~s"
			 (episode-container ep)
			 (episode-source ep)
			 (episode-codec ep)
			 (episode-resolution ep))
		   t)
	   
	   (progn
	     (setq hours (hours-available ep))
	     t)
	   
	   (setq tracker (episode-tracker ep))
	   ;; If there is a tracker download delay, then check that before
	   ;; checking the quality.
	   ;;
;;;;TODO: should the series delay trump this?  Should that turn into a
;;;;      "download immediately" flag???
	   (if* (or (null tracker)
		    (null (tracker-download-delay tracker)))
	      then (@log "  no tracker download delay")
	    elseif (>= hours (tracker-download-delay tracker))
	      then (@log "  ep available (~d) longer than tracker delay (~d)"
			 hours (tracker-download-delay tracker))
	      else (@log "  ep not available long enough (~d < ~d)"
			 hours (tracker-download-delay tracker))
		   nil)

	   ;; Check quality of episode.
	   ;;
	   (let* ((q (quality (episode-quality ep)))
		  (pri (when q (quality-priority q))))
	     (if* (eql -1 pri)
		then (@log "  this quality is disabled: ~s"
			   (episode-quality ep))
		     nil
	      elseif (series-quality series)
		then ;; We have a series quality override.  Only accept that.
		     (if* (eq (episode-quality ep)
			      (series-quality series))
			then (@log "  quality == series override")
			else (@log "  quality (~a) != series override (~a)"
				   (episode-quality ep)
				   (series-quality series))
			     nil)
		else (quality-acceptable-p ep quality)))
	   
	   ;; Check for a series or group delay.
	   ;;
;;;;TODO: move series delay above tracker delay??
	   (let ((delay (or (series-delay series)
			    (group-delay group))))
	     (if* (or (null delay) (= 0 delay))
		then (@log "  no delay")
	      elseif (>= hours delay)
		then (@log "  hours available (~d) >= delay (~d)" hours delay)
		else (@log "  ignore: hours available (~d) < delay (~d) "
			   hours delay)
		     nil)))
      (@log "  => matching episode")
      (push ep res))))

(defun quality-acceptable-p (episode quality
			     &aux (hours (hours-available episode)))
  ;; Return T if the quality of EPISODE is the same as that given by
  ;; QUALITY.
  ;;
  ;; This is a little more complex than just comparing the quality in the
  ;; arguments, since QUALITY can be a user-defined function which returns
  ;; a different quality based on time (since EPISODE was published).
  ;;
  (when (keywordp quality) (.error "didn't expect a keyword here: ~s" quality))
  
  (when (eq 't quality)
    (@log "  quality-acceptable-p: quality=t, returning")
    (return-from quality-acceptable-p t))

  (when (symbolp quality)
    (@log "  user-defined quality function: ~s" quality)
    (setq quality (quality (funcall quality episode)))
    (@log "    => ~s" (or (quality-name quality)
			  quality)))
  
  (@log "  quality-acceptable-p: comparing")
  (@log "    quality=~a" quality)
  (if* (quality-match-p episode quality)
     then (@log "    matches [hours avail=~s]" hours)
	  t
     else (@log "    no match [quality=~s, hours avail=~s]"
		(episode-quality episode) hours)
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
			   else (epnum< (episode-episode e1)
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
			(episode-number= last this))))
      (push this res))
    (setq last this)))

(defun episode-number= (e1 e2)
  (equal (episode-episode e1) (episode-episode e2)))

(defun epnum< (n1 n2)
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
     else (.error "internal error: epnum<: ~s ~s." n1 n2)))

(defun download-episodes (group episodes print-func)
  ;; EPISODES are transient.
  (dolist (episode episodes)
    (@log "download: ~a" episode)
    (funcall print-func episode)
    (when (torrent-handler *torrent-handler* episode group)
      ;; download worked or we're in *learn* mode.  In either case, mark
      ;; this ep as downloaded.
      (setf (episode-transient episode) nil)
      (update-complete-to (episode-series episode)
			  (episode-season episode)
			  (episode-episode episode))
      ;; copy the ep from *temp* to *main*
      (make-episode :transient nil
		    :full-title (episode-full-title episode)
		    :torrent-url (episode-torrent-url episode)
		    :pub-date (episode-pub-date episode)
		    :type (episode-type episode)
		    :length (episode-length episode)
		    :series (episode-series episode)
		    :series-name (episode-series-name episode)
		    :title (episode-title episode)
		    :season (episode-season episode)
		    :episode (episode-episode episode)
		    :pretty-epnum (episode-pretty-epnum episode)
		    :repack (episode-repack episode)
		    :filename (episode-filename episode)
		    :container (episode-container episode)
		    :source (episode-source episode)
		    :codec (episode-codec episode)
		    :resolution (episode-resolution episode))))
  (tget-commit *main*))

(defun uri-maybe-force-http (uri-or-string)
  (if* *avoid-https*
     then (net.uri:copy-uri (net.uri:parse-uri uri-or-string)
			    :scheme :http)
     else uri-or-string))

(defmethod torrent-handler ((obj transmission) episode group
			    &aux (tracker
				  (when (and episode
					     (slot-boundp episode 'tracker))
				    (episode-tracker episode))))
  (let* ((series (episode-series episode))
	 (raw-url (episode-torrent-url episode))
	 (url (if* (not *manual-add-mode*)
		 then (uri-maybe-force-http raw-url)
		 else raw-url))
	 (cmd
	  ;; -t all added because of
	  ;;    https://trac.transmissionbt.com/ticket/4409
	  ;; It's a horrible bug that's been known about for 4+ years.
	  ;; Grrrrrrrr.  -t all seems to do the trick, though.
	  (format nil "~
transmission-remote ~a:~a ~
  --auth=~a:~a ~
  -t all -a '~a' ~
  ~a ~
  -sr ~a ~@[--trash-torrent~*~] ~
  --download-dir '~a' ~
  ~@[--uplimit ~a~]"
		  (transmission-host obj)
		  (transmission-port obj)
		  (transmission-username obj)
		  (transmission-password obj)
		  url
		  (if* (transmission-add-paused obj)
		     then "--start-paused"
		     else "--no-start-paused")
		  (or (group-ratio group)
		      (and tracker (tracker-ratio tracker))
		      (transmission-ratio obj))
		  (transmission-trash-torrent-file obj)
		  (if* (series-subdir series)
		     then ;; Need to tack on the series-subdir to the end
			  ;; of the group download-path
			  (let ((dir
				 (merge-pathnames
				  (pathname-as-directory (series-subdir series))
				  (group-download-path group))))
			    (ensure-remote-directory-exists obj dir)
			    dir)
		     else (let ((dir (group-download-path group)))
			    (ensure-remote-directory-exists obj dir)
			    dir))
		  (and tracker (tracker-upload-limit tracker)))))
    (cond
     ((or *debug* *test* *learn*)
      (when *debug* (@log "cmd[not executed]: ~a" cmd))
      ;; success
      t)
     (t
      (multiple-value-bind (stdout stderr exit-status)
	  (excl.osi:command-output cmd :whole t)
	(@log "cmd: ~a" cmd)
	(@log "  exit status: ~a" exit-status)
	(if* (or (/= 0 exit-status)
		 (check-for-transmission-remote-errors stdout stderr))
	   then (@log "  stdout: ~a" stdout)
		(@log "  stderr: ~a" stderr)
		(format t "~&NOTE: error from transmission-remote:~%~a~%"
			(excl.shell:concat stdout #\Newline stderr))
		;; failure
		nil
	   else ;; success
		t))))))

(defun check-for-transmission-remote-errors (stdout stderr)
  ;; A non-nil return means there were errors.
  (if* (and stderr
	    (string/= "" stderr)
	    (=~ "No torrent specified!  Please use the -t option first"
		stderr))
     then ;; Happens when the torrent already exists.
	  ;; Return success, but print warning.
	  (format t "~&NOTE: transmission-remote did that -t thing, check dl~%")
	  nil
   elseif (or (and stderr (string/= "" stderr))
	      (=~ "(Error|error)" stdout))
     then t
     else ;; OK, I guess.  transmission-remote is a sucky program that
	  ;; sometimes doesn't finish the job, but returns a 0 exit status
	  nil))

(defmethod torrent-handler ((obj pathname) episode group)
  (declare (ignore group))
  ;; Download the torrent file from
  ;;   (episode-torrent-url episode)
  ;; and store it into pathname given by obj.
  ;; Download to a temporary file and rename it, so whatever program
  ;; watching the directory doesn't get confused by a partial torrent
  ;; file.
  (let ((url (episode-torrent-url episode))
	(temp-file (merge-pathnames
		    (system:make-temp-file-name "tortmp" obj)
		    obj))
	(pretty-name (merge-pathnames
		      (episode-to-pretty-file-name episode ".torrent")
		      obj))
	(res ;; assume success
	 t))
    (cond
     ((or *debug* *test* *learn*)
      (@log "torrent[not downloaded]: ~a" url)
      res)
     (t
      (@log "torrent[downloaded]: ~a" url)
      (handler-case (net.aserve.client:http-copy-file url temp-file)
	(error (c)
	  (@log "  http-copy-file failed")
	  (warn "failed (~a) to download torrent file: ~a" c url)
	  (when (probe-file temp-file) (delete-file temp-file))
	  ;; failure
	  (setq res nil)))
      (when res
	(handler-case (rename-file-raw temp-file pretty-name)
	  (error (c)
	    (@log "  rename failed")
	    (warn "Could not rename ~a to ~a: ~a" temp-file pretty-name c)
	    (when (probe-file temp-file) (delete-file temp-file))
	    ;; failure
	    (setq res nil))))
      (when (null res)
	(format t "~&NOTE: error downloading ep torrent, will try again~%"))
      res))))

(defmethod ensure-remote-directory-exists ((obj transmission) dir)
  (when (not *learn*)
    ;; If the command to make the directory fails, then tget will exit
;;;;TODO: can this be not remote?  Doesn't jkf use it in "local" mode?  If
;;;;      so, then this function is badly named.
    (execute-remote-command obj (format nil "mkdir -p ~a" dir))))

(defmethod execute-remote-command ((obj transmission) command-line)
  (@log "execute command on ~a (as ~a): ~a"
	(transmission-host obj)
	(transmission-ssh-user obj)
	command-line)
  (multiple-value-bind (stdout stderr exit-code)
      (cond
       ((transmission-docker-container-name obj)
	(excl.osi:command-output
	 (format nil "docker exec ~a su ~a -c '~a'"
		 (transmission-docker-container-name obj)
		 (transmission-docker-user obj)
		 command-line)))
       ((transmission-ssh-user obj)
	(ssh-command-output (transmission-ssh-user obj)
			    (transmission-ssh-identity obj)
			    (transmission-host obj)
			    command-line))
       (t (.error "Neither docker nor ssh defined for transmission torrent handler.")))
    (cond
     ((= 0 exit-code)
      (@log "  success"))
     (t
      (@log "  failed with exit status ~d" exit-code)
      (@log "  stdout: ~a" stdout)
      (@log "  stderr: ~a" stderr)
      (.error "remote command (~s) failed (~d) to host ~s: ~s, ~s"
	      command-line exit-code
	      (transmission-host obj)
	      stdout stderr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun manual-add (thing force)
  (when (not (probe-file thing))
    (usage "--add requires an existing file or directory argument"))
  (setq *manual-add-mode* t)
  (cond
   ((file-directory-p thing)
    (with-verbosity 1
      (format t ";; Manually adding files from ~a~%" thing))
    (let ((torrent-files 
	   (directory
	    (merge-pathnames
	     #p(:type "torrent")
	     (pathname-as-directory thing))))
	  (episodes '())
	  ep)
      (dolist (torrent torrent-files)
	(with-verbosity 1
	  (format t ";;   Manually adding ~a~%" (file-namestring torrent)))
	(setq ep (manual-add-file torrent force))
	(when ep (push ep episodes)))
		     
      (download-episodes (group-name-to-group :manual)
			 episodes
			 (lambda (ep) (format t "~&~a~%" ep)))
    
      ;; delete the transient episodes:
      (dolist (ep episodes) (delete-instance ep))))
   (t ;; single file
    (with-verbosity 1
      (format t ";; Manually adding ~a~%" (file-namestring thing)))
    (let ((ep (manual-add-file thing force)))
      (when ep
	(download-episodes (group-name-to-group :manual)
			   (list ep)
			   (lambda (ep) (format t "~&~a~%" ep)))
	;; delete the transient episode:
	(delete-instance ep))))))

(defun manual-add-file (torrent force)
  ;; This attempts to add TORRENT manually.
  (let (series ep)
    (multiple-value-bind (series-name season episode repack container
			  source codec resolution)
	(extract-episode-info-from-filename (file-namestring torrent)
					    :episode-required nil)
      (declare (ignore repack))
      (when (null season)
	(.error "Manual add: no season and episode information in torrent."))
      (when (null
	     (multiple-value-setq (series series-name)
	       (query-series-name-to-series series-name)))
	(with-verbosity 1
	  (format t "Manual add: unknown series: ~a~%" series-name))
	(return-from manual-add-file))
      ;; a series we care about...

      (setq ep (query-episode :series-name series-name
			      :season season
			      :ep-number (or episode *max-epnum*)))
      (when (cdr ep)
	(.error "There is more than one episode matching:~%~{  ~a~}" ep))
      (when ep
	(setq ep (car ep))
	(when (not force) (.error "episode already downloaded: ~a." ep))
	(delete-instance ep)
	(tget-commit *main*))
      
      (make-episode
       :transient t
       :full-title series-name
       :torrent-url (namestring torrent) ;; can be a URL or filename
       :pub-date nil
       :type :manual
       :length nil

       :series series
       :series-name series-name
       :title series-name
       :season season
       :episode (or episode *max-epnum*)
       :pretty-epnum (season-and-episode-to-pretty-epnum
		      season (or episode *max-epnum*))
       
       :container container
       :source source
       :codec codec
       :resolution resolution))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssh-command-output (user identity host command-line
			   &rest args &key &allow-other-keys)
  (apply
   #'excl.osi:command-output
   ;; First time sshing into the host, we'll need
   ;; StrictHostKeyChecking=no, otherwise we'll get asked if we want to
   ;; add it to the known_hosts file.
   (format nil "ssh -o StrictHostKeyChecking=no -l ~a -i ~a ~a '~a'"
	   user identity host command-line)
   args))

(defun episode-to-pretty-file-name (ep suffix)
  (format nil "~a_~a_~a~a"
	  (substitute #\_ #\space (episode-series-name ep))
	  (episode-pretty-epnum ep)
	  (pretty-episode-quality ep :separator #\.)
	  suffix))

(defun pretty-season-and-episode (season epnum)
  (format nil "S~2,'0dE~2,'0d" season epnum))

(defun catch-up ()
  ;; Our job is to mark the complete-to slot of all series objects with the
  ;; data from the episodes objects we have in the database.
  (format t ";; Catching up for all series:~%")
  (doclass (series (find-class 'series) :db *main*)
    ;; We merely have to call update-complete-to on all episodes in this
    ;; series and that function will set the complete-to and
    ;; discontinuous-episodes slots properly.
    (format t ";; ~52a: " (series-pretty-name series))
    (force-output t)
    (dolist (ep (query-episode :series-name (series-name series)
			       ;; There should be no other type, at this
			       ;; point:
			       :transient nil))
      (update-complete-to series
			  (episode-season ep)
			  (episode-episode ep)))
    (if* (series-complete-to series)
       then (format t "~a~%"
		    (pretty-season-and-episode
		     (car (series-complete-to series))
		     (cdr (series-complete-to series))))
       else (format t "--~%")))
  (tget-commit *main*))

(defun catch-up-series (what &key series)
  ;; NOTE: this is only called through direct user action, either via the
  ;;       command line or from a defseries option.
  ;;
  ;; The episode descriptor is at the end of the series.
  (multiple-value-bind (series-name season epnum ignore1 year month day)
      (parse-name-season-and-episode what :junk-allowed nil)
    (declare (ignore ignore1))
    (when (null series-name)
      (.error "Could not parse series name and episode info: ~a." what))
    
    (setq series-name (canonicalize-series-name series-name))
    (let ((series (or series
		      (query-series-name-to-series series-name)
		      (.error "Could not find series: ~s." series-name))))
      (when year
	;; date-based naming
	(setq epnum (month-day-to-ordinal year month day)))
      (setq epnum
	(if* epnum
	   thenret
	   else *max-epnum*))
      (update-complete-to series season epnum :verbose t :force t)
      ;; This needs to be reset, too:
      (setf (series-discontinuous-episodes series) nil))))

(defun delete-episode (ep-description)
  (multiple-value-bind (series-name season epnum)
      (parse-name-season-and-episode ep-description :junk-allowed nil)
    (when (or (null series-name)
	      (null season)
	      (null epnum))
      (.error "Could not parse series name and episode info: ~a."
	      ep-description))

    (setq series-name (canonicalize-series-name series-name))
    (let ((ep (or (query-episode :series-name series-name
				 :season season
				 :ep-number epnum)
		  (.error "Could not find episode: ~s." ep-description))))
      (when (cdr ep)
	(.error "There is more than one episode matching:~%~{  ~a~}" ep))
      (format t "removing episode ~a~%" (car ep))
      (delete-instance (car ep)))))

(defun skip-next (series)
  ;; NOTE: this is only called through direct user action via the
  ;;       command line.
  ;;
  ;; Skip the next episode of SERIES
  (setq series (canonicalize-series-name series))
  (let ((series (or (query-series-name-to-series series)
		    (.error "Could not find series: ~s." series)))
	complete-to)
    (when (not (setq complete-to (series-complete-to series)))
      (.error "Series does not have any episodes: ~a." series))
    (when (series-date-based series)
      (.error "Cannot skip the next episode of a date-based series: ~a."
	      series))
    (update-complete-to series
			(car complete-to)
			(1+ (cdr complete-to))
			:verbose t)))

(defun update-complete-to (series season epnum
			   &key verbose
				;; When non-nil, series-complete-to is
				;; updated even for series which are date
				;; based.
				force
			   &aux ct
				;; For a range, just select the end epnum
				(epnum (if (consp epnum) (cdr epnum) epnum))
				(new-ct (cons season epnum)))
  ;; Note that season/epnum have been seen for purposes of future
  ;; downloading.
  
  (when (null series) (.error "series is nil!"))
  (when (null season) (.error "season is nil!"))
  (when (null epnum) (.error "epnum is nil!"))
  
  (when (and (null force) (series-date-based series))
    ;; If the series is date based, like The Daily Show, and force is nil,
    ;; then return before doing anything.
    (return-from update-complete-to))
  
  ;; sanity check
  (when (and (null (series-complete-to series))
	     (series-discontinuous-episodes series))
    (.error "internal error: complete-to empty, but d-e not: ~s."
	   (series-discontinuous-episodes series)))
  
  (setq ct (series-complete-to series))
  
  (labels
      ((announce ()
	 (when verbose
	   (format
	    t "Setting series ~a complete-to to season ~d and epnum ~d~%."
	    series season epnum)))
       (contiguous-episodes (ct1 ct2)
	 ;; Return T if CT1 and CT2 represent contiguous episodes.
	 ;; Examples of contiguous episodes:
	 ;;  CT1          CT2
	 ;;  (1 . 10)     (1 . 11)
	 ;;  (1 . 10)     (2 . 1)
	 (or
	  ;; same season, next ep:
	  (and (= (car ct1) (car ct2))
	       (= (1+ (cdr ct1)) (cdr ct2)))
	  ;; next season, E01
	  (and (= (1+ (car ct1)) (car ct2))
	       (= 1 (cdr ct2)))))
       (canonicalize-discontinuous-episodes (series)
	 ;; Check for newly contiguous episodes in the head of the
	 ;; discontinuous-episodes list, OR for any elements in the d-e
	 ;; list which are older than the series-complete-to.
	 ;; See the test suite (test-tget-complete-to) for many examples,
	 ;; but here are a few to illusrate what we do:
	 ;;  IN:  c-t = (1 . 2)  d-e = ((1 . 3) (1 . 4))
	 ;;  OUT: c-t = (1 . 4)  d-e = nil
	 ;; and
	 ;;  IN:  c-t = (28 . 9999)  d-e = ((28 . 14) (28 . 16))
	 ;;  OUT: c-t = (28 . 9999)  d-e = nil
	 ;; and and very special case:
	 ;;  IN:  c-t = (28 . 9999)  d-e = ((28 . 14) (28 . 16) (29 . 1))
	 ;;  OUT: c-t = (29 . 1)     d-e = nil
	 (loop
	   (when (not (and
		       ;; make sure we didn't already exhaust it:
		       (series-discontinuous-episodes series)
		       (or
			(older-p
			 (car (series-discontinuous-episodes series))
			 (series-complete-to series))
			(contiguous-episodes
			 (series-complete-to series)
			 (car (series-discontinuous-episodes series))))))
	     (return))
	   (if* (older-p (car (series-discontinuous-episodes series))
			 (series-complete-to series))
	      then ;; Need to toss this d-e, since it's before our
		   ;; complete-to
		   (setf (series-discontinuous-episodes series)
		     (cdr (series-discontinuous-episodes series)))
	      else ;; move the first d-e to the complete-to
		   (setf (series-complete-to series) 
		     (car (series-discontinuous-episodes series)))
		   (setf (series-discontinuous-episodes series)
		     (cdr (series-discontinuous-episodes series))))))
       (sorted-insert (new-ct ct-list)
	 ;; CT is a new complete-to list we need to insert into CT-LIST,
	 ;; but keeping CT-LIST sorted.
	 (let ((res '()))
	   (dolist (ct ct-list)
	     (when (older-p new-ct ct)
	       (push new-ct res)
	       (setq new-ct nil))
	     (push ct res))
	   (when new-ct (push new-ct res))
	   (nreverse res)))

       (older-p (ct1 ct2)
	 ;; Return T if CT1 comes before CT2 in the season/epnum sequence
	 (or (< (car ct1) (car ct2))
	     (and (= (car ct1) (car ct2))
		  (< (cdr ct1) (cdr ct2))))))
    (cond
     ((null ct)
      ;; No complete-to, so just save it
      (setf (series-complete-to series) new-ct)
      (announce))
     
     ((equalp ct new-ct)
      ;; ignore, the same
      )
     
     ((older-p new-ct ct)
      (if* (null force)
	 thenret ;; ignore this, since NEW-CT is older than CT
	 else ;; from the command line, respect the user
	      (setf (series-complete-to series) new-ct)))
     
     ((null (series-discontinuous-episodes series))
      ;; No discontinuous-episodes...
      (if* (or (series-date-based series)
	       (contiguous-episodes ct new-ct))
	 then ;; ...and the epsisodes are contiguous, so save it
	      (setf (series-complete-to series) new-ct)
	 else ;; ...and the epsisodes are not contiguous, so start
	      ;; discontinuous-episodes
	      (setf (series-discontinuous-episodes series)
		(list new-ct)))
      (announce))
     
     (t
      ;; We have current discontinuous episodes.  Need to carefully manage
      ;; things.
      (if* (or (contiguous-episodes ct new-ct)
	       (older-p ct new-ct))
	 then (setf (series-complete-to series) new-ct)
	      ;; Normalize discontinuous-episodes, since there
	      ;; might be new contiguous episodes in that list now
	      (canonicalize-discontinuous-episodes series)
	      (announce)
	 else ;; Episodes are not contiguous, so merge NEW-CT into the list
	      ;; of discontiguous-episodes, but keep it sorted
	      (let ((v (sorted-insert new-ct
				      (series-discontinuous-episodes series))))
		(setf (series-discontinuous-episodes series) v)
		;; announce???
		;;   (not even sure what this comment means anymore)
		))))))

(defun episode-after-complete-to (ep series)
  (cond
   ((null (series-complete-to series)) t)
   (t
    (let ((epnum
	   (if* (consp (episode-episode ep))
	      then ;; Use the first epnum of the range, for the purposes of
		   ;; the calculations below
		   (car (episode-episode ep))
	      else (episode-episode ep))))
      (or (> (episode-season ep) (car (series-complete-to series)))
	  (and (= (episode-season ep) (car (series-complete-to series)))
	       (> epnum (cdr (series-complete-to series)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed processing

(defvar *cached-feeds*
    ;; An alist of (feed-url . rss-objects)
    nil)

(defun fetch-feed (thing debug &aux cache-key temp url)
  ;; Retrieve the rss feed from URL and return a list of net.rss:item's
  ;;
  ;; Cache the feed results.  We don't want to hit the feed 5-10 times in a
  ;; few seconds, since it's unlikely to change in that time.
  ;;
  
;;;;TODO: when debug, file-write-date the file so we can re-read it

  (if* (stringp thing)
     then (setq url thing)
   elseif (symbolp thing)
     then (setf url (funcall thing))
     else (.error "Bad url: ~s." thing))
  
  (if* (and debug *test*)
     then (setq cache-key debug)
     else (setq cache-key url))
  
  (if* (and *cached-feeds*
	    (setq temp (assoc cache-key *cached-feeds* :test #'string=)))
     then (@log "using cached feed for ~a" url)
	  (cdr temp)
     else (let ((res
		 (if* (or *test* (and *debug* debug))
		    then ;; while debugging, use a static version of the
			 ;; feed, so we don't hammer the server.
			 (when (not debug) (error "No debug feed."))
			 (format t ";;;; reading feed: ~a~%" debug)
			 (feed-to-rss-objects :file debug)
		    else (@log "read feed for ~a" url)
			 (feed-to-rss-objects :url url))))
	    (push (cons cache-key res) *cached-feeds*)
	    ;; Log here, so we don't log the cached version, too
	    (maybe-log-rss res))))

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
		  then (with-verbosity 1
			 (format t "~&;; reading feed from ~a..."
				 (net.uri:uri-host (net.uri:parse-uri url))))
		       (prog1 (net.rss:read-feed
			       (uri-maybe-force-http url)
			       :timeout *http-timeout*
			       :verbose (> *verbose* 0))
			 (with-verbosity 1
			   (format t "done.~%")))
		elseif file
		  then (net.rss::parse-feed (file-contents file))
		  else (assert content)
		       (net.rss::parse-feed content)))
	 (channel (cdr (find 'net.rss:channel (cdr lxml) :key #'car)))
	 (source (cadr (find 'net.rss:link channel :key #'car)))
	 (items (cdr (find 'net.rss:all-items channel :key #'car))))
    (multiple-value-bind (match whole source-name)
	(match-re
	 #.(concatenate 'simple-string
	     "("
	     "sporthd\\.org|"
	     "tvtorrents\\.com|"
	     "freshon\\.tv|"
	     "broadcasthe\\.net|"
	     "ezrss\\.it|"
	     "bt-chat\\.com"		;really eztv
	     ")")
	 source)
      (declare (ignore whole))
      (when (not match) (.error "don't grok the feed source: ~s." source))
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
			     net.rss:title
			     net.rss:fileName)
		       :test #'eq))
	  (when (eq 'net.rss:pubDate sym) (setq sym 'pub-date))
	  (push (intern (symbol-name sym) *kw-package*)
		constructor)
	  (push (second element) constructor))
	 ;; elements of the form of ((sym value1 value2...))
	 ;; we only care about one of them, net.rss::enclosure
	 ((and (consp element)
	       (consp (car element))
	       (symbolp (setq sym (caar element))))
	  (case sym
	    (net.rss::enclosure
	     (destructuring-bind (&key ((type type))
				       ((length length))
				       ((net.rss::url url)))
		 (cdar element)
	       (declare (ignore url))
	       (push :type constructor)
	       (push type constructor)
	       (push :length constructor)
	       (push length constructor)))
	    (net.rss::torrent
	     (dolist (torrent-element (cdr element))
	       (when (consp torrent-element)
		 (case (car torrent-element)
		   (net.rss::contentLength
		    (push :length constructor)
		    (push (second torrent-element) constructor)))))))))))
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

;; Do not remove, needed for test suite
(defmethod convert-rss-to-episode ((type (eql :tvtorrents.com)) rss)
  (let ((des (rss-item-description rss))
	series series-name season episode pretty-epnum repack container
	source codec resolution uncut)
    (multiple-value-bind (match whole des-series-name title des-season
			  des-episode filename)
	(match-re
	 #.(concatenate 'simple-string
	     "Show Name:\\s*([^;]+)\\s*;\\s*"
	     "Show Title:\\s*(.+)\\s*;\\s*"
	     "Season:\\s*([0-9]+)\\s*;\\s*"
	     "Episode:\\s*("
	     "[0-9]+|"
	     "[0-9]+-[0-9]+|"
	     "all|"
	     "complete|"
	     "special.*|"
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
	(when *debug* (@log "TVT: couldn't parse rss description: ~s." des))
	(return-from convert-rss-to-episode nil))
      
      ;; ".UNCUT." is in the "Filename" but not the "Show Name", but
      ;; "(Uncut)" is in the "Show Title"!  We need to put it into the
      ;; series-name...
      (setq uncut (match-re "uncut" title :case-fold t))
      
      (setq des-season (parse-integer des-season))
      (setq des-episode
	(if* (or (equalp "all" des-episode)
		 (equalp "complete" des-episode))
	   then ;; The above is wrong, since it matches "all" and "01.all",
		;; but it doesn't matter since I don't download seasons
		:all
	 elseif (match-re "special.*" des-episode :case-fold t)
	   then	:special
	 elseif (=~ "^(\\d\\d)\\.(0?\\d)$" des-episode)
	   then ;; e.g., 08.1 => return `8'
		(parse-integer $1)
	 elseif (=~ "^00\\.(\\d\\d)$" des-episode)
	   then ;; e.g., 00.nn => return `nn'
		;; This crazy format appeared with Gold Rush on 12/28/13
		(parse-integer $1)
	 elseif (and (= 6 (length des-episode))
		     (=~ "^(\\d\\d)\\.(?i:all)$" des-episode))
	   then ;; \\d\\d is the month -- this is a lie, but I don't care
		;; about these types of downloads:
		:complete-month
	 elseif (=~ "^(\\d{1,3})-(\\d{1,3})$" des-episode)
	   then ;; a range of episodes
		(cons (parse-integer $1) (parse-integer $2))
	 elseif (and (> des-season 1900) ;; check, to make sure
		     (= 5 (length des-episode)))
	   then (if* (=~ "^(\\d\\d)\\.(\\d\\d)$" des-episode)
		   then (month-day-to-ordinal des-season $1 $2)
		   else (.error "can't parse episode: ~s" des-episode))
	   else (parse-integer des-episode)))
      
      ;; Don't do seasons or complete months
      (when (or (eq :all des-episode)
		(eq :complete-month des-episode)
		(eq :special des-episode))
	(return-from convert-rss-to-episode))

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
	    (match-re "\\((repack|proper)\\)" (rss-item-title rss)
		      :case-fold t)))

      (multiple-value-setq (series-name season episode pretty-epnum)
	(check-episode-data des-series-name series-name
			    des-season season
			    des-episode episode))
      
      (when (null
	     (multiple-value-setq (series series-name)
	       (query-series-name-to-series series-name)))
	(return-from convert-rss-to-episode))
      ;; a series we care about...
      
      (make-episode
       :transient t
       :tracker *tracker*
       :full-title (rss-item-title rss)
       :torrent-url (rss-item-link rss)
       :pub-date (parse-rss20-date (rss-item-pub-date rss))
       :type (rss-item-type rss)
       :length (parse-integer (rss-item-length rss))

       :series series
       :series-name series-name
       :title title
       :season season
       :episode episode
       :pretty-epnum pretty-epnum
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

(defmethod convert-rss-to-episode ((type (eql :broadcasthe.net)) rss)
  (let ((rss-des (rss-item-description rss))
	(rss-title (rss-item-title rss))
	series series-name pretty-epnum repack container
	source codec resolution)
    
    ;; Don't have to worry about season packs in the BTN RSS feed since I
    ;; exclude them specifically.
    
    (when (not rss-des)
      ;; some sporting events have no description and only a title.  Ignore
      ;; these.
      (with-verbosity 5
	(format t "BTN: NULL rss-des... returning nil~%"))
      (return-from convert-rss-to-episode nil))
    
    ;; Show name must be extracted from the rss-item-title
    (multiple-value-bind (found whole show-name)
	(match-re "^(.*) - " rss-title)
      (declare (ignore whole))
      (when (not found)
	(.error "Couldn't find show name from title: ~s." rss-title))
      ;; Canonicalize the series name, so "Tosh.0" becomes "Tosh 0".
      
      (setq series-name (canonicalize-series-name show-name)))
    
    (setq repack (match-re "\\.repack\\." rss-title :case-fold t))
    
    (when (=~ #.(concatenate 'simple-string
		  " (?i:("
		  (excl:list-to-delimited-string *valid-containers* "|")
		  ")) ")
	      rss-title #|:case-fold t|# ;; doesn't work
	      )
      (setq container (intern (string-downcase $1) *kw-package*)))
    (when (=~ #.(concatenate 'simple-string
		  " (?i:("
		  (excl:list-to-delimited-string *valid-sources* "|")
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
	 #.(concatenate 'simple-string
	     "Episode Name:\\s*([^<]+)?<br />\\s*"
	     "Season:\\s*(\\d+)?<br />\\s*"
	     "Episode:\\s*(\\d+)?")
	 rss-des
	 :case-fold t
	 :multiple-lines t)
      (declare (ignore whole))
      (when (or (not found)
		(null des-title)
		(null season))
	;; If we can't parse the description, then we have nothing.
	;; Log it and move on.
	#+ignore ;; too many to log.  :(
	(@log "BTN: couldn't parse description: ~s." rss-des)
	
	;; Try the title.  Some Regular Show's don't have a description,
	;; but have all the info in the title.
	(if* (=~ "\\[\\s+([^\\]]+)\\s+\\]\\s+$" rss-title)
	   then (multiple-value-bind (tit-series-name tit-season tit-episode)
		    (extract-episode-info-from-filename $1)
		  (when (not tit-series-name)
		    (return-from convert-rss-to-episode))
		  (setq des-title tit-series-name)
		  (setq season tit-season)
		  (setq episode tit-episode))
	   else (return-from convert-rss-to-episode)))
      
      (with-verbosity 5
	(format t "BTN: rss-des=~s~%" rss-des)
	(format t "BTN: des-title=~s season=~s ep=~s~%" des-title season
		episode))
      
      (when (and (stringp season) (string/= "" season))
	(setq season (parse-integer season)))

      (when (or (null
		 (multiple-value-setq (series series-name)
		   (query-series-name-to-series series-name)))
		(not (numberp season)))
	(return-from convert-rss-to-episode))
      
      ;; a series we care about

      (setq episode (or (and episode
			     (if* (stringp episode)
				then (and (string/= "" episode)
					  (parse-integer episode))
				else episode))
			*max-epnum*))
      
      (setq pretty-epnum (season-and-episode-to-pretty-epnum season episode))

      (let (ep)
	(setq ep
	  (make-episode
	   :transient t
	   :tracker *tracker*
	   :full-title rss-title
	   :torrent-url (rss-item-link rss)
	   :pub-date (and (rss-item-pub-date rss)
			  (parse-rss20-date (rss-item-pub-date rss)))
	   ;;both `nil' for BTN!
	   :type (rss-item-type rss)
	   :length (and (rss-item-length rss)
			(parse-integer (rss-item-length rss)))

	   :series series
	   :series-name series-name
	   :title des-title
	   :season season
	   :episode episode
	   :pretty-epnum pretty-epnum
	   :repack repack
	   ;; no :filename in BTN feed!
	   :container container
	   :source source
	   :codec codec
	   :resolution resolution))
	(with-verbosity 2 (format t "BTN: consider ep: ~a~%" ep))
	ep))))

(defmethod convert-rss-to-episode ((type (eql :bt-chat.com)) rss)
  (convert-rss-to-episode :ezrss.it rss))


(defmethod convert-rss-to-episode ((type (eql :ezrss.it)) rss)
  (let ((des (rss-item-description rss))
	(filename (rss-item-fileName rss))
	series
	series-name season episode pretty-epnum repack container source
	codec resolution)
    
    (when (null filename)
      ;; nothing to go on...
      (return-from convert-rss-to-episode))
    
    ;; Don't have to worry about season packs in the EZTZ RSS feed since
    ;; there are none.
    
    (with-verbosity 4 (format t "EZTV: ~s~%" rss))
    
    (multiple-value-bind (match whole
			  des-series-name des-title
			  ignore1
			  des-season des-episode
			  des-episode-date
			  des-year des-month des-day)
	(match-re
	 #.(concatenate 'simple-string
	     "Show Name:\\s*([^;]+)\\s*;\\s*"
	     "Episode Title:\\s*([^;]+)\\s*;\\s*"
	     "("
	     "Season:\\s*([0-9]+)\\s*;\\s*"
	     "Episode:\\s*([0-9]+)"
	     "|"
	     "Episode Date:\\s*((\\d{4})-(\\d\\d)-(\\d\\d)|)"
	     ")"
	     )
	 des
	 :case-fold t)
      (declare (ignore whole ignore1))
      (when (not match) (.error "couldn't parse rss description: ~s." des))
      (cond
       (des-episode-date
	(when (string= "" des-episode-date)
	  ;; No season or episode info, so go home
	  (with-verbosity 2 (format t "EZTV: ignore1: ~a~%" series-name))
	  (return-from convert-rss-to-episode))
	(setq season (parse-integer des-year))
	(setq episode
	  (date-time-yd-day
	   (date-time (format nil "~a-~a-~a" des-year des-month des-day)))))
       (t
	(setq des-season (parse-integer des-season))
	(setq des-episode (parse-integer des-episode))))

      (setq des-series-name (canonicalize-series-name des-series-name))

      (multiple-value-setq (series-name season episode repack container
			    source codec resolution)
	(extract-episode-info-from-filename filename))

      (or repack
	  ;; See if it's a repack/proper from the title
	  (setq repack
	    (match-re " (repack|proper) " (rss-item-title rss)
		      :case-fold t)))

      (multiple-value-setq (series-name season episode pretty-epnum)
	(check-episode-data des-series-name series-name
			    des-season season
			    des-episode episode))
      
      (with-verbosity 2 (format t "EZTV: query: ~a~%" series-name))
      (when (null
	     (multiple-value-setq (series series-name)
	       (query-series-name-to-series series-name)))
	(with-verbosity 2 (format t "EZTV: ignore2: ~a~%" series-name))
	(return-from convert-rss-to-episode))
      ;; a series we care about...
      
      (let ((ep
	     (make-episode
	      :transient t
	      :tracker *tracker*
	      :full-title (rss-item-title rss)
	      :torrent-url (rss-item-link rss)
	      :pub-date (parse-rss20-date (rss-item-pub-date rss))
	      :type (rss-item-type rss)
	      :length (parse-integer (rss-item-length rss))

	      :series series
	      :series-name series-name
	      :title des-title
	      :season season
	      :episode episode
	      :pretty-epnum pretty-epnum
       
	      ;; container is always nil because the file extension is always
	      ;; .torrent
	      :container container
	      :source source
	      :codec codec
	      :resolution resolution)))
	(with-verbosity 2 (format t "EZTV: consider ep: ~a~%" ep))
	ep))))


(defmethod convert-rss-to-episode ((type (eql :freshon.tv)) rss)
  ;; Freshon (aka TvT) has a very brief RSS entry for each show.  The
  ;; `title' and `description' are all that we have, and the `description'
  ;; seems to be a superset of `title', but it doesn't offer much info that
  ;; is useful (seeders and leechers).  So, we only use the title.
  (let* ((title (rss-item-title rss))
	 (filename title)
	 series)
    
    (when (null filename)
      ;; nothing to go on...
      (return-from convert-rss-to-episode))
    
    ;; Don't have to worry about season packs in the EZTZ RSS feed since
    ;; there are none.
    
    (with-verbosity 4 (format t "TvT: ~s~%" rss))
    
    (multiple-value-bind (series-name season episode repack container
			  source codec resolution)
	(extract-episode-info-from-filename filename)

      (or repack
	  ;; See if it's a repack/proper from the title
	  (setq repack
	    (match-re " (repack|proper) " (rss-item-title rss)
		      :case-fold t)))

      (with-verbosity 3 (format t "TvT: query: ~a~%" series-name))
      (when (null (multiple-value-setq (series series-name)
		    (query-series-name-to-series series-name)))
	(with-verbosity 3 (format t "TvT: ignore2: ~a~%" series-name))
	(return-from convert-rss-to-episode))
      ;; a series we care about...
      
      (let ((ep
	     (make-episode
	      :transient t
	      :tracker *tracker*
	      :full-title (rss-item-title rss)
	      :torrent-url (rss-item-link rss)
	      :pub-date (parse-rss20-date (rss-item-pub-date rss))
	      :type (rss-item-type rss)

	      :series series
	      :series-name series-name
	      :title title
	      :season season
	      :episode episode
	      :pretty-epnum (season-and-episode-to-pretty-epnum season episode)
       
	      ;; container is always nil because the file extension is always
	      ;; .torrent
	      :container container
	      :source source
	      :codec codec
	      :resolution resolution)))
	(with-verbosity 2 (format t "TvT: consider ep: ~a~%" ep))
	ep))))

(defmethod convert-rss-to-episode ((type (eql :sporthd.org)) rss)
  ;; NBA feed, filter on "warriors"
  (let ((title (rss-item-title rss)))
    
    (when (not (match-re "warriors" title :case-fold t :return nil))
      ;; not for us...
      (return-from convert-rss-to-episode))    
    
    (with-verbosity 4 (format t "SHD: ~s~%" rss))
    
    ;; Extract info from the title, hardwire series name
    
    (multiple-value-bind (ignore1 season episode ignore2 container
			  source codec resolution)
	(extract-episode-info-from-filename title)
      (declare (ignore ignore1 ignore2))
      (let* ((series-name "NBA Warriors")
	     (series (query-series-name-to-series series-name)))
	
	(when (match-re "720p" title :case-fold t :return nil)
	  (assert (member :720p *valid-resolutions*))
	  (setq resolution :720p))
	(when (match-re "h264" title :case-fold t :return nil)
	  (assert (member :h.264 *valid-codecs*))
	  (setq codec :h.264))

	(make-episode
	 :transient t
	 :tracker *tracker*
	 :full-title (rss-item-title rss)
	 :torrent-url (rss-item-link rss)

	 :series series
	 :series-name series-name
	 :title title
	 :season season
	 :episode episode
	 :pretty-epnum (season-and-episode-to-pretty-epnum season episode)
       
	 ;; container is always nil because the file extension is always
	 ;; .torrent
	 :container container
	 :source source
	 :codec codec
	 :resolution resolution)))))

(defun check-episode-data (des-series-name series-name
			   des-season season
			   des-episode episode
			   &aux fuzzy-series-name)
  ;; Compare the series name, season and episode data obtained from
  ;; different parts of the feed.  The "des-" arguments refer to those from
  ;; the `description' element of the feed, and it is less reliable.
  ;; We return 3 values: series-name, season and episode.
  ;; For the series name, we can return a merged version of it that is
  ;; better than either of the parts.

  (when (and des-season season (/= des-season season))
    (when *debug*
      ;; mismatches too numerous for regular logging, but I want to see
      ;; them during debugging (ie, test output)
      (@log "Desc & fn season differ for ~s: ~s, ~s."
	    des-series-name des-season season)))
  
  (when (or (and des-episode
		 (numberp des-episode)
		 episode
		 (numberp episode)
		 (/= des-episode episode))
	    (and (symbolp des-episode)
		 des-episode
		 episode
		 (not (eq des-episode episode))))
    (when *debug*
      ;; mismatches too numerous for regular logging, but I want to see
      ;; them during debugging (ie, test output)
      (@log "Desc & fn episode differ for ~s: ~s, ~s."
	    des-series-name des-episode episode)))

  ;; fuzzy compare the series names, possibly creating a merged version
  ;; which is different than both, but more correct
  (when (and des-series-name series-name)
    (setq fuzzy-series-name
      (fuzzy-compare-series-names des-series-name series-name))
    (when *debug*
      (when (not fuzzy-series-name)
	(@log "Desc & fn series name differ (using 2nd): ~s, ~s."
	      des-series-name series-name))))

  (setq season (or season des-season)
	episode (or episode des-episode))
  
  (values (or fuzzy-series-name series-name des-series-name)
	  season
	  episode
	  (season-and-episode-to-pretty-epnum season episode)))
