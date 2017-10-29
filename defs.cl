
(eval-when (compile eval load)
  (require :ef-e-crcrlf)
  (require :list2)
  (require :acldns)
  (require :smtp)
  (require :osi)
  (require :shell)
  (require :datetime)
  (require :anydate)
  (require :ssl)
  (require :aserve)
  #+(version= 10 0) (require :acache "acache-3.0.5.fasl")
  #+(version= 10 1) (require :acache "acache-3.1.1.fasl")
  (require :autozoom))

(defpackage :user
  (:use #:common-lisp #:excl #:excl.osi #:excl.shell
	#:util.date-time #:net.post-office))

(in-package :user)

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

(defvar *now* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [some] user-settable variables (in config file)

(defvar *log-rss*
    ;; if non-nil, a pathanme to log rss feed info
    nil)

(defvar *log-xml*
    ;; if non-nil, a pathname to log the XML feed should there be an error
    ;; parsing it
    nil)

(defvar *log-file*
    ;; if non-nil, a pathanme to log episode info
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tracker
  name
  url
  debug-feed
  public
  download-delay
  disabled
  ratio
  upload-limit
;;;; for --cleanup
  ;; regexp to identify tracker
  re
  ;; the single char name for the tracker
  char
  ;; function to set torrent values, like ratio-limit, seed-min-time, etc
  setter)

(defvar *tracker-name-to-tracker* (make-hash-table :size 777 :test #'eq))
(push '(clrhash *tracker-name-to-tracker*)
      *init-forms*)

(defun tracker-name-to-tracker (tracker-name)
  (gethash tracker-name *tracker-name-to-tracker*))

(defun (setf tracker-name-to-tracker) (tracker tracker-name)
  (setf (gethash tracker-name *tracker-name-to-tracker*) tracker))

(defvar *all-trackers* nil)
(setq *all-trackers* nil)
(push '(setq *all-trackers* nil)
      *init-forms*)

(defmacro deftracker (name &key url debug-feed public download-delay
				disabled ratio upload-limit
				re char setter)
  `(push (.make-tracker
	  :name ,name
	  :url ,url
	  :debug-feed ,debug-feed
	  :public ,public
	  :download-delay ,download-delay
	  :disabled ,disabled
	  :ratio ,ratio
	  :upload-limit ,upload-limit
	  :re ,re
	  :char ,char
	  :setter ,setter)
	*all-trackers*))

(defmacro .make-tracker (&key name url debug-feed public download-delay
			      disabled ratio upload-limit re char setter)
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
     :upload-limit upload-limit
     :re re
     :char char
     :setter setter)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition tget (error) ())

(defun .error (format-string &rest format-arguments)
  ;; This separates known tget errors from unexpected program errors.  All
  ;; calls to error in this code should be to this function.  Any calls to
  ;; error or cerror cause a stack trace.
  (error 'tget :format-control format-string
	 :format-arguments format-arguments))

(provide :tget-defs)
