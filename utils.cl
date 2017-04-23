;; tget utility functions, macros and variables
;; None of these utilities require AllegroCache

(in-package :user)

(defvar *verbose*
    ;; Be noisy.  `nil' is used for cron mode.
    ;;  0 is not noisy.
    ;;  1 is normal noisy
    ;; >1 is debug-level noisy.
    1)

(defmacro with-verbosity (level &body forms)
  `(when (>= *verbose* ,level)
     (prog1 (progn ,@forms)
       (force-output t))))

;; User visible:
(defvar *repack-window-seconds*
    ;; download repacks within a day of the original episode
    (* 24 3600))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *max-epnum* 9999)

(defun season-and-episode-to-pretty-epnum (season epnum)
  (when (eq :all epnum) (setq epnum *max-epnum*))
  (if* (consp epnum)
     then (format nil "S~2,'0dE~2,'0d-E~2,'0d" season
		  (car epnum) (cdr epnum))
   elseif (null epnum)
     then (format nil "S~2,'0d" season)
   elseif (not (numberp epnum))
     then (format nil "~d.~a" season epnum)
   elseif (> season 999)
     then ;; 4 digit year
	  (let ((dt (date-time (format nil "~d-~3,'0d" season epnum))))
	    (format nil "~d.~2,'0d.~2,'0d"
		    season
		    (date-time-ymd-month dt)
		    (date-time-ymd-day dt)))
   elseif (= *max-epnum* epnum)
     then ;; All of that season
	  (format nil "S~2,'0d" season)
     else ;; easy, just SnnEmm
	  (format nil "S~2,'0dE~2,'0d" season epnum)))

(defun fuzzy-compare-series-names (series-name1 series-name2)
  ;; Compare SERIES-NAME1 and SERIES-NAME2, with these rules:
  ;;
  ;; - leading or trailing whitespace is not significant
  ;; - "the" at the beginning is not significant, but retain it
  ;; - a trailing question mark is not significant, but retain it
  ;; - a trailing "(word)" and "word" are the same, prefer the former
  ;; - if one ends in (word) then it is not significant, but retain it
  ;; - "and" and "&" are the same (prefer "and")
  ;; - colons are not significant (retain them)
  ;;   when a colon appears at the same position as a space, prefer the
  ;;   colon
  ;; - commas are not significant (retain them)
  ;; - if they differ, use the longer one
  ;;
  ;; don't really do this, but maybe I should:
  ;; - if one is a substring of the other, retain the longer one

  (labels
      ((decompose-series-name (name)
	 ;; return these values:
	 ;;  1. t or nil if there was a leading "the"
	 ;;  2. a list of the intermediate parts of the name
	 ;;  3. the last word of the name
	 (let ((parts (split-re "\\s+" name))
	       last2 the)
	   ;; remove leading ""
	   (when (string= "" (car parts))
	     (setq parts (cdr parts)))
	   ;; remove trailing ""
	   (setq last2 (last parts 2))
	   (when (and (second last2)
		      (string= "" (second last2)))
	     (setf (cdr last2) nil))
	   
	   (when (string= "the" (car parts))
	     (setq the t)
	     (setq parts (cdr parts)))

	   (let ((n (length parts)))
	     (if* (= 1 n)
		then (values the parts nil)
	      elseif (= 2 n)
		then (values the
			     (list (first parts))
			     (second parts))
		else (values the
			     (butlast parts)
			     (car (last parts)))))))
       (fuzzy-string= (s1 s2 &aux (char-bag '(#\: #\, #\?)))
	 ;; compare at the string level and DO comparison at
	 ;; the character level.  We return `nil' if the strings are
	 ;; not the same length.  Return the string with the better
	 ;; representation, if they are roughly similar.
	 (when (not (= (length s1) (length s2)))
	   (return-from fuzzy-string= nil))
	 (do* ((len (length s1))
	       (i 0 (1+ i))
	       best)
	     ((= i len) best)
	   (if* (char= (schar s1 i) (schar s2 i))
	      then (or best (setq best s1))
	    elseif (and (char= #\space (schar s1 i))
			(member (schar s2 i) char-bag :test #'char=))
	      then (setq best s2)
	    elseif (and (char= #\space (schar s2 i))
			(member (schar s1 i) char-bag :test #'char=))
	      then (setq best s1)
	      else (setq best nil))))
       (fuzzy-word= (s1 s2 &aux temp
				(tail-bag '(#\: #\, #\?))
				(len1 (length s1))
				(len2 (length s2)))
	 ;; compare at the word/phrase level, but don't do comparison at
	 ;; the character level
	 (if* (string= s1 s2)
	    then s1
	  elseif (or (and (string= "&" s1) (string= "and" s2))
		     (and (string= "and" s1) (string= "&" s2)))
	    then "and"
	  elseif (or (member (char (setq temp s1) (1- len1))
			     tail-bag :test #'char=)
		     (member (char (setq temp s2) (1- len2))
			     tail-bag :test #'char=))
	    then ;; the one ENDING with the [:,?] wins
		 temp
	  elseif (and (or (char= #\( (schar s1 0))
			  (char= #\( (schar s2 0)))
		      (setq temp (fuzzy2= s1 s2)))
	    then temp
	    else ;; no match
		 nil))
       (fuzzy2= (s1 s2)
	 ;; one of the strings begins with a paren, so compare the text
	 ;; inside the parens
	 (cond
	  ((and (=~ "^\\((.*)\\)$" s1) (string= $1 s2)) s1)
	  ((and (=~ "^\\((.*)\\)$" s2) (string= $1 s1)) s2))))
    (let ((new '())
	  the1 middle1 last1 the2 middle2 last2
	  m1 m2 e1 e2 temp)
      (when (setq temp (fuzzy-string= series-name1 series-name2))
	(return-from fuzzy-compare-series-names temp))
      (multiple-value-setq (the1 middle1 last1)
	(decompose-series-name series-name1))
      (multiple-value-setq (the2 middle2 last2)
	(decompose-series-name series-name2))
      (when (or the1 the2) (push "the" new))
      ;; we need to compare the elements of m1 and m2 and see how
      ;; similar they are, and if equal enough, then build a new, merged
      ;; version of them in new
      (setq m1 middle1 m2 middle2)
      (loop
	;; if we run out of one or the other, then we're not done, just
	;; exit the loop
	(when (or (null m1) (null m2)) (return))
	(setq e1 (car m1)
	      e2 (car m2))
	(if* (setq temp (fuzzy-word= e1 e2))
	   then (push temp new)
	   else (return))
	(setq m1 (cdr m1)
	      m2 (cdr m2)))
      (cond
       ((and (eq m1 middle1) (eq m2 middle2))
	;; nothing matched, so it's possible there would be a subset match
	;; take the longer one
	(let ((l1 (length m1))
	      (l2 (length m2)))
	  (if* (> l1 l2)
	     then (dolist (n m1) (push n new))
		  (push last1 new)
	     else (dolist (n m2) (push n new))
		  (push last2 new))))
       ((and m1 (null m2))
	;; m2 ran out of stuff to compare, copy the remaining m1 items to `new'
	(dolist (n m1) (push n new))
	(push last1 new))
       ((and (null m1) m2)
	;; m1 ran out of stuff to compare, copy the remaining m2 items to `new'
	(dolist (n m2) (push n new))
	(push last2 new))
       ((and (null m1) (null m2))
	;; both matched all the way, now deal with the last
	(cond
	 ((and last1 last2)
	  (let ((winner (fuzzy-word= last1 last2)))
	    (if* winner
	       then (push winner new)
	     elseif (or (string= "australia" last1)
			(string= "australia" last2))
	       then ;; a common mismatch on the last word
		    (push "(au)" new)
	       else ;; punt
		    (setq new nil))))
	 (last1 (push last1 new))
	 (last2 (push last2 new))))
       (t
	;; m1 and m2 sitting at mismatch
	;; punt
	(setq new nil)))
      
      (when new
	(list-to-delimited-string (nreverse new) #\space)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing torrent file names to extract qualities of the torrent, like
;; series name, season, episode and various indicators of quality.

(defparameter *valid-containers* '(:avi :mkv :vob :mpeg :mp4 :iso :wmv :ts
				   :m4v :m2ts))
(defparameter *valid-sources* '(:pdtv :hdtv :dsr :dvdrip :tvrip :vhsrip
				:bluray :bdrip :brrip :dvd5 :dvd9 :hddvd
				:web-dl))
(defparameter *valid-codecs* '(:x264 :h.264 :xvid :mpeg2 :divx :dvdr :vc-1
			       :wmv :bd))
(defparameter *valid-resolutions* '(:sd :720p :1080p :1080i))

(defun extract-episode-info-from-filename (filename
					   &key (episode-required t))
  ;; Extract season, episode, source, codec and resolution from the
  ;; filename and return multiple values.  Return `nil' for any that we
  ;; cannot determine.  In the case of `source', return `:unknown' for XviD
  ;; when we cannot determine the source.
  (let (series-name season episode container source codec resolution
	uncut)
    (declare (ignore-if-unused whole))

    (with-verbosity 4
      (format t "extract-episode-info-from-filename ~a...~%" filename))
    
;;;; series-name, season, episode
    ;; Start with the series-name, since if we can't get that then we go
    ;; home and the other things don't matter.
    (multiple-value-setq (series-name season episode)
      (parse-name-season-and-episode filename
				     :episode-required episode-required
				     ;; other stuff after the epnum is OK
				     :junk-allowed t))
    (when (null series-name)
      (when (null episode-required)
	;; See if this is a season pack, downloaded manually
	(multiple-value-bind (match whole series-name season-string)
	    (match-re "^(.*)\\s+-\\s+Season\\s+(\\d+)" filename :case-fold t)
	  (declare (ignore whole))
	  (when match
	    (return-from extract-episode-info-from-filename
	      (values series-name (parse-integer season-string))))))
      
      ;; we tried... let the info be collected in some other way 
      (with-verbosity 4
	(format t "  couldn't parse season and ep, aborting~%"))
      (return-from extract-episode-info-from-filename nil))

    (setq uncut (match-re "\\.uncut\\." filename :case-fold t))

    ;; filename-specific series-name canonicalization
    ;;
    (multiple-value-bind (match whole sname)
	(match-re "(.*)\\.$" series-name)
      (declare (ignore whole))
      (when match (setq series-name sname)))
    ;; If the series-name ends in `US' or `YYYY', append
    ;; " (US)" or " (YYYY)" to the series-name.
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
		(concatenate 'simple-string series-name " (uncut)"))))

    (setq series-name (canonicalize-series-name series-name))

;;;; container
    (when (=~ ".*\\.([A-Za-z0-9]+)$" filename)
      (setq container (intern (string-downcase $1)
			      (load-time-value (find-package :keyword))))
      (when (eq :torrent container)
	(setq container nil))
      (when (and container
		 (not (member container *valid-containers* :test #'eq)))
	(with-verbosity 4
	  (format t "  ignore invalid container for ~s: ~a~%"
		  filename container)
	  (@log "ignoring invalid container for ~s: ~s" filename container))
	(setq container nil)))

;;;; source
    (setq source
      (or (and (match-re "(x264|hdtv|720p)" filename :case-fold t)
	       :hdtv)
	  (and (match-re "xvid" filename :case-fold t)
	       :unknown)))

;;;; codec
    (setq codec
      (or (and (match-re "(divx|xvid)" filename :case-fold t)
	       ;; always use :xvid... :divx is little used and no reason to
	       ;; make config files deal with it.
	       :xvid)
	  (and (match-re "(x264|720p.*mkv|\\.mp4|hdtv.*indi)" filename
			 :case-fold t)
	       :x264)))
    
;;;; resolution
    (setq resolution
      ;; Crude, but the best we can do, it seems
      (if* (match-re "\\.720p\\." filename :case-fold t)
	 then :720p
       elseif (match-re "\\.1080p\\." filename :case-fold t)
	 then :1080p
       elseif (match-re "\\.1080i\\." filename :case-fold t)
	 then :1080i
	 else :sd))

    (with-verbosity 4
      (format t "  success: ~a ~a ~a~%" series-name season episode))
    
    (values series-name season episode
	    (match-re "\\.(repack|proper)\\." filename :case-fold t)
	    container source codec resolution)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for debugging episode parsing
;;(eval-when (compile eval) (pushnew :debug-episode-parser *features*))

(eval-when (compile eval)
(defun alt0-name-season-ep-re (episode-required junk-allowed)
  ;; examples:
  ;;   downton.abbey.S05E03.hdtv_x264-fov.mp4
  ;;   downton.abbey.S05E03.E04.hdtv_x264-fov.mp4
  ;;   downton.abbey.S05E03E04.hdtv_x264-fov.mp4
  ;; \1 is name
  ;; \3 is season
  ;; \5 and \6 are the range (start and end)
  ;;    OR
  ;; \7 is episode
  (concatenate 'simple-string
    "^(.*)(\\s|\\.+)"			; name
    "s([0-9]{2,3})"			; season
    "("
    "e([0-9]{2,3}).?e([0-9]{2,3})"	; episode range
    "|"
    "e([0-9]{2,3})"			; episode
    ")"
    (if episode-required "" "?")
    (junk-re junk-allowed)))

(defun alt6-name+year-season-pack-re (junk-allowed)
  ;; example: The.Player.2015.S01.HDTV.x264-TvT.torrent
  ;;
  ;; This needs to be after alt0-re and before alt2-re.
  ;; alt0-re will match SxxExx and alt2-re is the one that parses the above
  ;; example as S20E15.  So, get in between them to do the right thing.
  ;;
  ;; \1 is name
  ;; \3 is season
  (concatenate 'simple-string
    "^(.*\\d{4})(\\s|\\.+)"		; name
    "s([0-9]{2,3})"			; season
    (junk-re junk-allowed)))

(defun alt1-name-season-ep-re (junk-allowed)
  ;; example: downton.abbey.5x03.hdtv_x264-fov.mp4
  ;; \1 is name
  ;; \3 is season
  ;; \4 is episode
  (concatenate 'simple-string
    "^(.*)(\\s|\\.+)"
    "([0-9]{1,2})x([0-9]{2,3})"		; season & episode
    (junk-re junk-allowed)))

;; Bogus format!
(defun alt2-name-season-ep-re (junk-allowed)
  ;; example: downton.abbey.503.hdtv_x264-fov.mp4
  ;; \1 is name
  ;; \3 is season
  ;; \4 is episode
  (concatenate 'simple-string
    "^(.*?)(\\s|\\.+)"
    "([0-9]{1,2})([0-9]{2})"		; season & episode
    (junk-re junk-allowed)))

(defun alt3-name-re (episode-required junk-allowed)
  ;; example: UFC.187.720p.mp4
  ;; \1 is name
  (assert (null episode-required))
  (assert junk-allowed)
  (concatenate 'simple-string
    "^(.*)(\\s|\\.+)"
    (junk-re junk-allowed)))

;; Completely bogus format from BTN:
(defun alt4-name-year-season-ep-re (episode-required junk-allowed)
  ;; example: Frontline.US.S2015E02.Putins.Way.720p.HDTV.x264-TOPKEK.mkv
  ;; \1 is name
  ;; \2 is year/season
  ;; \3 is episode
  (concatenate 'simple-string
    "^(.*)(?:\\s|\\.+)"
    "s(20[0-9]{2})"			; season
    "(?:e([0-9]{2,3}))"			; episode
    (if episode-required "" "?")
    (junk-re junk-allowed)))

;; Completely bogus format from BTN:
(defun alt5-season-abbrev-multi-ep-re (junk-allowed)
  ;; example: parks.and.recreation.70304.hdtv-lol.mp4
  ;; \1 is name
  ;; \2 is year
  ;; \3 is episode start
  ;; \4 is episode end
  (concatenate 'simple-string
    "^(.*)(?:\\s|\\.+)"
    "([0-9])([0-9]{2})([0-9]{2})"	; season & episodes
    (junk-re junk-allowed)))

(defun date1-re (junk-allowed)
  ;; example: The.Daily.Show.2014x10.13.hdtv.x264-fov.mp4
  ;;          The.Daily.Show.2014.10.13.hdtv.x264-fov.mp4
  ;; \1 is name
  ;; \3 is year
  ;; \4 is month
  ;; \5 is day
  (concatenate 'simple-string
    "^(.*)(\\s|\\.+)"
    "(\\d\\d\\d\\d)[.x](\\d\\d)\\.(\\d\\d|all)"
    (junk-re junk-allowed)))
(defun date2-re (junk-allowed)
  ;; example: The.Daily.Show.141013.hdtv.x264-fov.mp4
  ;; \1 is name
  ;; \3 is year
  ;; \4 is month
  ;; \5 is day
  (concatenate 'simple-string
    "^(.*)(\\s|\\.+)(\\d\\d)(\\d\\d)(\\d\\d)"
    (junk-re junk-allowed)))

(defun date3-re (junk-allowed)
  ;; example: UFC.112.Jan.18th.2015.HDTV.x264-Sir.Paul.mp4
  ;; \1 is name
  ;; \2 is month
  ;; \3 is day
  ;; \4 is year
  (concatenate 'simple-string
    "^(.*)(?:\\s|\\.+)"
    "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)\\."
    "(?:(\\d{1,2})(?:th|nd|st|rd))\\."
    "(\\d{4})"
    (junk-re junk-allowed)))

(defun junk-re (junk-allowed)
  (if* junk-allowed
     then ;; make sure the resolution isn't mistaken for an episode
	  #+ignore "(?:.*720p)?"
	  ""
     else "$"))
)

(defparameter *alt0-name-season-ep-re-t-t*
    (compile-re #.(alt0-name-season-ep-re t t) :case-fold t))
(defparameter *alt0-name-season-ep-re-t-nil*
    (compile-re #.(alt0-name-season-ep-re t nil) :case-fold t))
(defparameter *alt0-name-season-ep-re-nil-t*
    (compile-re #.(alt0-name-season-ep-re nil t) :case-fold t))
(defparameter *alt0-name-season-ep-re-nil-nil*
    (compile-re #.(alt0-name-season-ep-re nil nil) :case-fold t))

(defparameter *alt6-name+year-season-pack-re-t*
    (compile-re #.(alt6-name+year-season-pack-re t) :case-fold t))
(defparameter *alt6-name+year-season-pack-re-nil*
    (compile-re #.(alt6-name+year-season-pack-re nil) :case-fold t))

(defparameter *alt1-name-season-ep-re-t*
    (compile-re #.(alt1-name-season-ep-re t) :case-fold t))
(defparameter *alt1-name-season-ep-re-nil*
    (compile-re #.(alt1-name-season-ep-re nil) :case-fold t))

(defparameter *alt2-name-season-ep-re-t*
    (compile-re #.(alt2-name-season-ep-re t) :case-fold t))
(defparameter *alt2-name-season-ep-re-nil*
    (compile-re #.(alt2-name-season-ep-re nil) :case-fold t))

(defparameter *alt3-name-re*
    (compile-re #.(alt3-name-re nil t) :case-fold t))

(defparameter *alt4-name-year-season-ep-re-t-t*
    (compile-re #.(alt4-name-year-season-ep-re t t) :case-fold t))
(defparameter *alt4-name-year-season-ep-re-t-nil*
    (compile-re #.(alt4-name-year-season-ep-re t nil) :case-fold t))
(defparameter *alt4-name-year-season-ep-re-nil-t*
    (compile-re #.(alt4-name-year-season-ep-re nil t) :case-fold t))
(defparameter *alt4-name-year-season-ep-re-nil-nil*
    (compile-re #.(alt4-name-year-season-ep-re nil nil) :case-fold t))

(defparameter *alt5-season-abbrev-multi-ep-re-t*
    (compile-re #.(alt5-season-abbrev-multi-ep-re t) :case-fold t))
(defparameter *alt5-season-abbrev-multi-ep-re-nil*
    (compile-re #.(alt5-season-abbrev-multi-ep-re nil) :case-fold t))

(defparameter *date1-re-t* (compile-re #.(date1-re t) :case-fold t))
(defparameter *date1-re-nil* (compile-re #.(date1-re nil) :case-fold t))

(defparameter *date2-re-t* (compile-re #.(date2-re t) :case-fold t))
(defparameter *date2-re-nil* (compile-re #.(date2-re nil) :case-fold t))

(defparameter *date3-re-t* (compile-re #.(date3-re t) :case-fold t))
(defparameter *date3-re-nil* (compile-re #.(date3-re nil) :case-fold t))

(defun parse-name-season-and-episode (thing &key episode-required
						 (junk-allowed t)
				      &aux temp
					   (this-year
					    (nth-value
					     5
					     (decode-universal-time
					      (get-universal-time)))))
  ;; Parse THING to extract and return values for SERIES-NAME, SEASON and
  ;; EPISODE.  A fourth value indicates whether Plex Media Server (PMS)
  ;; will fail to see the file, if THING is interpreted as a filename.
  ;;
  ;; EPISODE-REQUIRED -- return nil if season&episode is required and not
  ;;    found
  ;; IGNORE-DATES -- ignore certain types of dates, like:
  ;;      Jan.18th.2015  (current year)
  ;;      2015           (current year)
  ;;    as these are usually sporting event names
  ;; JUNK-ALLOWED -- stuff is allowed after the season/episode # and is
  ;;    typically a filename
  #+debug-episode-parser
  (format t "parse-name-season-and-episode: thing=~s~%" thing)
  
  ;; The presence of the resolution 720p is very tricky to deal with, so if
  ;; junk-allowed is non-nil, we just remove "720.*" from the string.
  (when junk-allowed
    (multiple-value-bind (match whole new-thing)
	(match-re "(.*)720p.*" thing :case-fold t)
      (declare (ignore whole))
      (when match
	(setq thing new-thing)
	#+debug-episode-parser (format t "  new thing=~s~%" thing))))
  
  (let* ((pms-fail nil)
	 (alt0-re
	  (if* episode-required
	     then (if* junk-allowed
		     then *alt0-name-season-ep-re-t-t*
		     else *alt0-name-season-ep-re-t-nil*)
	     else (if* junk-allowed
		     then *alt0-name-season-ep-re-nil-t*
		     else *alt0-name-season-ep-re-nil-nil*)))
	 (alt6-re (if* junk-allowed
		      then *alt6-name+year-season-pack-re-t*
		      else *alt6-name+year-season-pack-re-nil*))
	 (alt1-re (if* junk-allowed
		     then *alt1-name-season-ep-re-t*
		     else *alt1-name-season-ep-re-nil*))
	 (alt2-re (if* junk-allowed
		     then *alt2-name-season-ep-re-t*
		     else *alt2-name-season-ep-re-nil*))
	 (alt3-re (when (and (not episode-required) junk-allowed)
		    *alt3-name-re*))
	 (alt4-re
	  (if* episode-required
	     then (if* junk-allowed
		     then *alt4-name-year-season-ep-re-t-t*
		     else *alt4-name-year-season-ep-re-t-nil*)
	     else (if* junk-allowed
		     then *alt4-name-year-season-ep-re-nil-t*
		     else *alt4-name-year-season-ep-re-nil-nil*)))	 
	 (alt5-re (if* junk-allowed
		     then *alt5-season-abbrev-multi-ep-re-t*
		     else *alt5-season-abbrev-multi-ep-re-nil*))
	 (date1-re (if* junk-allowed
		      then *date1-re-t*
		      else *date1-re-nil*))
	 (date2-re (if* junk-allowed
		      then *date2-re-t*
		      else *date2-re-nil*))
	 (date3-re (if* junk-allowed
		      then *date3-re-t*
		      else *date3-re-nil*))
	 match whole ignore1 ignore2 series-name season episode epnum 
	 epnum-start epnum-end year month day)
    (declare (ignore-if-unused whole ignore1 ignore2))
    (cond
;;;;alt4
     ;; alt4 needs to go before alt0 because when :episode-required is nil
     ;; and an episode is given in format alt4, alt0 will match.
     ((multiple-value-setq (match whole series-name season epnum)
	(match-re alt4-re thing :case-fold t))
      #+debug-episode-parser (format t "  MATCH alt4-re~%")
      (values series-name (parse-integer season) (parse-integer epnum)))

;;;;alt0
     ;; alt0 must be before date2 because of 
     ;;   "The.100000.Dollar.Pyramid.2016.S01E02.720p.HDTV.x264-W4F"
     ;; the name has a bogus date-based season#/ep# in it.
     ((multiple-value-setq (match whole #|1:|# series-name ignore1
			    #|3:|# season ignore2
			    #|5:|# epnum-start #|6:|# epnum-end
			    #|7:|# epnum)
	(match-re alt0-re thing :case-fold t))
      #+debug-episode-parser (format t "  MATCH alt0-re~%")
      (setq season (parse-integer season))
      (if* (and epnum-start epnum-end)
	 then (setq episode (cons (parse-integer epnum-start)
				  (parse-integer epnum-end)))
       elseif epnum
	 then (setq episode (parse-integer epnum))
       elseif (null episode-required)
	 thenret
	 else (error "Should not get here"))
      (values series-name season episode))
     
;;;;date2
     ;; Must be before alt5
     ((and (multiple-value-setq (match whole series-name ignore1 year
				 month day)
	     (match-re date2-re thing :case-fold t))
	   (setq year (+ 2000 (parse-integer year)))
	   ;; Make sure this bogus format doesn't result in episodes from
	   ;; the future
	   (<= year this-year))
      match ;; squash bogus warning
      #+debug-episode-parser (format t "  MATCH date2-re~%")
      (setq season year)
      (setq episode
	(if* (equalp "all" day)
	   then :all
	   else ;; use the ordinal day of the year
		(month-day-to-ordinal year month day)))
      (values series-name season episode t (format nil "~d" year) month day))

;;;;alt5
     ((multiple-value-setq (match whole series-name season epnum-start
			    epnum-end)
	(match-re alt5-re thing :case-fold t))
      #+debug-episode-parser (format t "  MATCH alt5-re~%")
      (values series-name (parse-integer season)
	      (cons (parse-integer epnum-start) (parse-integer epnum-end))
	      ;; Definitely PMS fail:
	      t))
     
;;;;alt6
     ;; alt6 needs to come after alt0 because the "name that includes the
     ;; year" + season pack will cause the shebang to be parsed as as
     ;; season and episode, rather than a season pack.
     ((multiple-value-setq (match whole series-name ignore1 season)
	(match-re alt6-re thing :case-fold t))
      #+debug-episode-parser
      (format t "  MATCH alt6-re: ~s ~s~%" series-name season)
      (values series-name (parse-integer season)))

;;;;date1
     ;; Do date1 and date2 before alt1 and alt2 because the
     ;; latter will give false positives for date-based episode naming.
     ((multiple-value-setq (match whole series-name ignore1 year
			    month day)
	(match-re date1-re thing :case-fold t))
      match ;; squash bogus warning
      #+debug-episode-parser (format t "  MATCH date1-re~%")
      (setq season (parse-integer year))
      (setq episode
	(if* (equalp "all" day)
	   then :all
	   else ;; use the ordinal day of the year
		(month-day-to-ordinal year month day)))
      (values series-name season episode pms-fail year month day))

;;;;date3
     ((and
       (multiple-value-setq (match whole series-name month day year)
	 (match-re date3-re thing :case-fold t))
       (setq temp (compute-month month 0)))
      #+debug-episode-parser (format t "  MATCH date3-re~%")
      (setq season (parse-integer year))
      (setq month temp)
      (setq episode (month-day-to-ordinal year month day))
      (values series-name season episode
	      ;; technically, PMS would fail on this, but the items that
	      ;; use it are not episode based
	      nil
	      year
	      (format nil "~2,'0d" month)
	      day))

;;;;alt1
     ((multiple-value-setq (match whole series-name ignore1 season episode)
	(match-re alt1-re thing :case-fold t))
      #+debug-episode-parser (format t "  MATCH alt1-re~%")
      (setq season (parse-integer season))
      (setq episode (parse-integer episode))
      (values series-name season episode t))

;;;;alt2
     ((multiple-value-setq (match whole series-name ignore1 season episode)
	(match-re alt2-re thing :case-fold t))
      #+debug-episode-parser (format t "  MATCH alt2-re~%")
      (setq season (parse-integer season))
      (setq episode (parse-integer episode))
      (values series-name season episode t))

;;;;alt3
     ((and alt3-re
	   (multiple-value-setq (match whole series-name)
	     (match-re alt3-re thing :case-fold t)))
      #+debug-episode-parser (format t "  MATCH alt3-re~%")
      ;; technically, PMS could fail on this, but the items that
      ;; use it are not episode based
      (values series-name))
     )
;;;; values are returned from the cond above, do NOT insert more forms
;;;; here...
    ))

(defun canonicalize-series-name (name)
  ;; Canonicalize the series name
  ;;
  ;; downcase:
  (setq name (string-downcase name))
  ;; ...so "Tosh.0" becomes "Tosh 0"
  ;; ...and "Downton_Abbey" becomes "Downton Abbey"
  (setq name (replace-re name "[\\._]" " "))
  ;; ...so "James May's Man Lab" becomes "James Mays Man Lab"
  (replace-re name "[']" ""))

(defun episode-is-date-based-p (season episode)
  ;; Return T if SEASON and EPISODE are date based.  This is so we can
  ;; reject episodes from BS trackers that allow mixed mode filenames, like
  ;; "...S15E04..." and "...2017.04.21...".  I'm looking at you Freshon!
  (declare (ignore episode))
  (let ((this-year (nth-value 5 (decode-universal-time (get-universal-time)))))
    ;; If the season is in the range of current-year -20 to current-year,
    ;; it is.
    (when (<= (- this-year 20) season this-year)
      t)))

(defun month-day-to-ordinal (year month day)
  ;; Args are all strings representing the given quantities.
  (when (string= "00" day)
    ;; fucking dirty data
    (setq day "01"))
  (let ((year (if (stringp year) (parse-integer year) year))
	(month (if (stringp month) (parse-integer month) month))
	(day (if (stringp day) (parse-integer day) day)))
    ;; Sometimes it's MM.DD and sometimes it's
    ;; DD.MM.  The latter is rare, but does occur.
    ;; Only thing we can do is look for a month > 12
    ;; and reverse them.  <sigh>
    (if* (and (> month 12)
	      (> day 12))
       then (setq month 12)
     elseif (> month 12)
       then (let ((temp month))
	      (setq month day
		    day temp)))
    (date-time-yd-day
     (date-time (format nil "~a-~2,'0d-~2,'0d"
			year month day)))))

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
    ;;  	Sun, 06 Nov 1994 08:49:37 -0700
    (multiple-value-bind (matched whole day month year hour minute second tz
			  tz-sign tz-hour)
	(match-re 
	 #.(concatenate 'simple-string
	     "[A-Za-z]+, "
	     "([0-9]+) ([A-Za-z]+) ([0-9]+) "
	     "([0-9]+):([0-9]+):([0-9]+) "
	     "(GMT|([-+])(\\d\\d)\\d\\d)")
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
	      else (* (if (char= #\- (schar date (car tz-sign))) -1 1)
		      (cvt date tz-hour)))))))
    
    (error "couldn't parse date: ~s." date)))

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

(defun collect-hash-values (hash-table)
  (let ((vals '()))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (push v vals))
	     hash-table)
    vals))

(defvar *log-stream* nil)
(defvar *log-prefix* nil)

(defun @log (format-string &rest args)
  (cond
   (*log-stream*
    (when (null *log-prefix*)
      (setq *log-prefix* (format nil "~a: " (excl.osi:getpid))))
    (princ *log-prefix* *log-stream*)
    (apply #'format *log-stream* format-string args)
    (fresh-line *log-stream*)
    (force-output *log-stream*))
   ((> *verbose* 0)
    (apply #'format t format-string args)
    (fresh-line t)))

  (when (> *verbose* 1) ;; also print to t
    (apply #'format t format-string args)
    (fresh-line t))
  
  ;; Return t so this function can be used in logic chains.
  t)

(provide :tget-utils)
