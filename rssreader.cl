;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;; read rss feed
;; from jkf on 5/26/2005

(eval-when (compile load eval)
  (require :aserve)
  (require :pxml-sax))


(defpackage :net.rss.rdf 
  (:export #:about
	   #:RDF
	   #:Seq))

(defpackage :net.rss.dc
  (:export #:date))

(defpackage :net.rss.sy
  (:export #:sy))

(defpackage :net.rss.admin
  )



(defpackage :net.rss (:use :common-lisp :excl :net.xml.parser
			   :net.aserve
			   :net.aserve.client
			   )
	    (:export #:feed-rss-p
		     #:feed-slot-value
		     #:read-feed
		     
		     ; important tags
		     #:all-items
		     #:channel
		     #:description
		     #:item
		     #:link
		     #:pubDate
		     #:rss
		     #:title
		     #:version

		     #:feed-error
		     
		     ;; for EZTV:
		     #:torrent
		     #:fileName
		     
		     #:*uri-to-package*
		     ))


(in-package :net.rss)



(defparameter *uri-to-package*
    (list 
     
     (cons "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	   :net.rss.rdf)
     
     (cons "http://purl.org/dc/elements/1.1/"
	   :net.rss.dc)
     
     (cons "http://purl.org/rss/1.0/modules/syndication/"
	   :net.rss.sy)
     
     (cons  "http://webns.net/mvcb/"
	   :net.rss.admin)
     
     (cons  "http://purl.org/rss/1.0/"
	    :net.rss)
     
     ;; For EZTV
     (cons "http://xmlns.ezrss.it/0.1/"
	   :net.rss)
     ))
  
  


(define-condition feed-error (error)
  ((http-code :initarg :http-code :reader feed-error-httpcode
	      :initform nil)))

(defun read-feed (url &key timeout verbose)
  ;;
  ;;* exported
  ;;
  ;; read the feed given by the url and return a feed value
  ;;
  (multiple-value-bind (content code headers)
      (handler-case (do-http-request url :timeout timeout)
	(socket-error (c)
	  (error 'feed-error
		 :format-control "Socket error from do-http-request: ~a"
		 :format-arguments (list c)))
	(error (c)
	  (error 'feed-error
		 :format-control "Error from do-http-request: ~a"
		 :format-arguments (list c))))
    (declare (ignore headers))
    
    (if* (not (eq 200 code))
       then (if* verbose
	       then (error
		     'feed-error
		     :http-code code
		     :format-control "Accessing URL ~s gave response ~s"
		     :format-arguments (list url code))
	       else (error
		     'feed-error
		     :http-code code
		     :format-control "Accessing feed from ~s gave response ~s"
		     :format-arguments (list (net.uri:uri-host
					      (net.uri:parse-uri url))
					     code))))
    
    (parse-feed content)))


(defun parse-feed (content)
  (setq content
    ;; WTF?  Control chars in feeds?  Cripes, what'll they think of next?
    (remove-if (lambda (c) (<= (char-code c) #.(char-code #\^z)))
	       content))
  ;;
  ;;* exported
  ;;
  ;; read the feed given by the url and return a feed value
  ;;
  (let ((body (car 
	       (let ((*package* (find-package :net.rss)))
		 ;; parse-xml was crapping out on EZTV, so I switched to
		 ;; parse-to-lxml on the advice of mm.  When I say
		 ;; "crapping out" I mean crashing the lisp.
		 (net.xml.sax:parse-to-lxml
		  content
		  ;; needed for parse-to-lxml not parse-xml
		  :package *package* 
		  ;;The eztv.it feed requires this
		  :external nil
		  :uri-to-package *uri-to-package*)))))
    ;;(pprint body)
    ;;(setq *body* body)
    (if* (and (consp (car body))
	      (eq 'rss (caar body)))
       then `(rss
	      (version ,(getf (cdar body) 'version))
	      ,@(process-rss-body (cdr body) nil))
     elseif (and (consp (car body))
		 (eq 'net.rss.rdf:RDF (caar body)))
       then ;; rss 1.0
	    `(rss (version "1.0") ,@(process-rss-body (cdr body) 'rdf)))))


(defun process-rss-body (body rdfp) 
  (mapcar 
   (lambda (ch)
     `(channel
       ,@(mapcar
	  (lambda (it)
	    `(,(car it) ,@(big-stringify (cdr it))))
	  (find-not-items 'item (cdr ch)))
       (all-items
	,@(mapcar
	   (lambda (it
		    &aux (torrent (find-items 'torrent it))
			 ;; EZTV hack:
			 (fileName
			  (when torrent
			    (car (find-items 'fileName (car torrent))))))
	     `(item
	       ,@(mapcar
		  (lambda (vv)
		    `(,(car vv)
		      ,@(big-stringify (cdr vv))))
		  (find-conses (cdr it)))
	       ,@(when fileName (list fileName))))
	   (find-items 'item 
		       (if* rdfp
			  then ;; found in main body
			       (cdr body)
			  else ;; found in channel
			       (cdr ch)))))))
   (find-items 'channel body)))
  
			 
(defun find-items (key objs)
  ;; find all items beginning with key
  (let (res)
    (dolist (obj objs)
      (if* (and (consp obj) (or (eq key (car obj))
				(and (consp (car obj))
				     (eq key (caar obj)))))
	 then (push obj res)))
    (nreverse res)))

(defun find-not-items (key objs)
  ;; find conses not beginning with key
  (let (res)
    (dolist (obj objs)
      (if* (and (consp obj) 
		(not (or (eq key (car obj))
			 (and (consp (car obj))
			      (eq key (caar obj))))))
	 then (push obj res)))
    (nreverse res)))

(defun find-conses (objs)
  ;; find all things that are conses
  (let (res)
    (dolist (obj objs)
      (if* (consp obj)
	 then (push obj res)))
    (nreverse res)))


(defun big-stringify (objs)
  ;; concatenate all consecutie strings in objs
  (let (res thisres)
    (do* ((xx objs (cdr xx))
	  (val (car xx) (car xx)))
	((null xx)
	 (if* thisres
	    then (push (apply #'concatenate 'string (nreverse thisres))
		       res))
	 (nreverse res))
      (if* (stringp val)
	 then (push val thisres)
       elseif thisres
	 then (push (apply #'concatenate 'string (nreverse thisres))
		    res)
	      (setq thisres nil)
	      
	      (push val res)))))



;; functions for searching the feed results

(defun feed-rss-p (feed &optional version)
  ;; test if is feed is an rss feed (and if version is given test
  ;; that it is of that particular version).
  ;;
  ;; return the version number if true (or return t if it's an rss
  ;; feed but the version isn't known).
  ;; 
  (and (consp feed)
       (eq 'rss (car feed))
       (if* version
	  then (if* (equal (feed-slot-value feed 'version) version)
		  then version)
	  else (or(feed-slot-value feed 'version) t))))


(defun feed-slot-value (feed slot-name)
  (dolist (ent feed)
    (if* (consp ent) 
       then (if* (consp (car ent))
	       then (if* (eq slot-name (caar ent))
		       then (return (cdr ent)))
	     elseif (eq slot-name (car ent))
	       then (return (cdr ent))))))
		    
(defun testit ()
  (format t "0.91 feed ~%")
  (pprint (read-feed "http://www.franz.com/rss091_main.xml"))
  
  (format t "2.0 feed ~%")
  (pprint (read-feed "http://www.franz.com/rss20_main.xml"))
  
  (format t "1.0 feed ~%")
  (pprint (read-feed "http://www.franz.com/rss10_main.xml")))

(provide :rssreader)
