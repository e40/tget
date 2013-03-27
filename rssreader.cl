;; copyright (c) 2005 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: rssreader.cl,v 1.4 2005/05/31 15:33:37 layer Exp $

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
	   :net.rss)))
  
  





(defun read-feed (url)
  ;;
  ;;* exported
  ;;
  ;; read the feed given by the url and return a feed value
  ;;
  (multiple-value-bind (content code headers)
      (do-http-request url)
    
    (declare (ignore headers))
    
    (if* (not (eq 200 code))
       then (error "Accessing url ~s gave http response code ~s" url code))
    
    (parse-feed content)))

(defun parse-feed (content)
  ;;
  ;;* exported
  ;;
  ;; read the feed given by the url and return a feed value
  ;;
  (let ((body (car 
	       (let ((*package* (find-package :net.rss)))
		 (parse-xml content 
			    ;; the following line requires an
			    ;; acl patch not yet available??
;;;;eztv.it feed requires this????
			    :external nil
			    :uri-to-package *uri-to-package*)))))
					;(pprint body)
					;(setq *body* body)
    (if* (and (consp (car body))
	      (eq 'rss (caar body)))
       then `(rss
	      (version ,(getf (cdar body) 'version))
	      ,@(process-rss-body (cdr body) nil)
	      )
     elseif (and (consp (car body))
		 (eq 'net.rss.rdf:RDF (caar body)))
       then				; rss 1.0
	    `(rss (version "1.0")
		  ,@(process-rss-body (cdr body) 'rdf)))))


(defun process-rss-body (body rdfp) 
  (mapcar 
   #'(lambda (ch)
       `(channel
	 ,@(mapcar
	    #'(lambda (it)
		`(,(car it) ,@(big-stringify (cdr it))))
	    (find-not-items 'item (cdr ch)))
	 (all-items ,@(mapcar 
		       #'(lambda (it)
			   `(item
			     ,@(mapcar #'(lambda (vv)
					   `(,(car vv)
					     ,@(big-stringify (cdr vv))))
				       (find-conses (cdr it)))))
		       (find-items 'item 
				   (if* rdfp
				      then (cdr body) ; found in main body
				      else (cdr ch)) ; found in channel
				   )
		       ))))
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
