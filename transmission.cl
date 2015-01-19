
(in-package :user)

(defvar *transmission-directory*
    "~/Library/Application Support/Transmission/Torrents/")

(defparameter *filename-to-torrent-cache* nil)

(defun transmission-filename-to-tracker (given-filename
					 &key debug hash
					 &aux temp
					      temp2)
  (when (null *filename-to-torrent-cache*)
    (setq *filename-to-torrent-cache*
      (make-hash-table :size 777 :test #'equal)))
  (when (setq temp (gethash given-filename *filename-to-torrent-cache*))
    (return-from transmission-filename-to-tracker temp))
  (setq temp2
    (excl.osi:command-output
	     (format nil "ls -la \"~a\"" (truename *transmission-directory*))
	     :whole t))
  (dolist (torrent-file (directory
			 (merge-pathnames "*.torrent"
					  *transmission-directory*)))
    (if* (probe-file torrent-file)
       then (let* ((dict (bdecode-file torrent-file))
		   (announce (dict-get "announce" dict))
		   (info (dict-get "info" dict))
		   (name (dict-get "name" info))
		   ;; The hash prefix at the end of the .torrent filename
		   hash-prefix)
	      ;; If the filename was renamed in Transmission, the simple
	      ;; `name' to `given-filename' test will fail.  We can salvage
	      ;; this by looking at the hash included in the filename.
	      (when (=~ "\\.([0-9a-fA-F]+)\\.torrent$"
			(file-namestring torrent-file))
		(setq hash-prefix $1))
	      (when (or (string= name given-filename)
			(and hash-prefix
			     hash
			     (match-re hash-prefix hash :return nil)))
		(setf (gethash given-filename *filename-to-torrent-cache*)
		  (setq temp
		    (net.uri:uri-host (net.uri:parse-uri announce))))
		(and debug (format t "~a: ~s~%" given-filename temp))
		(return temp)))
       else ;; For reasons I don't understand, torrent-file sometimes
	    ;; disappears. WTF?  It was there at the top of the loop?
	    (warn "torrent file disappeared! ~a" torrent-file)
	    (format t "~a~%" temp2)
	    )))
