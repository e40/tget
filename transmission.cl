
(in-package :user)

(defvar *transmission-directory*
    "/me/tplex/transmission-daemon/info/torrents/")

(defparameter *filename-to-torrent-cache* nil)

(defun transmission-filename-to-tracker (given-filename
					 &key debug hash
					 &aux temp)
  (when (null *filename-to-torrent-cache*)
    (setq *filename-to-torrent-cache*
      (make-hash-table :size 777 :test #'equal)))
  (when (setq temp (gethash given-filename *filename-to-torrent-cache*))
    (return-from transmission-filename-to-tracker temp))
  (dolist (torrent-file (directory
			 (merge-pathnames "*.torrent"
					  *transmission-directory*)))
    (if* (probe-file torrent-file)
       then (let* ((dict (bittorrent::bdecode-file torrent-file))
		   (announce (bittorrent::dict-get "announce" dict))
		   (info (bittorrent::dict-get "info" dict))
		   (name (bittorrent::dict-get "name" info))
		   ;; The hash prefix at the end of the .torrent filename
		   hash-prefix)
	      ;; If the filename was renamed in Transmission, the simple
	      ;; `name' to `given-filename' test will fail.  We can salvage
	      ;; this by looking at the hash included in the filename.
	      (when (=~ "([0-9a-fA-F]+)\\.torrent$"
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
	    #+ignore (warn "torrent file disappeared! ~a" torrent-file)
	    #-ignore nil)))
