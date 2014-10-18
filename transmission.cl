
(in-package :user)

(defvar *transmission-directory*
    "~/Library/Application Support/Transmission/Torrents/")

(defvar *filename-to-torrent-cache* nil)

(defun transmission-filename-to-tracker (given-filename
					 &key debug
					 &aux temp)
  (when (null *filename-to-torrent-cache*)
    (setq *filename-to-torrent-cache*
      (make-hash-table :size 777 :test #'equal)))
  (when (setq temp (gethash given-filename *filename-to-torrent-cache*))
    (return-from transmission-filename-to-tracker temp))
  (dolist (torrent-file (directory
			 (merge-pathnames "*.torrent"
					  *transmission-directory*)))
  
    (let* ((dict (bdecode-file torrent-file))
	   (announce (dict-get "announce" dict))
	   (info (dict-get "info" dict))
	   (name (dict-get "name" info)))
      (when (string= name given-filename)
	(setf (gethash given-filename *filename-to-torrent-cache*)
	  (setq temp
	    (net.uri:uri-host (net.uri:parse-uri announce))))
	(and debug (format t "~a: ~s~%" given-filename temp))
	(return temp)))))
