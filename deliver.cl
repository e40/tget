
(in-package :user)

(defvar *debug* nil)

(let ((*record-source-file-info* nil)
      (*load-source-file-info* nil)
      (excl::*break-on-warnings* t))
  (dolist (file '("rssreader.cl" "tget.cl"))
    (load (compile-file file))))

(generate-application
 "tget"
 "tget/"
 (append (if* *debug*
	    then '(:streamc :inspect :trace)
	    else nil)
	 '(:list2 :seq2 "rssreader.fasl" "tget.fasl"))
 :restart-init-function 'main
 :application-administration
 '(:resource-command-line "-Q")
 :read-init-files nil
 :print-startup-message nil

 :purify nil

 :include-debugger t ;;*debug*
 :include-tpl t ;;*debug*
 :include-compiler nil
 :include-devel-env nil
 :include-ide nil
 :discard-arglists (null *debug*)
 :discard-local-name-info (null *debug*)
 :discard-source-file-info (null *debug*)
 :discard-xref-info (null *debug*)
 
 ;; debugging:
 :verbose *debug*
 :build-input "build.in"
 :build-output "build.out"
 
 ;; dumplisp arguments:
 :ignore-command-line-arguments t
 :suppress-allegro-cl-banner t)

(exit 0)
