
(in-package :user)

(let ((*record-source-file-info* nil)
      (*load-source-file-info* nil)
      (excl::*break-on-warnings* t))
  (load "sys.cl"))

(generate-application
 "tget"
 "tget/"
 (append (if* *debug*
	    then '(:streamc :inspect :trace)
	    else nil)
	 (append '(:list2 :seq2 :disasm) user::*fasls*))
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
