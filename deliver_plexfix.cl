
(in-package :user)

(setq excl::*break-on-warnings* t)
(load (compile-file-if-needed "defs.cl"))
(load (compile-file-if-needed "utils.cl"))
(load (compile-file "plexfix.cl"))

(generate-application
 "plexfix"
 "plexfix/"
 '("defs.fasl"
   "utils.fasl"
   "plexfix.fasl")
 :restart-init-function 'main
 :application-administration '(:resource-command-line "-Q")
 :read-init-files nil			; don't read ACL init files
 :print-startup-message nil		; don't print ACL startup messages
 :ignore-command-line-arguments t	; ignore ACL (not app) cmd line options
 :suppress-allegro-cl-banner t
 :purify nil
 :autoload-warning nil
 :include-debugger nil
 :include-tpl t				; getting a backtrace on error
					; needs this
 :include-ide nil
 :include-devel-env nil
 :include-compiler nil
 :discard-arglists t
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t
 
 ;; for debugging:
 :verbose nil
 :build-input "build.in"
 :build-output "build.out"
 
 :runtime :standard
 )
