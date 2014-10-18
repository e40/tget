
(in-package :user)

(setq excl::*break-on-warnings* t)
(load "bittorrent/bittorrent_full.fasl")
(load (compile-file "transmission.cl"))
(load (compile-file "seedstatus.cl"))

(generate-application
 "seedstatus"
 "seedstatus/"
 '("bittorrent/bittorrent_full.fasl"
   "transmission.fasl"
   "seedstatus.fasl")
 :restart-init-function 'main
 :application-administration '(:resource-command-line "-Q")
 :read-init-files nil			; don't read ACL init files
 :print-startup-message nil		; don't print ACL startup messages
 :ignore-command-line-arguments t	; ignore ACL (not app) cmd line options
 :suppress-allegro-cl-banner t
 :purify nil
 :autoload-warning nil
 :include-debugger nil
 :include-tpl nil
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
