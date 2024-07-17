;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (auth-manager config)
  #:use-module (oop goops)
  #:use-module (www util)
  #:export (%user-agent

            fork-on-startup?
            set-fork-on-startup!

            developer-mode?
            set-developer-mode!

            set-www-upload-root!
            www-upload-root

            www-cache-root

            www-listen-address-family
            www-listen-address
            set-www-listen-address!

            www-listen-port
            set-www-listen-port!

            www-max-file-size
            set-www-max-file-size!

            www-name
            set-www-name!

            sg-web-uri
            set-sg-web-uri!

            self-uri
            set-self-uri!

            rdf-store-uri
            set-rdf-store-uri!

            rdf-store-backend
            set-rdf-store-backend!

            isql-bin
            set-isql-bin!
            isql-hostname
            set-isql-hostname!
            isql-port
            set-isql-port!
            rdf-store-username
            set-rdf-store-username!
            rdf-store-password
            set-rdf-store-password!))

;; ----------------------------------------------------------------------------
;; RUNTIME-CONFIGURATION CLASS
;; ----------------------------------------------------------------------------
;;
;; This class definition collects all run-time configurable options plus a few
;; “basic infrastructure” properties.
;;

(define-class <runtime-configuration> ()

  ;; Web settings
  ;; --------------------------------------------------------------------------

  (www-name                 #:init-value (gethostname)
                            #:getter get-www-name
                            #:setter set-www-name-private!)

  (www-self-uri               #:init-value '()
                            #:getter get-self-uri
                            #:setter set-self-uri-private!)

  (www-upload-root          #:init-value
                            (lambda _
                              (if (getenv "SG_WEB_UPLOAD_ROOT")
                                  (getenv "SG_WEB_UPLOAD_ROOT")
                                  (if (getenv "TMPDIR")
                                      (getenv "TMPDIR")
                                      "/tmp")))
                            #:setter set-www-upload-root-private!
                            #:getter get-www-upload-root)

  (www-cache-root           #:init-value
                            (lambda _
                              (let ((xdg-cache-home (getenv "XDG_CACHE_HOME")))
                                (if xdg-cache-home
                                    (string-append xdg-cache-home
                                                   "/sparqling-genomics")
                                    (string-append
                                     (getenv "HOME")
                                     "/.cache/sparqling-genomics"))))
                            #:getter get-www-cache-root)

  (www-max-file-size        #:init-value 250000000
                            #:getter get-www-max-file-size)

  (www-listen-address-family #:init-value AF_INET
                            #:getter get-www-listen-address-family
                            #:setter set-www-listen-address-family-private!)

  (www-listen-address       #:init-value INADDR_LOOPBACK
                            #:getter get-www-listen-address
                            #:setter set-www-listen-address-private!)

  (www-listen-port          #:init-value 5000
                            #:getter get-www-listen-port
                            #:setter set-www-listen-port-private!)

  ;; Other settings
  ;; --------------------------------------------------------------------------
  (fork-on-startup?         #:init-value #f
                            #:getter get-fork-on-startup?
                            #:setter set-fork-on-startup-private!)

  (developer-mode?          #:init-value #f
                            #:getter get-developer-mode?
                            #:setter set-developer-mode-private!)

  ;; sg-web configuration
  ;; --------------------------------------------------------------------------
  (sg-web-uri               #:init-value '()
                            #:getter get-sg-web-uri
                            #:setter set-sg-web-uri-private!)

  ;; RDF-store configuration
  ;; --------------------------------------------------------------------------
  (rdf-store-uri            #:init-value #f
                            #:getter get-rdf-store-uri
                            #:setter set-rdf-store-uri-private!)

  (rdf-store-backend        #:init-value #f
                            #:getter get-rdf-store-backend
                            #:setter set-rdf-store-backend-private!)

  (rdf-store-username       #:init-value #f
                            #:getter get-rdf-store-username
                            #:setter set-rdf-store-username-private!)

  (rdf-store-password       #:init-value #f
                            #:getter get-rdf-store-password
                            #:setter set-rdf-store-password-private!)

  ;; Virtuoso-specifics
  ;; --------------------------------------------------------------------------
  (isql-bin                 #:init-value #f
                            #:getter get-isql-bin
                            #:setter set-isql-bin-private!)

  (isql-hostname            #:init-value #f
                            #:getter get-isql-hostname
                            #:setter set-isql-hostname-private!)

  (isql-port                #:init-value #f
                            #:getter get-isql-port
                            #:setter set-isql-port-private!))


;; Create an instance of the <runtime-configuration> environment.
;; ----------------------------------------------------------------------------
(define %am-runtime-configuration (make <runtime-configuration>))

(define %user-agent "SPARQLing-genomics Authentication Manager")

;; ----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS
;; ----------------------------------------------------------------------------
;;
;; These functions abstract away the need to know about the implementation
;; details.  Each property of <runtime-configuration> can be accessed using a
;; parameterless function.
;;

(define (make-getter symbol)
  (primitive-eval `(define-syntax-rule (,symbol)
                     (,(symbol-append 'get- symbol)
                      %am-runtime-configuration))))

(define (make-setter symbol)
  (primitive-eval
   `(define-syntax-rule (,(symbol-append 'set- symbol '!) val)
      (,(symbol-append 'set- symbol '-private!)
       %am-runtime-configuration val))))

(define (make-getter/setter symbol)
  (make-getter symbol)
  (make-setter symbol))

(for-each make-getter
          '(developer-mode?
            fork-on-startup?
            www-listen-address
            www-listen-address-family))

(make-setter 'fork-on-startup)

(for-each make-getter/setter
          '(isql-bin
            isql-hostname
            isql-port
            rdf-store-backend
            rdf-store-password
            rdf-store-uri
            rdf-store-username
            self-uri
            sg-web-uri
            www-listen-port
            www-max-file-size
            www-name
            www-upload-root))

(define (www-cache-root)
  (let ((cache-root (get-www-cache-root %am-runtime-configuration)))
    (unless (file-exists? (cache-root))
      (mkdir-p (cache-root)))
    (cache-root)))

(define (set-www-listen-address! arg)
  (if (string? arg)
      (set-www-listen-address-private!
       %am-runtime-configuration
       (cond
        [(string= arg "INADDR_ANY")       INADDR_ANY]
        [(string= arg "INADDR_LOOPBACK")  INADDR_LOOPBACK]
        [(string-contains arg ":")
         (begin
           (set-www-listen-address-family-private!
            %am-runtime-configuration AF_INET6)
           (inet-pton AF_INET6 arg))]
        [(string-contains arg ".")
         (begin
           (set-www-listen-address-family-private!
            %am-runtime-configuration AF_INET)
           (inet-pton AF_INET arg))]))
      #f))
