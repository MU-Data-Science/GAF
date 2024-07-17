;;; Copyright © 2016, 2017, 2018, 2019, 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www config)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (www util)
  #:use-module (sparql driver)
  #:use-module ((www db connections)
                #:select (connection-uri
                          connection-username
                          connection-password
                          connection-backend
                          default-connection
                          system-wide-connection?))
  #:export (beacon-connection
            beacon-enabled?
            beacon-organization-name
            beacon-organization-id
            beacon-organization-description
            beacon-organization-address
            beacon-organization-welcome-url
            beacon-organization-contact-url
            beacon-organization-logo-url
            beacon-organization-info
            beacon-sparql-query
            configuration-file
            default-prefixes
            internal-prefixes
            developer-mode?
            set-developer-mode!
            system-state-graph
            system-sparql-query
            sparql-query-with-connection
            is-shorthand-uri?
            shorthand-uri->uri
            uri->shorthand-uri
            fork-on-startup?
            set-fork-on-startup!
            set-system-connection!
            set-www-listen-address!
            set-www-listen-port!
            graph-name-max-length
            uri-string
            system-connection
            www-listen-address
            www-listen-address-family
            www-listen-port
            www-max-file-size
            www-name
            www-version
            www-hostname
            www-root
            www-cache-root
            www-static-root
            www-upload-root
            ldap-enabled?
            ldap-uri
            ldap-common-name
            ldap-organizational-unit
            ldap-domain
            ldap-ssl-certificate-directory
            ldap-ssl-certificate-file
            local-users
            add-local-user!
            set-www-upload-root!
            set-beacon-enabled!
            set-configuration-file!
            set-ldap-enabled!
            set-ldap-uri!
            set-ldap-common-name!
            set-ldap-organizational-unit!
            set-ldap-domain!
            set-ldap-ssl-certificate-directory!
            set-ldap-ssl-certificate-file!
            set-beacon-connection!
            set-beacon-organization-name!
            set-beacon-organization-id!
            set-beacon-organization-description!
            set-beacon-organization-address!
            set-beacon-organization-welcome-url!
            set-beacon-organization-contact-url!
            set-beacon-organization-logo-url!
            set-beacon-organization-info!
            persist-connection-lock
            set-persist-connection-lock!))


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
  (www-name                 #:init-value "sparqling-genomics"
                            #:getter get-www-name)

  (www-version              #:init-value "0.99.11"
                            #:getter get-www-version)

  (www-hostname             #:init-value "http://sparqling-genomics/"
                            #:getter get-www-hostname)

  (www-root                 #:init-value
                            (lambda _
                              (if (getenv "SG_WEB_ROOT")
                                  (getenv "SG_WEB_ROOT")
                                  "/usr/local/share/sparqling-genomics/web"))
                            #:getter get-www-root)

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
  (www-static-root          #:init-value
                            (lambda _
                              (if (getenv "SG_WEB_ROOT")
                                  (string-append (getenv "SG_WEB_ROOT") "/static")
                                   "/usr/local/share/sparqling-genomics/web/static"))
                            #:getter get-www-static-root)

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

  (beacon-enabled?          #:init-value #f
                            #:getter get-beacon-enabled?
                            #:setter set-beacon-enabled-private!)

  (ldap-enabled?            #:init-value #f
                            #:getter get-ldap-enabled?
                            #:setter set-ldap-enabled-private!)

  (ldap-uri                 #:init-value #f
                            #:getter get-ldap-uri
                            #:setter set-ldap-uri-private!)

  (ldap-common-name         #:init-value #f
                            #:getter get-ldap-common-name
                            #:setter set-ldap-common-name-private!)

  (ldap-organizational-unit #:init-value #f
                            #:getter get-ldap-organizational-unit
                            #:setter set-ldap-organizational-unit-private!)

  (ldap-domain              #:init-value #f
                            #:getter get-ldap-domain
                            #:setter set-ldap-domain-private!)

  (ldap-ssl-certificate-directory #:init-value #f
                            #:getter get-ldap-ssl-certificate-directory
                            #:setter set-ldap-ssl-certificate-directory-private!)

  (ldap-ssl-certificate-file #:init-value #f
                            #:getter get-ldap-ssl-certificate-file
                            #:setter set-ldap-ssl-certificate-file-private!)

  (local-users              #:init-value '()
                            #:getter get-local-users
                            #:setter set-local-users-private!)

  ;; Other settings
  ;; --------------------------------------------------------------------------
  (fork-on-startup?         #:init-value #f
                            #:getter get-fork-on-startup?
                            #:setter set-fork-on-startup-private!)

  (graph-name-max-length    #:init-value 128
                            #:getter get-graph-name-max-length)

  (developer-mode?          #:init-value #f
                            #:getter get-developer-mode?
                            #:setter set-developer-mode-private!)

  (configuration-file       #:init-value #f
                            #:getter get-configuration-file
                            #:setter set-configuration-file-private!)

  ;; Beacon settings
  ;; --------------------------------------------------------------------------
  (beacon-organization-name #:init-value #nil
                            #:getter get-beacon-organization-name
                            #:setter set-beacon-organization-name-private!)

  (beacon-organization-id   #:init-value #nil
                            #:getter get-beacon-organization-id
                            #:setter set-beacon-organization-id-private!)

  (beacon-organization-description #:init-value #nil
                            #:getter get-beacon-organization-description
                            #:setter set-beacon-organization-description-private!)

  (beacon-organization-address #:init-value #nil
                            #:getter get-beacon-organization-address
                            #:setter set-beacon-organization-address-private!)

  (beacon-organization-welcome-url #:init-value #nil
                            #:getter get-beacon-organization-welcome-url
                            #:setter set-beacon-organization-welcome-url-private!)

  (beacon-organization-contact-url #:init-value #nil
                            #:getter get-beacon-organization-contact-url
                            #:setter set-beacon-organization-contact-url-private!)

  (beacon-organization-logo-url #:init-value #nil
                            #:getter get-beacon-organization-logo-url
                            #:setter set-beacon-organization-logo-url-private!)

  (beacon-organization-info #:init-value #nil
                            #:getter get-beacon-organization-info
                            #:setter set-beacon-organization-info-private!)

  (beacon-connection        #:init-value '()
                            #:getter get-beacon-connection
                            #:setter set-beacon-connection-private!)

  ;; System connection settings
  ;; --------------------------------------------------------------------------
  (system-connection        #:init-value '()
                            #:getter get-system-connection
                            #:setter set-system-connection-private!)

  ;; Mutexes
  ;; --------------------------------------------------------------------------
  (persist-connection-lock  #:init-value #nil
                            #:getter get-persist-connection-lock
                            #:setter set-persist-connection-lock-private!))


;; Create an instance of the <runtime-configuration> environment.
;; ----------------------------------------------------------------------------
(define %runtime-configuration (make <runtime-configuration>))


;; ----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS
;; ----------------------------------------------------------------------------
;;
;; These functions abstract away the need to know about the implementation
;; details.  Each property of <runtime-configuration> can be accessed using a
;; parameterless function.
;;
;; Instead of writing out a syntax-rule for each getter/setter, we use
;; primitive-eval to generate the definitions.
;;

(define (make-getter symbol)
  (primitive-eval `(define-syntax-rule (,symbol)
                     (,(symbol-append 'get- symbol)
                      %runtime-configuration))))

(define (make-setter symbol)
  (primitive-eval
   `(define-syntax-rule (,(symbol-append 'set- symbol '!) val)
      (,(symbol-append 'set- symbol '-private!)
       %runtime-configuration val))))

(define (make-getter/setter symbol)
  (make-getter symbol)
  (make-setter symbol))

(for-each make-getter
          '(beacon-enabled?
            configuration-file
            developer-mode?
            fork-on-startup?
            graph-name-max-length
            ldap-enabled?
            local-users
            www-hostname
            www-listen-address
            www-listen-address-family
            www-max-file-size
            www-name
            www-root
            www-version))

(for-each make-setter
          '(beacon-enabled
            configuration-file
            developer-mode
            fork-on-startup
            ldap-enabled))

(for-each make-getter/setter
          '(beacon-connection
            beacon-organization-address
            beacon-organization-contact-url
            beacon-organization-description
            beacon-organization-id
            beacon-organization-info
            beacon-organization-logo-url
            beacon-organization-name
            beacon-organization-welcome-url
            ldap-common-name
            ldap-domain
            ldap-organizational-unit
            ldap-ssl-certificate-directory
            ldap-ssl-certificate-file
            ldap-uri
            persist-connection-lock
            system-connection
            www-listen-port
            www-upload-root))

(define (www-root)
  ((get-www-root %runtime-configuration)))

(define (www-static-root)
  ((get-www-static-root %runtime-configuration)))

(define (www-cache-root)
  (let ((cache-root (get-www-cache-root %runtime-configuration)))
    (unless (file-exists? (cache-root))
      (mkdir-p (cache-root)))
    (cache-root)))

(define (set-www-listen-address! arg)
  (if (string? arg)
      (set-www-listen-address-private!
       %runtime-configuration
       (cond
        [(string= arg "INADDR_ANY")       INADDR_ANY]
        [(string= arg "INADDR_LOOPBACK")  INADDR_LOOPBACK]
        [(string-contains arg ":")
         (begin
           (set-www-listen-address-family-private!
            %runtime-configuration AF_INET6)
           (inet-pton AF_INET6 arg))]
        [(string-contains arg ".")
         (begin
           (set-www-listen-address-family-private!
            %runtime-configuration AF_INET)
           (inet-pton AF_INET arg))]))
      #f))

(define (add-local-user! username password)
  (set-local-users-private! %runtime-configuration
    (cons (list username password)
          (get-local-users %runtime-configuration))))

;; ----------------------------------------------------------------------------
;; DEFAULT URIS
;; ----------------------------------------------------------------------------
;;
;; The following symbol collects commonly used URI strings.
;;

(define internal-uri-strings
  '((agent        . "https://sparqling-genomics.org/0.99.11/Agent/")
    (auth         . "https://sparqling-genomics.org/0.99.11/Authorization/")
    (dcterms      . "http://purl.org/dc/terms/")
    (faldo        . "http://biohackathon.org/resource/faldo#")
    (project      . "https://sparqling-genomics.org/0.99.11/Project/")
    (prov         . "http://www.w3.org/ns/prov#")
    (query        . "https://sparqling-genomics.org/0.99.11/Query/")
    (rdf          . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (sg           . "https://sparqling-genomics.org/0.99.11/")
    (seq          . "sg://0.99.11/vcf2rdf/sequence/")
    (vc           . "sg://0.99.11/vcf2rdf/variant/")
    (xml2rdf      . "sg://0.99.11/xml2rdf/")
    (xsd          . "http://www.w3.org/2001/XMLSchema#")))

(define default-uri-strings
  '((dc           . "http://purl.org/dc/elements/1.1/")
    (dcterms      . "http://purl.org/dc/terms/")
    (dctype       . "http://purl.org/dc/dcmitype/")
    (faldo        . "http://biohackathon.org/resource/faldo#")
    (jdt          . "sg://0.99.11/json2rdf/DynamicType/")
    (prov         . "http://www.w3.org/ns/prov#")
    (rdf          . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rdfs         . "http://www.w3.org/2000/01/rdf-schema#")
    (sg           . "https://sparqling-genomics.org/0.99.11/")
    (vcf2rdf      . "sg://0.99.11/vcf2rdf/")
    (col          . "sg://0.99.11/table2rdf/Column/")
    (xdt          . "sg://0.99.11/xml2rdf/DynamicType/")
    (xsd          . "http://www.w3.org/2001/XMLSchema#")))

(define (generate-prefixes prefixes)
  (format #f "~{PREFIX ~a~%~}" (map (lambda (item)
                                      (format #f "~12a <~a>"
                                              (symbol-append (car item) ':)
                                              (cdr item)))
                                    prefixes)))

(define default-prefixes  (generate-prefixes default-uri-strings))
(define internal-prefixes (generate-prefixes internal-uri-strings))

(define (uri->shorthand-uri input)
  (catch #t
    (lambda _
      (car (delete '#f
             (map (lambda (prefix)
                    (if (string-prefix? (cdr prefix) input)
                        (let [(remainder (substring input (string-length (cdr prefix))))]
                          (if (string-index remainder #\/)
                              #f
                              (format #f "~a:~a" (car prefix) remainder)))
                        #f))
                  default-uri-strings))))
    (lambda (key . args)
      input)))

(define (shorthand-uri->uri input)
  (catch #t
    (lambda _
      (car (delete '#f
             (map (lambda (prefix)
                    (let [(prefix-str (string-append
                                       (symbol->string (car prefix)) ":"))]
                      (if (string-prefix? prefix-str input)
                          (let [(remainder (substring input (string-length prefix-str)))]
                            (if (string-index remainder #\/)
                                #f
                                (string-append (cdr prefix) remainder)))
                          #f)))
                  default-uri-strings))))
    (lambda (key . args)
      input)))

(define (is-shorthand-uri? input)
  (catch #t
    (lambda _
      (not (null?
            (delete '#f
                    (map (lambda (prefix)
                           (and (string-prefix?
                                 (string-append
                                  (symbol->string (car prefix)) ":")
                                 input)
                                (not (string-prefix? "sg://" input))))
                         default-uri-strings)))))
    (lambda (key . args) #f)))

(define system-state-graph "http://sparqling-genomics.org/sg-web/state")

(define-syntax-rule
  (uri-string key)
  (assoc-ref default-uri-strings key))

(define (sparql-query-with-connection connection query token project-hash)
  (if (system-wide-connection? connection)
      (sparql-query query
                    #:uri           (connection-uri connection)
                    #:store-backend 'sparqling-genomics
                    #:token         token
                    #:project-hash  project-hash)
      (sparql-query query
                    #:uri (connection-uri connection)
                    #:store-backend (connection-backend connection)
                    #:digest-auth
                    (if (and (connection-username connection)
                             (connection-password connection))
                        (string-append
                         (connection-username connection) ":"
                         (connection-password connection))
                        #f))))

(define-syntax-rule (system-sparql-query query)
  (sparql-query-with-connection (system-connection) query #f #f))

(define-syntax-rule (beacon-sparql-query query)
  (sparql-query-with-connection (beacon-connection) query #f #f))
