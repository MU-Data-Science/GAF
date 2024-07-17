;;; Copyright © 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www config-reader)
  #:use-module (ldap authenticate)
  #:use-module (logger)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:export (read-configuration-from-file))

(define (read-configuration-from-file filename)
  (log-debug "read-configuration-from-file" "Reading ~s." filename)
  (set-configuration-file! filename)
  (catch #t
    (lambda _
      (unless (file-exists? filename)
        (throw 'file-does-not-exist filename))
      (let* ((sxml-data (call-with-input-file filename
                          (lambda (port)
                            (xml->sxml port #:trim-whitespace? #t))))
             (config (assoc-ref sxml-data 'web-interface)))

        ;; Handle options
        ;; --------------------------------------------------------------------
        (let [(fork?             (assoc-ref config 'fork))
              (developer?        (assoc-ref config 'developer-mode))
              (backtrace?        (assoc-ref config 'backtrace-on-error))
              (upload-root       (assoc-ref config 'upload-root))
              (address           (assoc-ref config 'bind-address))
              (port              (assoc-ref config 'port))
              (beacon            (assoc-ref config 'beacon))
              (authentication    (assoc-ref config 'authentication))
              (sys-connection    (assoc-ref config 'system-connection))]
          (when (and fork? (> (string->number (car fork?)) 0))
            (set-fork-on-startup! #t))
          (when (and developer? (string= (car developer?) "1"))
            (set-developer-mode! #t))
          (when (and backtrace? (string= (car backtrace?) "1"))
            (set-backtrace-on-error! #t))
          (when upload-root
            (if (file-exists? (car upload-root))
                (set-www-upload-root! (car upload-root))
                (throw 'file-does-not-exist (car upload-root))))
          (when port
            (set-www-listen-port! (string->number (car port))))
          (when address
            (set-www-listen-address! (car address)))
          (when beacon
            (let [(enabled      (assoc-ref beacon 'enabled))
                  (connection   (assoc-ref beacon 'connection))
                  (organization (assoc-ref beacon 'organization))]
              (set-beacon-enabled! (string= (car enabled) "1"))
              (when (beacon-enabled?)
                (log-debug "read-configuration-from-file"
                           "The Beacon service has been enabled.")
                (if connection
                    (let [(uri         (assoc-ref connection 'uri))
                          (backend     (assoc-ref connection 'backend))
                          (username    (assoc-ref connection 'username))
                          (password    (assoc-ref connection 'password))]
                      (cond
                       [(not uri)       (throw 'invalid-beacon-connection
                                               "No URI specified.")]
                       [(null? uri)     (throw 'invalid-beacon-connection
                                               "The URI may not be empty.")]
                       [(not backend)   (throw 'invalid-beacon-connection
                                               "No backend specified.")]
                       [(null? backend) (throw 'invalid-beacon-connection
                                               "Invalid backend specified.")]
                       [(not (member (string->symbol (car backend))
                                     (sparql-available-backends)))
                        (throw 'invalid-beacon-connection
                               "Invalid backend specified.")]
                       [else
                        (set-beacon-connection!
                         (alist->connection
                          `((name     . "beacon-connection")
                            (uri      . ,(car uri))
                            (username . ,(if (or (not username)
                                                 (null? username))
                                             ""
                                             (car username)))
                            (password . ,(if (or (not password)
                                                 (null? password))
                                             ""
                                             (car password)))
                            (backend  . ,(car backend)))))]))
                    (throw 'invalid-beacon-connection
                           "Missing 'connection' for 'beacon'."))
                (if organization
                    (let [(name        (assoc-ref organization 'name))
                          (id          (assoc-ref organization 'id))
                          (description (assoc-ref organization 'description))
                          (address     (assoc-ref organization 'address))
                          (welcome-url (assoc-ref organization 'welcome-url))
                          (logo-url    (assoc-ref organization 'logo-url))
                          (info        (assoc-ref organization 'info))]
                      (if name
                          (set-beacon-organization-name! (car name))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon organization name was set.")))
                      (if id
                          (set-beacon-organization-id! (car id))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon organization ID was set.")))
                      (if description
                          (set-beacon-organization-description!
                           (multi-line-trim (car description)))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon organization description was set.")))
                      (if address
                          (set-beacon-organization-address! (car address))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No address for the Beacon endpoint was set.")))
                      (if welcome-url
                          (set-beacon-organization-welcome-url!
                           (car welcome-url))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon welcome URL was set.")))
                      (if logo-url
                          (set-beacon-organization-logo-url! (car logo-url))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon logo URL was set.")))
                      (if info
                          (set-beacon-organization-info! (car info))
                          (when (beacon-enabled?)
                            (log-warning "read-configuration-from-file"
                                         "No Beacon 'info' was set."))))))))
          (when authentication
            (cond
             ;; LDAP is preferred over local-user authentication.
             [(assoc-ref authentication 'ldap)
              (if (not (ldap-is-available?))
                  (begin
                    (display "Warning: Your configuration specifies an LDAP ")
                    (display "connection, but LDAP is unavailable.  Please ")
                    (display "enable LDAP-support when building ")
                    (display "sparqling-genomics.")
                    (newline))
                  (let ((ldap (assoc-ref authentication 'ldap)))
                    (set-ldap-uri! (car (assoc-ref ldap 'uri)))
                    (when (assoc-ref ldap 'common-name)
                      (set-ldap-common-name! (car (assoc-ref ldap 'common-name))))
                    (when (assoc-ref ldap 'organizational-unit)
                      (set-ldap-organizational-unit!
                       (car (assoc-ref ldap 'organizational-unit))))
                    (when (assoc-ref ldap 'ssl-certificate-directory)
                      (set-ldap-ssl-certificate-directory!
                       (car (assoc-ref ldap 'ssl-certificate-directory)))
                      (log-debug "read-configuration-from-file"
                                 "LDAP uses the certificate directory ~s."
                                 (ldap-ssl-certificate-directory)))
                    (when (assoc-ref ldap 'ssl-ca-certificate-file)
                      (set-ldap-ssl-certificate-file!
                       (car (assoc-ref ldap 'ssl-ca-certificate-file)))
                      (log-debug "read-configuration-from-file"
                                 "LDAP uses the CA certificate bundle from ~s."
                                 (ldap-ssl-certificate-file)))
                    (set-ldap-domain! (car (assoc-ref ldap 'domain)))
                    (set-ldap-enabled! #t)))]
             ;; Local-user configuration is an alternative to LDAP auth.
             [(assoc-ref authentication 'user)
              (for-each (lambda (user)
                          (if (or (null? (assoc-ref user 'username))
                                  (null? (assoc-ref user 'password)))
                              (begin
                                (display "An empty username or password is not ")
                                (display "allowed.")
                                (newline))
                              (let [(username (car (assoc-ref user 'username)))
                                    (password (car (assoc-ref user 'password)))]
                                (add-local-user! username password))))
                        authentication)]))
          (if sys-connection
              (let [(uri         (assoc-ref sys-connection 'uri))
                    (backend     (assoc-ref sys-connection 'backend))
                    (username    (assoc-ref sys-connection 'username))
                    (password    (assoc-ref sys-connection 'password))]
                (cond
                 [(not uri)       (throw 'invalid-system-connection "No URI specified.")]
                 [(null? uri)     (throw 'invalid-system-connection "The URI may not be empty.")]
                 [(not backend)   (throw 'invalid-system-connection "No backend specified.")]
                 [(null? backend) (throw 'invalid-system-connection "Invalid backend specified.")]
                 [(not (member (string->symbol (car backend))
                               (sparql-available-backends)))
                  (throw 'invalid-system-connection "Invalid backend specified.")]
                 [else
                  (set-system-connection! (alist->connection
                                           `((name     . "system-connection")
                                             (uri      . ,(car uri))
                                             (username . ,(if (or (not username)
                                                                  (null? username))
                                                              ""
                                                              (car username)))
                                             (password . ,(if (or (not password)
                                                                  (null? password))
                                                              ""
                                                              (car password)))
                                             (backend  . ,(car backend)))))]))
              (throw 'invalid-system-connection "Missing 'system-connection'."))
          #t)))
    (lambda (key . args)
      (cond
       [(eqv? key 'invalid-system-connection)
        (begin
          (display "Error: There was a problem with the ")
          (display "'system-connection' property:")
          (newline)
          (display (car args))
          (newline)
          #f)]
       [(eqv? key 'invalid-beacon-connection)
        (begin
          (display "Error: There was a problem with the ")
          (display "'beacon-connection' property:")
          (newline)
          (display (car args))
          (newline)
          #f)]
       [(eqv? key 'file-does-not-exist)
        (begin
          (format #t "Error: ~s does not exist.~%" (car args))
          #f)]
       [else
        (format #t "Error: Couldn't read configuration file (~a: ~a).~%"
                key args)
        #f]))))
