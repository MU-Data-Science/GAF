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

(define-module (auth-manager config-reader)
  #:use-module (auth-manager config)
  #:use-module (logger)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:export (read-configuration-from-file))

(define (read-configuration-from-file filename)
  (log-debug "read-configuration-from-file" "Reading ~s." filename)
  (catch #t
    (lambda _
      (unless (file-exists? filename)
        (throw 'file-does-not-exist filename))
      (let* ((sxml-data (call-with-input-file filename
                          (lambda (port)
                            (xml->sxml port #:trim-whitespace? #t))))
             (config (assoc-ref sxml-data 'auth-manager)))

        ;; Handle options
        ;; --------------------------------------------------------------------
        (let [(fork?        (assoc-ref config 'fork))
              (developer?   (assoc-ref config 'developer-mode))
              (backtrace?   (assoc-ref config 'backtrace-on-error))
              (address      (assoc-ref config 'bind-address))
              (port         (assoc-ref config 'port))
              (upload-root  (assoc-ref config 'upload-root))
              (name         (assoc-ref config 'name))
              (public-uri   (assoc-ref config 'public-uri))
              (rdf-store    (assoc-ref config 'rdf-store))
              (sg-web       (assoc-ref config 'sg-web))]
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
          (when name
            (set-www-name! (car name)))
          (when public-uri
            (set-self-uri! (car public-uri)))
          (when sg-web
            (let [(sg-web-uri (assoc-ref sg-web 'api-uri))]
              (cond
               [(not sg-web-uri)
                (throw 'invalid-sg-web-configuration
                 "Please add a <uri> to the <sg-web> definition.")]
               [else
                (set-sg-web-uri! (car sg-web-uri))])))
          (when rdf-store
            (let [(sparql-uri (assoc-ref rdf-store 'sparql-uri))
                  (backend    (assoc-ref rdf-store 'backend))]
              (if (not backend)
                  (throw 'invalid-rdf-store-configuration
                    "Please add a <backend> to the <rdf-store> definition.")
                  (set-rdf-store-backend! (string->symbol (car backend))))

              (if (not sparql-uri)
                  (throw 'invalid-rdf-store-configuration
                    "Please add a <sparql-uri> to the <rdf-store> definition.")
                  (set-rdf-store-uri! (car sparql-uri)))

              (when (string= (car backend) "virtuoso")
                (let [(isql-bin      (car (assoc-ref rdf-store 'isql-path)))
                      (isql-hostname (car (assoc-ref rdf-store 'isql-hostname)))
                      (isql-port     (car (assoc-ref rdf-store 'isql-port)))
                      (username      (car (assoc-ref rdf-store 'username)))
                      (password      (car (assoc-ref rdf-store 'password)))]
                  (when isql-bin      (set-isql-bin!      isql-bin))
                  (when isql-hostname (set-isql-hostname! isql-hostname))
                  (when isql-port     (set-isql-port!     isql-port))
                  (when username      (set-rdf-store-username! username))
                  (when password      (set-rdf-store-password! password))))))
          #t)))
    (lambda (key . args)
      (cond
       [(eqv? key 'invalid-rdf-store-configuration)
        (begin
          (display "Error: There was a problem with the ")
          (display "'rdf-store' property:")
          (newline)
          (display (car args))
          (newline)
          #f)]
       [(eqv? key 'invalid-sg-web-configuration)
        (begin
          (display "Error: There was a problem with the ")
          (display "'sg-web' property:")
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
