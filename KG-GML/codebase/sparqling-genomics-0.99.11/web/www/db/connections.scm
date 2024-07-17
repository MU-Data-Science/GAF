;;; Copyright © 2018, 2019, 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www db connections)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (logger)
  #:use-module (oop goops)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www util)

  #:export (connection-add
            connection-edit
            remove-user-connection
            remove-system-wide-connection
            connections-by-user
            connection-by-name
            user-connection-by-name
            system-wide-connection-by-name
            load-user-connections
            load-system-wide-connections

            alist->connection
            alist->user-connection
            alist->system-wide-connection
            connection->alist
            connection->alist-safe

            connection-name
            connection-uri
            connection-backend
            connection-username
            connection-password
            connection-is-default?
            connection?
            user-connection?
            system-wide-connection?

            connection-is-online?
            connection-down-since
            set-connection-down-since!

            connection-set-as-default!
            set-connection-name!
            set-connection-username!
            set-connection-password!

            persist-user-connections
            persist-system-wide-connections
            default-connection))


;; SYSTEM-WIDE-CONNECTION RECORD TYPE
;; ----------------------------------------------------------------------------
;;
;; Connections can be per-user (with separate credentials), or system-wide
;; (using session tokens through the ‘sg-auth-manager’).
;;
;; These user-connections are made by users themselves, while system-wide
;; connections are registered by ‘sg-auth-manager’ through a call to
;; ‘/api/register-connection’.
;;
;; Instead of implementing record types, we use object-oriented programming
;; to use inheritance to make a general “connection” type, and two specific
;; types: “user-connection”, and “system-wide-connection”.
;;

(define-class <connection> ()
  (name        #:init-value #nil
               #:init-keyword #:name
               #:getter connection-name
               #:setter set-connection-name!)

  (uri         #:init-value #nil
               #:init-keyword #:uri
               #:getter connection-uri
               #:setter set-connection-uri!))

(define (make-connection name uri)
  (make <connection>
    #:name name
    #:uri uri))

(define-class <system-wide-connection> (<connection>)
  (down-since  #:init-value #f
               #:init-keyword #:down-since
               #:getter connection-down-since
               #:setter set-connection-down-since!))

(define-method (write (self <system-wide-connection>) port)
  (format port "#<<system-wide-connection> name: ~s>" (connection-name self)))

(define-method (connection-backend (self <system-wide-connection>) port)
  (log-error "connection-backend" "~a"
             (string-append "A possibly erroneous call to "
                            "‘connection-backend’ for a "
                            "<system-wide-connection> was invoked."))
  'sparqling-genomics)

(define (make-system-wide-connection name uri down-since)
  (make <system-wide-connection>
    #:name name
    #:uri uri
    #:down-since down-since))

(define (system-wide-connection? instance)
  (catch #t
    (lambda _
      (eq? (class-name (class-of instance)) '<system-wide-connection>))
    (lambda (key . args) #f)))

;; USER CONNECTION
;; ----------------------------------------------------------------------------
;;
;; This class extends <connection> with a few properties.
;;

(define-class <user-connection> (<connection>)
  (backend     #:init-keyword #:backend
               #:getter connection-backend
               #:setter set-connection-backend!)

  (username    #:init-keyword #:username
               #:getter connection-username
               #:setter set-connection-username!)

  (password    #:init-keyword #:password
               #:getter connection-password
               #:setter set-connection-password!))

(define-method (write (self <user-connection>) port)
  (format port "#<<user-connection> name: ~s>" (connection-name self)))

(define (make-user-connection name uri backend username password)
  (make <user-connection>
    #:name        name
    #:uri         uri
    #:backend     backend
    #:username    username
    #:password    password))

(define (user-connection? instance)
  (catch #t
    (lambda _
      (eq? (class-name (class-of instance)) '<user-connection>))
    (lambda (key . args) #f)))


;; ALIST->CONNECTION AND CONNECTION->ALIST
;; ----------------------------------------------------------------------------
(define (alist->user-connection input)
  "Turns the association list INPUT into a <user-connection> instance."
  (let ((obj (make <user-connection>
               #:name        (assoc-ref input 'name)
               #:uri         (assoc-ref input 'uri)
               #:backend     (assoc-ref input 'backend)
               #:username    (assoc-ref input 'username)
               #:password    (assoc-ref input 'password))))

    (unless (and (string? (connection-username obj))
                 (not (string= (connection-username obj) "")))
      (set-connection-username! obj #f))

    (unless (and (string? (connection-password obj))
                 (not (string= (connection-password obj) "")))
      (set-connection-password! obj #f))

    (let ((backend (connection-backend obj)))
      (cond
       [(symbol? backend)
        (set-connection-backend! obj backend)]
       [(and (string? backend) (not (string= backend "")))
        (set-connection-backend! obj (string->symbol backend))]
       [else
        (set-connection-backend! obj 'virtuoso)]))
    obj))

(define (alist->system-wide-connection input)
  "Turns the association list INPUT into a <system-wide-connection> instance."
  (make <system-wide-connection>
    #:name        (assoc-ref input 'name)
    #:uri         (assoc-ref input 'uri)
    #:down-since  (assoc-ref input 'down-since)))

(define (alist->connection input)
  "Turns the association list INPUT into a <connection>-derived instance."
  (if (or (assoc-ref input 'backend)
          (assoc-ref input 'username)
          (assoc-ref input 'password))
      (alist->user-connection input)
      (alist->system-wide-connection input)))

(define (connection->alist record)
  (cond
   [(user-connection? record)
    `((name        . ,(connection-name     record))
      (uri         . ,(connection-uri      record))
      (backend     . ,(connection-backend  record))
      (username    . ,(connection-username record))
      (password    . ,(connection-password record)))]
   [(system-wide-connection? record)
    `((name        . ,(connection-name       record))
      (uri         . ,(connection-uri        record))
      (down-since  . ,(connection-down-since record)))]
   [else
    (log-error "connection->alist"
               "Connection records must be instances of either ~a"
               "<user-connection> or <system-wide-connection>.")
    #f]))

(define (connection->alist-safe record)
  (cond
   [(user-connection? record)
    `((name        . ,(connection-name     record))
      (uri         . ,(connection-uri      record))
      (backend     . ,(connection-backend  record))
      (username    . ,(connection-username record)))]
   [(system-wide-connection? record)
    `((name        . ,(connection-name       record))
      (uri         . ,(connection-uri        record))
      (down-since  . ,(connection-down-since record)))]
   [else
    (log-error "connection->alist-safe"
               "Connection records must be instances of either ~a"
               "<user-connection> or <system-wide-connection>.")
    #f]))

;; CONNECTIONS PERSISTENCE
;; ----------------------------------------------------------------------------

(define (persistence-path username)
  (string-append (www-cache-root) "/" username "/connections.scm"))

(define (system-wide-connections-file)
  (string-append (www-cache-root) "/system-connections.scm"))

(define (connection-defaults-path)
  (string-append (www-cache-root) "/default-connections.scm"))

(define (load-user-connections username)
  (catch #t
    (lambda _
      (let ((filename (persistence-path username)))
        (if (file-exists? filename)
          (call-with-input-file filename
            (lambda (port)
              (map alist->user-connection (read port))))
          '())))
    (lambda (key . args)
      '())))

(define (load-system-wide-connections)
  (catch #t
    (lambda _
      (let ((filename (system-wide-connections-file)))
        (if (file-exists? filename)
            (call-with-input-file filename
              (lambda (port)
                (map alist->system-wide-connection (read port))))
            '())))
    (lambda (key . args)
      #f)))

(define (persist-connections connections filename)
  "This function writes CONNECTIONS to FILENAME."
  (lock-mutex (persist-connection-lock))
  (unless (file-exists? (dirname filename))
    (mkdir-p (dirname filename)))
  (call-with-output-file filename
    (lambda (port)
      ;; Before writing to the file under 'port', chmod it so that
      ;; only the user this process runs as can read its contents.
      (chmod port #o600)
      (format port ";; This file was generated by sparqling-genomics.~%")
      (format port ";; Please do not edit this file manually.~%")
      (write (map connection->alist connections) port)))
    (unlock-mutex (persist-connection-lock)))

(define (persist-user-connections connections username)
  (persist-connections connections (persistence-path username)))

(define (persist-system-wide-connections connections)
  (persist-connections connections (system-wide-connections-file)))


;; CONNECTION-ADD
;; ----------------------------------------------------------------------------
(define* (connection-add record #:optional (username #f))
  "Adds a reference to the internal graph for the connection RECORD."
  (let [(name (connection-name record))
        (uri  (connection-uri  record))]
    (cond
     [(string-is-longer-than name (graph-name-max-length))
      (values #f
       (format #f "The connection name cannot be longer than ~a characters."
               (graph-name-max-length)))]
     [(string= name "")
      (values #f "The connection name cannot empty.")]
     [(string-contains name " ")
      (values #f "The connection name cannot contain whitespace characters.")]

     ;; User-specified connections.
     ;; -----------------------------------------------------------------------
     [(and (user-connection? record)
           username)
      (cond
       [(user-connection-by-name (connection-name record) username)
        (values #f "There already exists a connection with this name.")]
       [(string= uri "")
        (values #f "The connection URI cannot empty.")]
       [else
        (let* ((connections (connections-by-user username))
               (new-state   (cons record connections)))
          (persist-user-connections new-state username)
          (connection-set-as-default! record username)
          (values #t ""))])]

     ;; System-wide connections.
     ;; -----------------------------------------------------------------------
     [(system-wide-connection? record)
      (if (system-wide-connection-by-name (connection-name record))
          (values #t "Connection is already registered.")
          (let* ((connections (load-system-wide-connections))
                 (new-state   (cons record connections)))
            (persist-system-wide-connections new-state)
            (values #t "")))]
     [else
      #f])))


;; CONNECTION-EDIT
;; ----------------------------------------------------------------------------
(define (connection-edit record username)
  "Updates RECORD for which NAME equals the name of an existing record."
  (let ((name (connection-name record))
        (uri  (connection-uri  record)))
    (cond
     ((system-wide-connection? record)
      (values #f "System-wide connections cannot be modified."))
     ((string-is-longer-than name (graph-name-max-length))
      (values #f
       (format #f "The connection name cannot be longer than ~a characters."
               (graph-name-max-length))))
     ((string= name "")
      (values #f "An empty connection name is not allowed."))
     ((not (user-connection-by-name name username))
      (values #f "There is no connection with this name."))
     ((string= uri "")
      (values #f "The connection URI cannot empty."))
     ((string-contains name " ")
      (values #f "The connection name cannot contain whitespace characters."))
     ((user-connection? record)
      (let ((connections (load-user-connections username)))
        (persist-user-connections
         (cons record
               (delete #f
                       (map (lambda (connection)
                              (if (string= (connection-name connection) name)
                                  #f
                                  connection))
                            connections)))
         username)
        (values #t "The connection has been modified."))))))


;; REMOVE-*-CONNECTION
;; ----------------------------------------------------------------------------

(define (remove-user-connection connection username)
  "Removes the reference in the internal graph for CONNECTION."
  (let ((name        (if (string? connection)
                         connection
                         (connection-name connection)))
        (connections (load-user-connections username)))
    (persist-user-connections
     (filter (lambda (record)
               (not (string= (connection-name record) name)))
             connections)
     username)
    (values #t (format #f "Removed “~a”." name))))

(define (remove-system-wide-connection connection)
  "Removes the reference in the internal graph for CONNECTION."
  (let ((name        (if (string? connection)
                         connection
                         (connection-name connection)))
        (connections (load-system-wide-connections)))
    (persist-system-wide-connections
     (filter (lambda (record)
               (not (string= (connection-name record) name)))
             connections))
    (values #t (format #f "Removed “~a”." name))))

;; CONNECTION-SET-AS-DEFAULT!
;; ----------------------------------------------------------------------------
(define (connection-set-as-default! connection username)
  "Sets CONNECTION as the default connection."
  (let* ((name          (connection-name connection))
         (defaults-path (connection-defaults-path))
         (new-state (if (file-exists? defaults-path)
                        (call-with-input-file defaults-path
                          (lambda (port)
                            (let ((records (read port)))
                              (if (eof-object? records)
                                  `((,username . ,(connection-name connection)))
                                  (cons
                                   `(,username . ,(connection-name connection))
                                   (filter (lambda (record)
                                             (not (string= (car record) username)))
                                           records))))))
                        `((,username . ,(connection-name connection))))))
    (call-with-output-file defaults-path
      (lambda (port)
        (chmod port #o600)
        (format port ";; This file was generated by sparqling-genomics.~%")
        (format port ";; Please do not edit this file manually.~%")
        (write new-state port)))))

;; ALL-CONNECTIONS
;; ----------------------------------------------------------------------------

(define* (connections-by-user username #:key (filter #f))
  "Returns a list of connection records, applying FILTER to the records."
  (let* ((user-connections (delete #f (load-user-connections username)))
         (sys-connections  (delete #f (load-system-wide-connections)))
         (connections      (sort (append user-connections sys-connections)
                                 (lambda (first second)
                                   (string<? (connection-name first)
                                             (connection-name second))))))
    (if (and (not (null? connections))
             filter)
        (map filter connections)
        connections)))

;; CONNECTION-BY-NAME
;; ----------------------------------------------------------------------------

(define (system-wide-connection-by-name name)
  (let ((item (filter (lambda (connection)
                        (string= (connection-name connection) name))
                      (load-system-wide-connections))))
    (if (null? item)
        #f
        (car item))))

(define (user-connection-by-name name username)
  (let ((item (filter (lambda (connection)
                        (string= (connection-name connection) name))
                      (load-user-connections username))))
    (if (null? item)
        #f
        (car item))))

(define* (connection-by-name name #:optional (username #f))
  (let ((sys  (system-wide-connection-by-name name))
        (user (user-connection-by-name name username)))
    (if sys
        sys
        user)))

;; DEFAULT-CONNECTION
;; ----------------------------------------------------------------------------

(define (connection-is-default? record username)
  (let* ((name          (connection-name record))
         (defaults-path (connection-defaults-path))
         (defaults (if (file-exists? defaults-path)
                       (call-with-input-file defaults-path read)
                       '()))
         (user-default (assoc-ref defaults username)))
    (and (string? user-default)
         (string= (connection-name record) user-default))))

(define (default-connection username)
  (let ((default (delete #f (connections-by-user username
                              #:filter (lambda (connection)
                                         (if (connection-is-default?
                                              connection username)
                                             connection
                                             #f))))))
    (if (> (length default) 0)
        (car default)
        #f)))

;; AVAILABILITY
;; ----------------------------------------------------------------------------

(define (connection-is-online? record)
  (catch 'system-error
    (lambda _
      (let [(uri (connection-uri record))]
        (receive (header body)
            (http-get uri
              #:headers `((accept . ((application/s-expression)))))
          (unless (or (= (response-code header) 200)
                      (= (response-code header) 401))
            (throw 'system-error (response-code header))))
        #t))
    (lambda (key . args)
      #f)))
