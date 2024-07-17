;;; Copyright © 2016, 2017, 2018, 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www requests)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www db api)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (www db sessions)
  #:use-module (www pages edit-connection)
  #:use-module (www pages error)
  #:use-module (www pages project-details)
  #:use-module (www pages)
  #:use-module (www requests-api)
  #:use-module (www requests-beacon)
  #:use-module (www util)

  #:export (request-handler
            start-server))

;; ----------------------------------------------------------------------------
;; CONNECTION TEST
;; ----------------------------------------------------------------------------
;;
;; This function makes it easier to detect a connection failure that might
;; cause trouble later on.
;;

(define (system-is-connected?)
  (catch 'system-error
    (lambda _
      (let [(uri (connection-uri (system-connection)))]
        (call-with-values (lambda _ (http-get uri))
          (lambda (header body)
            (unless (or (= (response-code header) 200)
                        (= (response-code header) 401))
              (throw 'system-error ""))
            #t))))
    (lambda (key . args)
      (log-error "system-is-connected?" "Connection to ~a failed."
                 (connection-uri (system-connection)))
      #f)))

;; ----------------------------------------------------------------------------
;; HANDLERS
;; ----------------------------------------------------------------------------
;;
;; The way a request is handled varies upon the nature of the request.  It can
;; be as simple as serving a pre-existing file, or as complex as finding a
;; Scheme module to use for handling the request.
;;
;; In this section, the different handlers are implemented.
;;

(define (request-file-handler path client-port)
  "This handler takes data from a file and sends that as a response."

  (define (response-content-type path)
    "This function returns the content type of a file based on its extension."
    (let ((extension (substring path (1+ (string-rindex path #\.)))))
      (cond [(string= extension "css")  '(text/css)]
            [(string= extension "js")   '(application/javascript)]
            [(string= extension "json") '(application/javascript)]
            [(string= extension "html") '(text/html)]
            [(string= extension "n3")  '(text/plain)]
            [(string= extension "png")  '(image/png)]
            [(string= extension "svg")  '(image/svg+xml)]
            [(string= extension "ico")  '(image/x-icon)]
            [(string= extension "pdf")  '(application/pdf)]
            [(string= extension "ttf")  '(application/font-sfnt)]
            [(#t '(text/plain))])))

  (let ((full-path (string-append (www-root) "/" path)))
    (if (not (file-exists? full-path))
        (respond-to-client 404 client-port '(text/html)
          (with-output-to-string
            (lambda _ (sxml->xml (page-error-404 path)))))
        (let* [(file-stat (stat full-path))
               (bytes     (stat:size file-stat))]
          (write-response
           (build-response
            #:code 200
            #:headers `((content-type . ,(response-content-type full-path))
                        (content-length . ,bytes)))
           client-port)
          (call-with-input-file full-path
            (lambda (input-port)
              (sendfile client-port input-port bytes)))))))

(define* (request-scheme-page-handler request request-path
                                      client-port #:key (username #f)
                                                        (token #f))

  (define (module-path prefix elements)
    "Returns the module path so it can be loaded."
    (append prefix (map string->symbol elements)))

  (define (resolve-module-function request-path)
    "Return FUNCTION from MODULE."
    (let* ((module (if (developer-mode?)
                       (catch 'wrong-type-arg
                         (lambda _
                           (reload-module
                            (resolve-module (module-path '(www pages)
                              (string-split request-path #\/)) #:ensure #f)))
                         (lambda (key . args) #f))
                       (resolve-module (module-path '(www pages)
                         (string-split request-path #\/)) #:ensure #f)))
           (page-symbol (symbol-append 'page-
                         (string->symbol
                          (string-replace-occurrence request-path #\/ #\-)))))
      ;; Return #f unless the 'page-symbol' exists in 'module',
      ;; in which case we return that.
      (if module
          (catch #t
            (lambda _ (module-ref module page-symbol))
            (lambda (key . args) #f))
          #f)))

  (define (resolve-form-module request-path)
    "Return FUNCTION from MODULE."
    (let* ((module (if (developer-mode?)
                       (catch 'wrong-type-arg
                         (lambda _
                           (reload-module
                            (resolve-module (module-path '(www forms)
                              (string-split request-path #\/)) #:ensure #f)))
                         (lambda (key . args) #f))
                       (resolve-module (module-path '(www forms)
                         (string-split request-path #\/)) #:ensure #f))))
      ;; Return #f unless the expected symbols exist in 'module',
      ;; in which case we return that.
      (if module
          (catch #t
            (lambda _ `((page   . ,(module-ref module 'page))
                        (submit . ,(module-ref module 'submit))))
            (lambda (key . args)
              (log-error "resolve-form-module"
                         "Couldn't resolve the module's structure for ~s."
                         request-path)
              #f))
          (begin
            (log-error "resolve-form-module"
                       "Couldn't resolve module for ~s." request-path)
            #f))))

  (define* (submenu-route request-path #:key (post-data #f) (partial-page? #f))
    (let [(hash (basename request-path))]
      (respond-to-client 200 client-port '(text/html)
        (call-with-output-string
          (lambda (port)
            (set-port-encoding! port "utf8")
            (let [(submenu-page (resolve-module-function
                                 (substring (dirname request-path) 1)))]
              (unless partial-page? (format port "<!DOCTYPE html>~%"))
              (sxml->xml
               (submenu-page request-path username hash
                 #:post-data
                 (if (eq? (request-method request) 'POST)
                     (utf8->string (read-request-body request))
                     "")) port)))))))

  ;; Return-type handlers.
  ;; --------------------------------------------------------------------------
  (cond
   ;; If the connection to the back-end cannot be established, show an
   ;; indicative error page.
   [(not (system-is-connected?))
    (if (string-prefix? "/api" request-path)
        (respond-503 client-port (request-accept request)
          (string-append "The API frontend cannot reach the SPARQL "
                         "endpoint that was configured as the "
                         "system-connection."))
        (respond-to-client 503 client-port '(text/html)
          (call-with-output-string
            (lambda (port)
              (set-port-encoding! port "utf8")
              (let* ((page-function (resolve-module-function "connection-failure"))
                     (sxml-tree     (page-function request-path)))
                (catch 'wrong-type-arg
                  (lambda _
                    (when (eq? (car (car sxml-tree)) 'html)
                      (format port "<!DOCTYPE html>~%")))
                  (lambda (key . args) #f))
                (sxml->xml sxml-tree port))))))]

   ;; The “/” page is special, because we re-route it to “dashboard”.
   [(not (string-is-longer-than request-path 2))
    (if username
        (respond-303 client-port "/dashboard" #f)
        (respond-303 client-port "/login" #f))]

   ;; The POST request of the login page is special, because it must set
   ;; a Set-Cookie HTTP header.  This is something out of the control of
   ;; the normal page functions.
   [(and (string-prefix? "/login" request-path)
         (eq? (request-method request) 'POST))
    (let* [(data (post-data->alist (utf8->string (read-request-body request))))
           (session (authenticate-user data))]
      (if session
          ;; Redirect to the “dashboard” page.
          (respond-303 client-port "/dashboard" (session->cookie session))
          (respond-to-client 200 client-port '(text/html)
            (call-with-output-string
              (lambda (port)
                (set-port-encoding! port "utf8")
                (let* ((page-function (resolve-module-function "login"))
                       (sxml-tree (page-function
                                   request-path
                                   #:post-data
                                   "Nope")))
                  (catch 'wrong-type-arg
                    (lambda _
                      (when (eq? (car (car sxml-tree)) 'html)
                        (format port "<!DOCTYPE html>~%")))
                    (lambda (key . args) #f))
                  (sxml->xml sxml-tree port)))))))]

   ;; The regular login page is special because the username
   ;; isn't known at this point.
   [(and (string-prefix? "/login" request-path)
         (eq? (request-method request) 'GET))
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (let* ((page-function (resolve-module-function "login"))
                 (sxml-tree     (page-function request-path)))
            (catch 'wrong-type-arg
              (lambda _
                (when (eq? (car (car sxml-tree)) 'html)
                  (format port "<!DOCTYPE html>~%")))
              (lambda (key . args) #f))
            (sxml->xml sxml-tree port)))))]

   ;; Form functionality
   ;; -------------------------------------------------------------------------
   ;;
   ;; Forms can be loaded outside of the source code of this project.  The
   ;; dynamic loading of Scheme modules has been slightly modified so that
   ;; we can carefully test whether the module implements the expected
   ;; interface.

   [(and (string-prefix? "/form/" request-path)
         (eq? (request-method request) 'GET))
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (let ((form (resolve-form-module (substring request-path 6))))
            (if form
                (sxml->xml ((assoc-ref form 'page) request-path) port)
                (sxml->xml (page-form-error-404 request-path) port))))))]

   [(and (string-prefix? "/form/" request-path)
         (eq? (request-method request) 'POST))
    (let ((form         (resolve-form-module (substring request-path 6)))
          (accept-type  (request-accept request))
          (content-type (request-content-type request)))
      (if form
          (cond
           [(not (api-serveable-format? accept-type))
            (respond-406 client-port)]
           [else
            ((assoc-ref form 'submit)
             (api-request-data->alist
              (request-content-type request)
              (utf8->string (read-request-body request))))])
          (respond-404 client-port accept-type
            "The form could not be found.")))]

   [(string-prefix? "/logout" request-path)
    (respond-303 client-port "/" (string-append
                                  (session-cookie-prefix)
                                  "=deleted; expires=Thu,"
                                  " Jan 01 1970 00:00:00 UTC;"))]

   ;; The API is implemented as purely virtual locations.
   ;; -------------------------------------------------------------------------
   [(and (beacon-enabled?)
         (string-prefix? "/beacon" request-path))
    (request-beacon-handler request request-path client-port
                            #:username username)]

   [(string-prefix? "/api" request-path)
    (request-api-handler request request-path client-port
                         #:username username #:token token)]

   ;; COMPONENT CALLBACKS
   ;; -------------------------------------------------------------------------
   [(string= "/add-connection" request-path)
    (if (eq? (request-method request) 'POST)
        (let* [(post-data   (utf8->string (read-request-body request)))
               (referer     (request-referer request))
               (connections (connections-by-user username))]
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (post-data->alist (uri-decode post-data))))
                    (match alist
                      [(('backend . a) ('name . b) ('password . c) ('uri . d) ('username . e))
                       (connection-add
                        (alist->connection alist) username)]
                      [(('name . a) ('uri . b))
                       (connection-add
                        (alist->connection alist) username)]
                      [else #f]))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f)
          (respond-303 client-port (if (uri? referer) (uri->string referer) "/dashboard") #f))
        (respond-405 client-port '(POST)))]

   [(string= "/remove-project" request-path)
    (if (eq? (request-method request) 'POST)
        (let* [(post-data   (post-data->alist
                             (uri-decode
                              (utf8->string (read-request-body request)))))
               (project-uri (assoc-ref post-data 'name))
               (referer     (request-referer request))]
          (project-remove project-uri username)
          (respond-303 client-port "/dashboard" #f))
        (respond-405 client-port '(POST)))]

   ;; SUBMENU ROUTING
   ;; -------------------------------------------------------------------------
   [(any (lambda (x) x)
         (map (lambda (path) (string-prefix? path request-path))
              '("/automate/"
                "/collect/"
                "/query/"
                "/report/"
                "/structure/"
                "/exploratory/"
                "/prompt/"
                "/import/"
                "/forms/"
                "/create-form/")))
    (submenu-route request-path)]

   [(any (lambda (x) x)
         (map (lambda (path) (string-prefix? path request-path))
              '("/query-history/")))
    (submenu-route request-path #:partial-page? #t)]

   [(string-prefix? "/query-response" request-path)
    (let [(hash               (basename request-path))
          (response-mime-type (if (string-suffix? ".json" request-path)
                                  '(application/javascript)
                                  '(text/html)))]
      (if (string= hash "query-response")
          (respond-400 client-port (request-accept request)
            "The URI needs to be formed as: '/query-response/<project hash>'.")
          (respond-to-client 200 client-port response-mime-type
           ;; TODO: Let query-response stream the results, rather than
           ;; building it up in-memory and send it in a single large response.
           (call-with-output-string
             (lambda (port)
               (let* ((path          "query-response")
                      (page-function (resolve-module-function path)))
                 (when page-function
                   (if (eq? (request-method request) 'POST)
                       ((page-function request-path username hash token
                                       #:post-data (utf8->string
                                                    (read-request-body request))
                                       #:return-type response-mime-type) port)
                       (respond-405 client-port '(POST))))))))))]

   ;; When the “file extension” of the request indicates JSON, treat the
   ;; returned format as ‘application/javascript’.
   [(string-suffix? ".json" request-path)
    (respond-to-client 200 client-port '(application/javascript)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (let* ((request-path (basename request-path ".json"))
                 (page-function (resolve-module-function request-path)))
            (if page-function
                (if (eq? (request-method request) 'POST)
                    (put-string port
                                (page-function
                                 request-path username
                                 #:type 'json
                                 #:post-data (utf8->string
                                              (read-request-body request))))
                    (put-string port (page-function request-path username
                                                    #:type 'json)))
                (put-string port "[]"))))))]

   ;; When the URI begins with “/edit-connection/”, use the edit-connection
   ;; page.
   [(string-prefix? "/edit-connection" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (if (eq? (request-method request) 'POST)
                         (page-edit-connection request-path username
                           #:post-data (utf8->string
                                        (read-request-body request)))
                         (page-edit-connection request-path username))
                     port))))]

   ;; When the URI begins with “/project-details/”, use the project-details
   ;; page.
   [(string-prefix? "/project-details/" request-path)
    (catch #t
      (lambda _
        (let* [(hash    (last (string-split request-path #\/)))
               (project (project-by-hash hash))]
          (if (project-has-member? (project-id project) username)
              (respond-to-client 200 client-port '(text/html)
                (call-with-output-string
                 (lambda (port)
                   (set-port-encoding! port "utf8")
                   (format port "<!DOCTYPE html>~%")
                   (sxml->xml (if (eq? (request-method request) 'POST)
                                  (page-project-details request-path username
                                    #:post-data (utf8->string
                                                 (read-request-body request)))
                                  (page-project-details request-path username))
                              port))))
              (throw 'no-access ""))))
      (lambda (key . args)
        (log-error "handle-request" "While handling project-details: ~a: ~a"
                   key args)
        (if (equal? key 'no-access)
            (respond-to-client 403 client-port '(text/html)
              (with-output-to-string
                (lambda _ (sxml->xml (page-error-403 request-path)))))
            (respond-to-client 404 client-port '(text/html)
              (with-output-to-string
                (lambda _ (sxml->xml (page-error-404 request-path))))))))]

   ;; For “/query-history-clear”, we must call a database function and
   ;; redirect to “/query”.
   [(string-prefix? "/query-history-clear" request-path)
    (let [(hash (basename request-path))]
      (if (string= hash "query-history-clear")
          (respond-400 client-port (request-accept request)
            (string-append
             "The URI needs to be formed as: "
             "'/query-history-clear/<project hash>'."))
          (begin
            (query-remove-unmarked-for-project username
              (project-id (project-by-hash hash)))
            (respond-303 client-port (string-append "/query/" hash) #f))))]

   ;; All other requests can be handled as HTML responses.
   [#t
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (let* ((path          (substring request-path 1))
                 (page-function (resolve-module-function path))
                 (sxml-tree     (if page-function
                                    (if (eq? (request-method request) 'POST)
                                        (page-function request-path username
                                                       #:post-data
                                                       (utf8->string
                                                        (read-request-body request)))
                                        (page-function request-path username))
                                    (page-error request-path))))
            (catch 'wrong-type-arg
              (lambda _
                (when (eq? (car (car sxml-tree)) 'html)
                  (format port "<!DOCTYPE html>~%")))
              (lambda (key . args) #f))
            (sxml->xml sxml-tree port)))))]))


;; ----------------------------------------------------------------------------
;; ROUTING & HANDLERS
;; ----------------------------------------------------------------------------
;;
;; Requests can have different handlers.
;; * Static objects (images, stylesheet, javascript files) have their own
;;   handler.
;; * The 'regular' Scheme pages have their own handler that resolves the
;;   module dynamically.
;;
;; Feel free to add your own handler whenever that is necessary.
;;

(define (request-handler client-port)
  (let* [(request      (read-request client-port))
         (request-path (uri-path (request-uri request)))
         (headers      (request-headers request))]
    ;; There can be multiple cookies on the top-level domain, so we have
    ;; to pick the right one.
    (let* ((cookies-str (assoc-ref headers 'cookie))
           (cookies (if (string? cookies-str)
                        (delete #f (map (lambda (cookie)
                                          (if (string-prefix?
                                               (session-cookie-prefix) cookie)
                                              cookie #f))
                                        (map string-trim-both
                                             (string-split cookies-str #\;))))
                        #f))
           (cookie (if (and (list? cookies) (not (null? cookies)))
                      (car cookies)
                      #f))
           (session (if cookie (cookie->session cookie) #f)))
      (cond
       ;; Static resources are served using the ‘request-file-handler’.
       ;; ----------------------------------------------------------------------
       [(string-prefix? "/static/" request-path)
        (request-file-handler request-path client-port)]

       ;; Authentication is required for almost all pages.
       ;; ----------------------------------------------------------------------
       [session
        (let ((username (session-username session)))
          (log-access username request-path)
          (catch #t
            (lambda _
              (request-scheme-page-handler
               request request-path client-port #:username username
               #:token (session-token session)))
            (lambda (key . args)
              (log-error "request-handler" "1: ~a: ~a: ~s"
                         request-path key args))))]

       ;; The following pages may be accessed without logging in.
       ;; ----------------------------------------------------------------------
       [(or (string= "/login" request-path)
            (and (beacon-enabled?) (string-prefix? "/beacon" request-path))
            (string-prefix? "/form/" request-path)
            (string= "/api" request-path)
            (string= "/api/login" request-path)
            (string= "/api/register-connection" request-path))
        (catch #t
          (lambda _
            (request-scheme-page-handler request request-path client-port))
          (lambda (key . args)
            (log-error "request-handler" "2: ~a: ~s" key args)))]

       ;; When not authenticated, redirect to the login page.
       ;; ----------------------------------------------------------------------
       [(string= "/" request-path)
        (respond-303 client-port "/login" #f)]

       [(string-prefix? "/api" request-path)
        (if (request-accept request)
            (respond-401 client-port (request-accept request) "Please log in.")
            (respond-406 client-port))]

       [else
        (respond-303 client-port "/login" #f)]))))

(define (health-maintainer)
  (while #t
    (let ((system-connections (load-system-wide-connections)))
      (for-each (lambda (record)
                  (if (connection-is-online? record)
                      ;; Connection is online
                      ;; ------------------------------------------------------
                      (when (connection-down-since record)
                        (log-debug "health-maintainer"
                                   "Connection ~s is back online."
                                   (connection-name record))
                        (set-connection-down-since! record #f)
                        (persist-system-wide-connections system-connections))

                      ;; Connection is offline
                      ;; ------------------------------------------------------
                      (if (number? (connection-down-since record))

                          ;; Connection was already offline.
                          ;; --------------------------------------------------
                          (let [(downtime (- (current-time)
                                             (connection-down-since record)))
                                (name     (connection-name record))]
                            (log-debug "health-maintainer"
                                       "Connection ~s is offline for ~a seconds."
                                       name downtime)

                            (if (and (> downtime 29)
                                     (remove-system-wide-connection record))

                                ;; Connection is down for >30 seconds
                                ;; --------------------------------------------
                                (log-debug "health-maintainer"
                                           "Connection ~s has been removed." name)
                                #f))

                          ;; Connection is newly offline
                          ;; --------------------------------------------
                          (let ((timestamp (current-time)))
                            (log-debug "health-maintainer"
                                       "Connection ~s down since: ~a"
                                       (connection-name record)
                                       timestamp)
                            (set-connection-down-since! record timestamp)
                            (persist-system-wide-connections system-connections)))))
                system-connections))
    (sleep 10)))

(define (start-server request-handler)

  ;; Start a background thread to maintain a healthy system.
  (call-with-new-thread health-maintainer)

  ;; Listen to HTTP requests for user interaction.
  (let* ((family (www-listen-address-family))
         (s (socket family SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)

    (bind s family (if (string? (www-listen-address))
                       (inet-pton family (www-listen-address))
                       (www-listen-address))
          (www-listen-port))

    (listen s 50)
    (while #t
      (let* [(client-connection (accept s))
             ;(client-details    (cdr client-connection))
             (client-port       (car client-connection))]

        ;; Each request is handled in a separate thread.
        (call-with-new-thread
         (lambda _
           (request-handler client-port)
           (close client-port))
         (lambda (key . args)
           (close client-port)))))))
