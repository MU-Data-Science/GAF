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

(define-module (www requests-api)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (ldap authenticate)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (sparql stream)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www config-reader)
  #:use-module (www db api)
  #:use-module (www db connections)
  #:use-module (www db exploratory)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (www db sessions)
  #:use-module (www db prompt)
  #:use-module (www util)
  #:use-module (web http)

  #:export (request-api-handler
            authenticate-user))

(define (authenticate-user data)
  "This function returns a user session on success or #f on failure."
  (if (and (assoc-ref data 'username)
           (assoc-ref data 'password))
      (if (or (and (ldap-enabled?)
                   (may-access?
                    (ldap-uri) (ldap-common-name) (ldap-organizational-unit)
                    (ldap-domain)
                    (assoc-ref data 'username)
                    (assoc-ref data 'password)))
              (and (not (ldap-enabled?))
                   (not (null? (local-users)))
                   (any (lambda (x) x)
                        (map (lambda (user)
                               (and (string= (car user) (assoc-ref data 'username))
                                    (string= (cadr user) (string->sha256sum
                                                          (assoc-ref data 'password)))))
                             (local-users)))))
          (let [(session (session-by-name (assoc-ref data 'username) "Web"))]
            (if session
                session
                (let ((session (alist->session
                                `((name     . "Web")
                                  (username . ,(assoc-ref data 'username))
                                  (token    . "")))))
                  (session-add session)
                  session)))
          (begin
            (log-access (assoc-ref data 'username) "Failed login attempt.")
            #f))
      (begin
        (log-error "authenticate-user"
                   "Either the username or the password property is missing.")
        #f)))

(define* (request-api-handler request request-path client-port
                              #:key (username #f) (token #f))

  ;; The following function can be used to read the request data
  ;; and transform it to an association list.
  (define (entire-request-data request)
    (api-request-data->alist
     (request-content-type request)
     (utf8->string (read-request-body request))))

  (let [(accept-type  (request-accept request))
        (content-type (request-content-type request))]
    (cond

     ;; The remainder of API calls is expected to return data.  For these
     ;; calls, the format in which to send data is important, and therefore
     ;; when the client does not provide an 'Accept' header, or does not
     ;; request a supported format, we do not need to process the API call
     ;; further, because the response will always be a 406.
     [(not (api-serveable-format? accept-type))
      (respond-406 client-port)]

     [(string= "/api" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       `((name     . "SPARQLing-genomics API")
                         (version  . ,(www-version))
                         (homepage . "https://www.sparqling-genomics.org/")))
          (respond-405 client-port '(GET)))]

     ;; LOGIN
     ;; ---------------------------------------------------------------------
     [(string= "/api/login" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data    (entire-request-data request))
                 (session (authenticate-user data))]
            (if session
                (respond-200 client-port accept-type
                             `((cookie . ,(string-append
                                           (session-cookie-prefix) "="
                                           (session-token session)))))
                (respond-401 client-port accept-type
                             "Invalid username or password.")))
          (respond-405 client-port '(POST)))]

     ;; GRAPHS-BY-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/graphs-by-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data      (entire-request-data request))
                 (hash      (assoc-ref data 'project-hash))
                 ;; The connection-name is optional.
                 (name      (assoc-ref data 'connection))]
            (if hash
                (respond-200 client-port accept-type
                  (all-graphs-in-project username name hash))
                (respond-400 client-port accept-type
                             "Missing 'project-hash' parameter.")))
          (respond-405 client-port '(POST)))]

     ;; ROOT-TYPES-BY-GRAPH
     ;; ---------------------------------------------------------------------
     [(string= "/api/root-types-by-graph" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data         (entire-request-data request))
                 (conn-name    (assoc-ref data 'connection))
                 (connection   (connection-by-name conn-name username))
                 (project-hash (assoc-ref data 'project-hash))
                 (graph        (assoc-ref data 'graph))]
            (cond
             [(not conn-name)
              (respond-400 client-port accept-type
                           "Missing 'connection' parameter.")]
             [(not graph)
              (respond-400 client-port accept-type
                           "Missing 'graph' parameter.")]
             [(not project-hash)
              (respond-400 client-port accept-type
                           "Missing 'project-hash' parameter.")]
             [else
              (respond-200 client-port accept-type
                           (hierarchical-tree-roots connection graph token
                                                    project-hash))]))
          (respond-405 client-port '(POST)))]

     ;; CHILDREN-BY-TYPE
     ;; ---------------------------------------------------------------------
     [(string= "/api/children-by-type" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data         (entire-request-data request))
                 (conn-name    (assoc-ref data 'connection))
                 (connection   (connection-by-name conn-name username))
                 (project-hash (assoc-ref data 'project-hash))
                 (graph        (assoc-ref data 'graph))
                 (rdf-type     (assoc-ref data 'type))]
            (respond-200 client-port accept-type
              (hierarchical-tree-children
               connection token project-hash graph rdf-type)))
          (respond-405 client-port '(POST)))]

     ;; PREDICATES-BY-TYPE
     ;; ---------------------------------------------------------------------
     [(string= "/api/predicates-by-type" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data         (entire-request-data request))
                 (conn-name    (assoc-ref data 'connection))
                 (connection   (connection-by-name conn-name username))
                 (project-hash (assoc-ref data 'project-hash))
                 (graph        (assoc-ref data 'graph))
                 (rdf-type     (assoc-ref data 'type))]
            (respond-200 client-port accept-type
              (all-predicates username connection project-hash token
                              #:graph graph #:type rdf-type)))
          (respond-405 client-port '(POST)))]

     ;; KNOWN-TYPES
     ;; ---------------------------------------------------------------------
     [(string= "/api/known-types" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data         (entire-request-data request))
                 (conn-name    (assoc-ref data 'connection))
                 (connection   (connection-by-name conn-name username))
                 (project-hash (assoc-ref data 'project-hash))
                 (graph        (assoc-ref data 'graph))]
            (if (not project-hash)
                (respond-400 client-port accept-type
                             "Missing the 'project-hash' parameter.")
                (respond-200 client-port accept-type
                             (all-types username connection
                                        token project-hash))))
          (respond-405 client-port '(POST)))]

     ;; ASSIGN-GRAPH
     ;; ---------------------------------------------------------------------
     [(string= "/api/assign-graph" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data            (entire-request-data request))
                 (project-uri     (assoc-ref data 'project-uri))
                 (connection-name (assoc-ref data 'connection))
                 (graph-uri       (assoc-ref data 'graph-uri))]
            (if (project-has-member? project-uri username)
                (if (project-assign-graph!
                     project-uri graph-uri connection-name username)
                    (respond-201 client-port)
                    (respond-500 client-port accept-type "Not OK"))
                (respond-401 client-port accept-type "Not allowed.")))
          (respond-405 client-port '(POST)))]

     ;; UNASSIGN-GRAPH
     ;; ---------------------------------------------------------------------
     [(string= "/api/unassign-graph" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (project-uri (assoc-ref data 'project-uri))
                 (graph-uri   (assoc-ref data 'graph-uri))]
            (if (project-has-member? project-uri username)
                (if (project-forget-graph! project-uri graph-uri)
                    (respond-204 client-port)
                    (respond-500 client-port accept-type "Not OK"))
                (respond-401 client-port accept-type "Not allowed.")))
          (respond-405 client-port '(POST)))]

     ;; ADD-FORM
     ;; ---------------------------------------------------------------------
     [(string= "/api/add-form" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data      (entire-request-data request))
                 (title     (assoc-ref data 'title))
                 (form-id   (if title (generate-id "form-" title) #f))]
            (if form-id
                (respond-200 client-port accept-type `((form-id . ,form-id)))
                (respond-400 client-port accept-type
                             "Missing 'form-id' parameter.")))
          (respond-405 client-port '(POST)))]

     ;; SUBMIT-FORM
     ;; ---------------------------------------------------------------------
     [(string= "/api/submit-form" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (form-id     (assoc-ref data 'form-id))]
            (if form-id
                (begin
                  #t)
                (respond-400 client-port accept-type
                             "Missing 'form-id' parameter.")))
          (respond-405 client-port '(POST)))]

     ;; PROJECTS
     ;; ---------------------------------------------------------------------
     ;;

     ;; PROJECTS
     ;; ---------------------------------------------------------------------
     [(string= "/api/projects" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type (projects-by-user username))
          (respond-405 client-port '(GET)))]

     ;; ADD-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/add-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (name        (assoc-ref data 'name))]
            (receive (state message project-id)
                (project-add name username)
              (if state
                  (respond-200 client-port accept-type
                               `((project-id . ,project-id)))
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; REMOVE-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/remove-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (uri   (assoc-ref data 'project-uri))
                 (hash  (assoc-ref data 'project-hash))
                 (id    (if uri uri (project-id (project-by-hash hash))))]
             (if (project-has-member? id username)
                (receive (state message)
                    (project-remove id username)
                  (if state
                      (respond-204 client-port)
                      (respond-403 client-port accept-type message)))
                (respond-403 client-port accept-type
                             "You are not a member of this project.")))
          (respond-405 client-port '(POST)))]


     ;; QUERY INTERFACE
     ;; ---------------------------------------------------------------------
     ;;

     ;; QUERIES
     ;; ---------------------------------------------------------------------
     [(string= "/api/queries" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type (queries-by-username username))
          (respond-405 client-port '(GET)))]

     [(string= "/api/query" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data       (entire-request-data request))
                 (query      (assoc-ref data 'query))
                 (conn-name  (assoc-ref data 'connection))
                 (connection (connection-by-name conn-name username))
                 (hash       (assoc-ref data 'project-hash))
                 (project    (project-by-hash hash))
                 (id         (project-id project))
                 (start-time (current-time))]
            (if (system-wide-connection? connection)

                ;; Act as a proxy via sg-auth-manager.
                ;; ------------------------------------------------------------
                (receive (header port)
                    (http-post (string-append (connection-uri connection)
                                              "/api/query/" hash)
                     #:headers `((accept       . ,accept-type)
                                 (content-type . (application/s-expression)))
                     #:streaming? #t
                     #:body
                     `(()))
                  (cond
                   [(= (response-code header) 200)
                    (let* ((end-time   (current-time))
                           (buffer-size (expt 2 16)))
                      (query-add query conn-name username start-time end-time id)
                      ;; On cool platforms, ‘sendfile’ uses the equally named
                      ;; libc procedure.
                      (if (eq? (response-content-encoding header) '(chunked))
                          (let ((wrapped-port (make-chunked-input-port port)))
                            (while (> (sendfile client-port
                                                wrapped-port
                                                buffer-size) 0)))
                          (while (> (sendfile client-port port buffer-size) 0))))]
                   [(= (response-code header) 401)
                    (respond-401 client-port accept-type "Authentication failed.")]
                   [else
                    (respond-401 client-port accept-type (get-string-all port))]))

                ;; Directly query user connections.
                ;; ------------------------------------------------------------
                (receive (header port)
                    (sparql-query-with-connection connection query token hash)
                  (cond
                   [(= (response-code header) 200)
                    (let* ((end-time   (current-time)))
                      (query-add query conn-name username start-time end-time id)
                      (csv-stream port client-port accept-type))]
                   [(= (response-code header) 401)
                    (respond-401 client-port accept-type "Authentication failed.")]
                   [else
                    (respond-401 client-port accept-type (get-string-all port))]))))
          (respond-405 client-port '(POST)))]

     ;; QUERY-MARK
     ;; ---------------------------------------------------------------------
     [(string= "/api/query-mark" request-path)
      (if (eq? (request-method request) 'POST)
          (let* ((data       (entire-request-data request))
                 (state       (assoc-ref data 'state))
                 (query-id    (assoc-ref data 'query-id)))
            (if (set-query-marked! query-id state)
                (respond-200 client-port accept-type
                             `((state    . ,state)
                               (query-id . ,query-id)))
                (respond-500 client-port accept-type
                             `((message . "An unknown error occurred.")))))
          (respond-405 client-port '(POST)))]

     ;; QUERIES-REMOVE-UNMARKED
     ;; ---------------------------------------------------------------------
     [(string= "/api/queries-remove-unmarked" request-path)
      (if (eq? (request-method request) 'POST)
          (let* ((data       (entire-request-data request))
                 (uri        (assoc-ref data 'project-uri))
                 (project     (project-by-id uri))
                 (id          (project-id project)))
            (receive (status message)
                (query-remove-unmarked-for-project username id)
              (if status
                  (respond-204 client-port)
                  (respond-500 client-port accept-type
                   `(message . ,message)))))
          (respond-405 client-port '(POST)))]

     ;; CONNECTIONS
     ;; ---------------------------------------------------------------------
     [(string= "/api/connections" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       (map connection->alist-safe
                            (connections-by-user username)))
          (respond-405 client-port '(GET)))]

     [(string= "/api/connection-by-name" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data       (entire-request-data request))
                 (conn-name  (assoc-ref data 'name))
                 (connection (connection-by-name conn-name username))]
            (if connection
                (respond-200 client-port accept-type
                             (connection->alist-safe connection))
                (respond-400 client-port accept-type "No such connection.")))
          (respond-405 client-port '(POST)))]

     ;; DEFAULT-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/default-connection" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       (connection->alist-safe (default-connection username)))
          (respond-405 client-port '(GET)))]

     [(string= "/api/set-default-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (name      (assoc-ref data 'name))
                 (record    (connection-by-name name username))]
            (connection-set-as-default! record username)
            (respond-204 client-port))
          (respond-405 client-port '(POST)))]

     ;; ADD-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/add-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (record      (alist->connection data))]
            (receive (state message)
                (connection-add record username)
              (if state
                  (respond-201 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; REGISTER-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/register-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (record      (alist->system-wide-connection data))]
            (receive (state message)
                (connection-add record username)
              (if state
                  (respond-201 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; REMOVE-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/remove-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (name        (assoc-ref data 'name))]
            (receive (state message)
                (remove-user-connection name username)
              (if state
                  (respond-204 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; PROMPTS
     ;; ---------------------------------------------------------------------
     ;;
     ;; Prompts are designed to interactively add triplets.  It's the basis
     ;; of the “prompt” functionality in the web interface.

     [(string= "/api/prompts" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type (prompts-by-user username))
          (respond-405 client-port '(GET)))]

     [(string= "/api/prompt-start" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (tag        (assoc-ref data 'tag))
                 (prompt-id  (prompt-start username #:tag tag))]
            (if prompt-id
                (respond-200 client-port accept-type `((prompt-id . ,prompt-id)))
                (respond-500 client-port accept-type "Couldn't create prompt.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/prompt-with-tag" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data       (entire-request-data request))
                 (tag        (assoc-ref data 'tag))
                 (prompt-id  (prompt-with-tag tag username))]
            (if prompt-id
                (respond-200 client-port accept-type `((prompt-id . ,prompt-id)))
                (respond-404 client-port accept-type
                             "No prompt with the specified tag was found.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/prompt-commit" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data         (entire-request-data request))
                 (prompt-id    (assoc-ref data 'prompt-id))
                 (graph        (assoc-ref data 'graph))
                 (project-hash (assoc-ref data 'project-hash))
                 (state       (prompt-commit prompt-id graph username
                                             token project-hash))]
            (if state
                (respond-204 client-port)
                (respond-500 client-port accept-type "Couldn't commit prompt.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/prompt-delete" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (prompt-id   (assoc-ref data 'prompt-id))
                 (state       (prompt-delete prompt-id username))]
            (if state
                (respond-204 client-port)
                (respond-500 client-port accept-type "Couldn't remove prompt.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/prompt-add-triplet" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (prompt-id   (assoc-ref data 'prompt-id))
                 (subject     (assoc-ref data 'subject))
                 (predicate   (assoc-ref data 'predicate))
                 (object      (assoc-ref data 'object))
                 (state       (prompt-add-triplet
                               prompt-id username subject predicate object))]
            (if state
                (respond-204 client-port)
                (respond-500 client-port accept-type "Couldn't add triplet.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/prompt-remove-triplet" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (triplet-id  (assoc-ref data 'triplet-id))
                 (state       (prompt-delete-triplet triplet-id username))]
            (if state
                (respond-204 client-port)
                (respond-500 client-port accept-type "Couldn't remove triplet.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/user-info" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type `((username . ,username)))
          (respond-405 client-port '(GET)))]

     [(string= "/api/new-session-token" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (entire-request-data request))
                 (name        (assoc-ref data 'session-name))
                 (session     (alist->session
                               `((name     . ,name)
                                 (username . ,username)
                                 (token    . ""))))]
            (if (and session
                     (session-add session))
                (respond-200 client-port accept-type
                             `((token . ,(session-token session))))
                (respond-500 client-port accept-type
                             "Couldn't create session token.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/remove-session" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data    (entire-request-data request))
                 (token   (assoc-ref data 'token))
                 (session (session-by-token token))]
            (if (and session (session-remove token))
                (respond-200 client-port accept-type
                             `((message . "Session has been removed.")))
                (respond-500 client-port accept-type
                             "Couldn't delete session.")))
          (respond-405 client-port '(POST)))]

     [(string= "/api/reload-configuration" request-path)
      (if (eq? (request-method request) 'POST)
          (begin
            (if (read-configuration-from-file (configuration-file))
                (log-error "requests-api"
                           "Reloaded configuration file")
                (log-error "requests-api"
                           "Failed to reload configuration file."))
            (respond-200 client-port accept-type `((message . "Done."))))
          (respond-405 client-port '(POST)))]

     ;; This API call shuts down the sg-web service.  It is only available
     ;; when developer-mode is enabled.
     [(and (developer-mode?)
           (string= "/api/shutdown" request-path))
      (if (eq? (request-method request) 'POST)
          (begin
            (log-error "requests-api" "Shutting down.")
            (exit #t))
          (respond-405 client-port '(POST)))]

     [else
      (respond-404 client-port accept-type "This method does not exist.")])))
