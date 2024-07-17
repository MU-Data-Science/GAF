;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (auth-manager api)
  #:use-module (auth-manager config)
  #:use-module (auth-manager permission-check)
  #:use-module (auth-manager proxy)
  #:use-module (auth-manager virtuoso)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (sparql driver)
  #:use-module (sparql stream)
  #:use-module (srfi srfi-19)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (www db api)
  #:use-module ((www db sessions) #:select (session-cookie-prefix))
  #:use-module (www util)
  #:use-module (system foreign)
  #:use-module (web http)

  #:export (start-server
            request-handler))

(define system-load-average
  (let [(getloadavg-from-c (pointer->procedure int
                            (dynamic-func "getloadavg" (dynamic-link))
                            (list '* int)))]
    (lambda ()
      (let* [(averages (bytevector->pointer
                        (make-bytevector (sizeof double))))
             (success  (getloadavg-from-c averages 1))]
        (if (>= success 0)
            (bytevector-ieee-double-ref
             (pointer->bytevector
              averages (sizeof double)) 0 (native-endianness))
            -1)))))

(define* (api-handler request request-path client-port
                      #:key (username #f) (token #f))
  "Responds to API calls."
  (let [(accept-type  (request-accept request))
        (content-type (request-content-type request))]
    (cond
     [(string= "/api" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
           `((name     . "SPARQLing-genomics Authentication Manager API")
             (homepage . "https://www.sparqling-genomics.org/")))
          (respond-405 client-port '(GET)))]

     [(string= "/api/status" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       `((load-average   . ,(system-load-average))
                         (available-cpus . ,(current-processor-count))))
          (respond-405 client-port '(GET)))]

     ;; So we expect all parameters to be sent as url-encoded data,
     ;; and the actual file contents as content of the POST request.  The
     ;; Content-Type should match the actual file content's mime type.
     [(string-prefix? "/api/import-rdf" request-path)
      (if (eq? (request-method request) 'POST)
          (let [(index  (string-index request-path #\?))]
            (if index
                (let* [(metadata      (post-data->alist
                                       (substring request-path (1+ index))))
                       (graph-uri     (assoc-ref metadata 'graph))
                       (wait-for-more (assoc-ref metadata 'wait-for-more))
                       (upload-dir     (string-append
                                        (www-upload-root) "/" username))
                       (output-port    (begin
                                         (mkdir-p upload-dir)
                                         (mkstemp! (string-append
                                                    upload-dir "/XXXXXX"))))
                       (tmp-filename   (port-filename output-port))
                       (input-port     (request-port request))
                       (bytes-to-fetch (request-content-length request))
                       (bytes-fetched  (sendfile output-port input-port
                                                 bytes-to-fetch))]
                  (close-port output-port)
                  (if (= bytes-fetched bytes-to-fetch)
                      (cond
                       [(equal? (rdf-store-backend) 'virtuoso)
                        (if (not (stage-file tmp-filename graph-uri))
                            (respond-500 client-port accept-type
                                         "Cannot stage the file.")
                            (if wait-for-more
                                (respond-202 client-port)
                                (if (start-bulk-load)
                                    (respond-200 client-port accept-type
                                     `((message . "The file has been imported."))))))]
                       [else
                        (respond-500 client-port accept-type
                         `((message . "The sg-auth-manager has been misconfigured.")))])
                      (respond-500 client-port accept-type
                                   (format #f "Received ~a of ~a bytes."
                                           bytes-fetched
                                           bytes-to-fetch))))
                (respond-400 client-port accept-type
                             "Missing 'graph' parameter.")))
          (respond-405 client-port '(POST)))]

     [(or (string-prefix? "/sparql" request-path)
          (string-prefix? "/api/query" request-path))
      (if (eq? (request-method request) 'POST)
          (let [(index  (string-index request-path #\?))]
            (if index
                (let* [(metadata (post-data->alist
                                  (substring request-path (1+ index))))
                       (hash     (assoc-ref metadata 'project-hash))
                       (query    (utf8->string (read-request-body request)))]
                  (call-with-values
                      (lambda _ (may-execute? token hash query))
                    (lambda (answer message)
                      (if answer
                          (call-with-values
                            (lambda _
                              (sparql-query query
                                #:store-backend (rdf-store-backend)
                                #:uri (rdf-store-uri)
                                #:digest-auth
                                (if (and (rdf-store-username)
                                         (rdf-store-password))
                                    (string-append
                                     (rdf-store-username) ":"
                                     (rdf-store-password))
                                    #f)))
                            (lambda (header port)
                              (cond
                               [(= (response-code header) 200)
                                (csv-stream port client-port accept-type)]
                               [(= (response-code header) 401)
                                (respond-401 client-port accept-type
                                             "Authentication failed.")]
                               [else
                                (respond-401 client-port accept-type
                                             (get-string-all port))])))
                          (begin
                            (log-access username "/api/query"
                                        "Denied query request.")
                            (respond-401 client-port accept-type message))))))
                (respond-400 client-port accept-type
                             "Missing 'project-hash' parameter")))
          (respond-405 client-port '(POST)))]

     [else
      (respond-404 client-port accept-type
                   "This method does not exist.")])))

(define (request-handler client-port)
  "The gateway function to the actual API handler."
  (let* [(request            (read-request client-port))
         (request-path       (uri->string (request-uri request)))
         (parameters         (string-contains request-path "?token="))
         (path-wo-parameters (if parameters
                                 (substring request-path 0 parameters)
                                 request-path))
         (accept-type        (request-accept request))
         (headers            (request-headers request))
         (biscuit            (assoc-ref headers 'cookie))]

    ;; There can be multiple cookies on the top-level domain, so we have
    ;; to pick the right one.
    (let* [(cookies-str (cond
                         [biscuit    biscuit]
                         [parameters (string-append
                                      (session-cookie-prefix) "="
                                      (substring request-path
                                                 (+ 7 parameters)))]
                         [else       #f]))
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
           (username (token->user cookie))]
      (cond
       ;; Deal with unserveable requests.
       [(not (api-serveable-format? accept-type))
        (log-debug "request-handler" "Not a serveable format: ~a" accept-type)
        (respond-406 client-port)]
       ;; Only proceed when the sg-web instance approves.
       [username
        (log-access username path-wo-parameters)
        (api-handler request path-wo-parameters client-port
                     #:username username
                     #:token cookie)]
       [else
        (respond-401 client-port (request-accept request)
                     "Please log in.")]))))

(define (start-server request-handler)
  (let* [(family (www-listen-address-family))
         (s      (socket family SOCK_STREAM 0))]
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s family (if (string? (www-listen-address))
                       (inet-pton family (www-listen-address))
                       (www-listen-address))
          (www-listen-port))
    (listen s 50)
    (while #t
      (let* [(client-connection (accept s))
             (client-details    (cdr client-connection))
             (client-port       (car client-connection))]

        ;; Each request is handled in a separate thread.
        (call-with-new-thread
         (lambda _
           (request-handler client-port)
           (close client-port))
         (lambda (key . args)
           (close client-port)))))))
