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

(define-module (auth-manager permission-check)
  #:use-module (auth-manager config)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (sparql parser)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (logger)

  #:export (has-unscoped-variables?
            may-execute?))

(define (graphs-by-project auth-token project-hash)
  "Returns a list of graphs part of PROJECT-HASH that may be accessed using 
AUTH-TOKEN."
  (let ((uri (string-append (sg-web-uri) "/api/graphs-by-project")))
    (receive (header port)
        (http-post uri
                   #:headers `((user-agent   . ,%user-agent)
                               (Cookie       . ,auth-token)
                               (content-type . (application/s-expression))
                               (accept       . ((application/s-expression))))
                   #:body    (call-with-output-string
                               (lambda (port)
                                 (write `((project-hash . ,project-hash)
                                          (connection   . ,(www-name))) port)))
                   #:streaming? #t)
      (if (= (response-code header) 200)
          (read port)
          (begin
            (log-debug "graphs-by-project" "Reading graphs-by-project failed.")
            '())))))

(define (inferred-graphs query)
  "Returns a list of graph names that are used in the query."
  (append (query-global-graphs query)
          (delete #f (map car (query-quads query)))))

(define (has-unscoped-variables? query)
  "Returns #t when there is a triplet pattern without an explicit graph,
otherwise it returns #f."
  (any not (map car (query-quads query))))

(define (may-execute? auth-token project-hash query)
  "Returns #t when the query may be executed, #f otherwise."
  (let [(parsed (parse-query query))]

    ;; The parser must be able to parse the query.
    ;; -----------------------------------------------------------------------
    (if (not parsed)
        (values #f (format #f "Couldn't parse:~%~a" query))
        (let* [(allowed-graphs (map (lambda (item) (assoc-ref item "graph"))
                                    (graphs-by-project auth-token
                                                       project-hash)))
               (global-graphs  (query-global-graphs parsed))
               (disallowed-graphs (lset-difference string= global-graphs
                                                   allowed-graphs))
               (used-graphs    (inferred-graphs parsed))]
          (cond
           ;; Check whether all variables are scoped in a graph.
           ;; -----------------------------------------------------------------
           [(or  (and (has-unscoped-variables? parsed)
                      (null? global-graphs))
                 (and (has-unscoped-variables? parsed)
                      (not (null? disallowed-graphs))))
            (if (not (null? disallowed-graphs))
                (values #f (format #f "Disallowed graphs:~{~%-> ~a~}"
                                   disallowed-graphs))
                (values #f
                 (string-append
                  "Specify the graph to search for the following triplets:"
                  (format #f "~{~%-> ~a~}"
                    (delete #f (map (lambda (quad)
                                      (if (car quad) #f
                                          (format #f "~a ~a ~a"
                                                  (list-ref quad 1)
                                                  (list-ref quad 2)
                                                  (list-ref quad 3))))
                                    (query-quads parsed)))))))]

           ;; Check whether only allowed-graphs are used.
           ;; -----------------------------------------------------------------
           [(not (null? (lset-difference string= used-graphs allowed-graphs)))
            (let* [(g (delete-duplicates
                       (lset-difference string= used-graphs allowed-graphs)))]
              (values #f (format #f "Disallowed graphs:~{~%-> ~a~}" g)))]

           ;; If all previous tests passed, the query may be executed.
           ;; -----------------------------------------------------------------
           [else
            (log-debug "may-execute?" "Approved:~%---~%~a~%---" query)
            (values #t "")])))))
