;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www db queries)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (rnrs io ports)
  #:use-module (web response)
  #:use-module (sxml simple)

  #:export (query-add
            query-remove
            query-remove-unmarked-for-project
            all-queries
            query-by-id
            queries-by-username
            queries-by-project

            persist-query
            query-id
            query-username
            query-content
            query-endpoint
            query-project
            query-execution-time
            query-start-time
            query-end-time
            query-marked?
            query?

            set-query-endpoint!
            set-query-start-time!
            set-query-end-time!
            set-query-content!
            set-query-marked!))


;; PUBLIC INTERFACE
;; ----------------------------------------------------------------------------
;;
;; The following macros implement the same interface as its SRFI-9 record-type
;; equivalent.  The implementation difference is that this interface operates
;; directly on the triple store, rather than the Scheme state.
;;

(define-syntax-rule (query-id query)             (assoc-ref query "queryId"))
(define-syntax-rule (query-content query)        (assoc-ref query "queryText"))
(define-syntax-rule (query-endpoint query)       (assoc-ref query "executedAt"))
(define-syntax-rule (query-username query)       (assoc-ref query "executedBy"))
(define-syntax-rule (query-start-time query)     (assoc-ref query "startTime"))
(define-syntax-rule (query-end-time query)       (assoc-ref query "endTime"))
(define-syntax-rule (query-execution-time query) (assoc-ref query "executionTime"))
(define-syntax-rule (query-project query)        (assoc-ref query "isRelevantTo"))
(define (query-marked? query)
  (string= (assoc-ref query "isProtected") "1"))

(define (set-query-property! query-id predicate object type)
  (let [(query (string-append
                internal-prefixes
                "WITH <" system-state-graph ">
DELETE { ?query " predicate " ?value . }
INSERT { ?query " predicate " " (if type
                                    (if (string= type "xsd:boolean")
                                        (if object "1" "0")
                                        (format #f "~s^^~a" object type))
                                    (format #f "<~a>" object)) " . }
WHERE  { ?query ?predicate ?value . FILTER (?query = <" query-id ">) }"))
        (connection (system-connection))]
    (receive (header body)
        (system-sparql-query query)
      (= (response-code header) 200))))

(define-syntax-rule
  (set-query-content! query value)
  (set-query-property! query "sg:queryText" value "xsd:string"))

(define-syntax-rule
  (set-query-endpoint! query value)
  (set-query-property! query "sg:executedAt" value "xsd:string"))

(define-syntax-rule
  (set-query-start-time! query value)
  (set-query-property! query "prov:startedAtTime" value "xsd:dateTime"))

(define-syntax-rule
  (set-query-end-time! query value)
  (set-query-property! query "prov:endedAtTime" value "xsd:dateTime"))

(define-syntax-rule
  (set-query-project! query value)
  (set-query-property! query "sg:isRelevantTo" value #f))

(define-syntax-rule
  (set-query-marked! query value)
  (set-query-property! query "sg:isProtected" value "xsd:boolean"))

;; QUERIES PERSISTENCE
;; ----------------------------------------------------------------------------
(define (persist-query content endpoint username start-time end-time project-id marked?)
  (let* [(query-id (generate-id content endpoint username project-id))
         (format-timestamp (lambda (timestamp)
                             (format #f "~s^^xsd:dateTimeStamp"
                                     (strftime "%Y-%m-%dT%H:%M:%SZ"
                                               (gmtime timestamp)))))
         (query (string-append
                 internal-prefixes
                 "INSERT INTO <" system-state-graph "> { "
                 "query:" query-id
                 " rdf:type sg:Query ;"
                 " sg:queryText " (format #f "~s^^xsd:string" content) " ;"
                 " sg:executedAt " (format #f "~s^^xsd:string" endpoint) " ;"
                 " sg:executedBy agent:" username " ;"
                 " dcterms:date " (format-timestamp (current-time)) " ;"
                 " prov:startedAtTime " (format-timestamp start-time) " ;"
                 " prov:endedAtTime " (format-timestamp end-time) " ;"
                 " sg:isRelevantTo <" project-id "> ."
                 "}"))
         (connection (system-connection))]
    (receive (header body)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          #t
          (begin
            (display (get-string-all body))
            #f)))))

;; QUERY-ADD
;; ----------------------------------------------------------------------------
(define (query-add content endpoint username start-time end-time project)
  "Adds a reference to the internal graph for the query RECORD."
  (cond
   [(string= content "")
    (values #f (format #f "The query cannot be empty."))]
   [(string= endpoint "")
    (values #f (format #f "The query must have an endpoint."))]
   [(not (string? project))
    (values #f (call-with-output-string
                 (lambda (port)
                   (sxml->xml '("Please make one of your "
                                (a (@ (href "/projects")) "projects")
                                " active.") port))))]
   [(string= project "")
    (values #f (format #f "The query must have a project."))]
   [#t
    (begin
      (persist-query content endpoint username start-time end-time project #f)
      (values #t ""))]))

;; QUERY-REMOVE
;; ----------------------------------------------------------------------------
(define (query-remove query-uri username)
  "Removes the reference in the internal graph for QUERY."
  (let [(query (string-append
                internal-prefixes
                "WITH <" system-state-graph ">"
                " DELETE { <" query-uri "> ?predicate ?object . }"
                " WHERE  { <" query-uri "> ?predicate ?object ; "
                "sg:executedBy agent:" username " . }"))]
    (receive (header body)
        (system-sparql-query query)
      (= (response-code header) 200))))

;; QUERY-REMOVE-UNMARKED-FOR-PROJECT
;; ----------------------------------------------------------------------------

(define (query-remove-unmarked-for-project username project-uri)
  "Removes queries for which marked? is #f inside PROJECT-URI."
  (let [(query (string-append
                internal-prefixes
                "WITH <" system-state-graph ">
DELETE { ?query ?p ?o }
WHERE { ?query sg:executedBy agent:" username " ;
               sg:isRelevantTo <" project-uri "> ; ?p ?o .
  OPTIONAL {
    ?query sg:isProtected ?isProtected .
  }
  FILTER (!BOUND(?isProtected) OR ?isProtected = false)
}
"))
        (connection (system-connection))]
    (receive (header body)
        (sparql-query query
                      #:uri (connection-uri connection)
                      #:digest-auth
                      (if (and (connection-username connection)
                               (connection-password connection))
                          (string-append
                           (connection-username connection) ":"
                           (connection-password connection))
                          #f))
      (if (= (response-code header) 200)
          (values #t (format #f "Removed unmarked."))
          (values #f (get-string-all body))))))


;; ALL-QUERIES
;; ----------------------------------------------------------------------------

(define (generate-query-with-filters filters)
  (string-append
   internal-prefixes
   "
SELECT DISTINCT ?query AS ?queryId ?queryText ?executedAt
       (STRAFTER(STR(?executedBy), STR(agent:)) AS ?executedBy)
       ?projectTitle ?isProtected
       (MAX(?startTime) AS ?startTime) (MAX(?endTime) AS ?endTime)
       (AVG(?executionTime) AS ?executionTime)
FROM <" system-state-graph ">
WHERE {
  ?query rdf:type         sg:Query ;
         sg:queryText     ?queryText     ;
         sg:executedAt    ?executedAt    ;
         sg:executedBy    ?executedBy    ;
         sg:isRelevantTo  ?project       .

  OPTIONAL { ?query   prov:startedAtTime ?startTime     . }
  OPTIONAL { ?query   prov:endedAtTime   ?endTime       . }
  OPTIONAL { ?query   sg:isProtected     ?isProtected   . }
  OPTIONAL { ?query   dcterms:date       ?date          . }
  OPTIONAL { ?project dcterms:title      ?projectTitle  . }

  BIND(IF((BOUND(?startTime) AND BOUND(?endTime)),
            xsd:dateTime(?endTime) - xsd:dateTime(?startTime),
             0)
        AS ?executionTime)
"
   (if filters
       (format #f "~{  FILTER (~a)~%~}" filters)
       "")
   "}
GROUP BY ?queryId ?query ?queryText ?executedAt ?executedBy ?projectTitle ?isProtected
ORDER BY DESC(?startTime)"))

(define* (all-queries #:key (filter #f))
  "Returns a list of query records, applying FILTER to the records."
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters '()))))]
    (if filter
        (map filter results)
        results)))

(define* (queries-by-username username #:key (filter #f))
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters
                     `(,(format #f "?executedBy = agent:~a" username))))))]
    (if filter
        (map filter results)
        results)))

(define* (query-by-id id #:key (filter #f))
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters
                     `(,(format #f "?query = <~a>" id))))))]
    (if filter
        (map filter results)
        results)))

(define (queries-by-project project)
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters
                     `(,(format #f "?project = <~a>" project))))))]
    (if (null? results)
        '()
        results)))
