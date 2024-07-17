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

(define-module (www db prompt)
  #:use-module (ice-9 receive)
  #:use-module (sparql util)
  #:use-module (rnrs io ports)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www util)
  #:use-module (logger)

  #:export (prompt-start
            prompt-delete
            prompt-add-triplet
            prompt-delete-triplet
            prompt-with-tag
            prompt-commit

            prompt-get-triplets
            prompt-clear-triplets
            prompt-save-session

            prompts-by-user))

(define (restore-uri input)
  (cond
   [(is-uri? input)
    (if (string-prefix? "<" input)
        input
        (string-append "<" input ">"))]
   [(is-shorthand-uri? input)
    input]
   [(string->number input)
    input]
   [else
    (if (string-prefix? "\"" input)
        input
        (string-append "\"" input "\""))]))

(define (rdf-token-type token)
  (cond
   [(string-prefix? "<" token)    'IRI]
   [(is-shorthand-uri? token)     'IRI]
   [(string-prefix? "\"" token)   'literal]
   [else 'literal]))

(define* (prompt-start username #:key (tag #f))
  (let* [(timestamp (strftime "%Y-%m-%dT%H:%M:%SZ"
                              (gmtime (current-time))))
         (prompt-id (generate-id "prompt-" username "-" timestamp))
         (query (string-append
                 internal-prefixes
                 "INSERT INTO <" system-state-graph "> {"
                 " <prompt://" prompt-id "> rdf:type <prompt://session> ;"
                 " sg:owner agent:" username " ;"
                 (if tag
                     (string-append " sg:promptTag \"" tag "\"^^xsd:string ;")
                     "")
                 " dcterms:created \"" timestamp "\"^^xsd:dateTime . }"))]
    (receive (header port)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          prompt-id
          #f))))

(define* (prompt-with-tag tag username #:key (create? #f))
  (let* [(query (string-append
                 internal-prefixes
                 "SELECT DISTINCT (STRAFTER(STR(?id), 'prompt://') AS ?id)"
                 " WHERE { GRAPH <" system-state-graph "> {"
                 " ?id rdf:type <prompt://session> ;"
                 " sg:owner agent:" username " ;"
                 " sg:promptTag \"" tag "\"^^xsd:string . } } LIMIT 1"))
         (results (query-results->alist (system-sparql-query query)))]
    (if (null? results)
        (if create?
            (prompt-start username #:tag tag)
            #f)
        (assoc-ref (car results) "id"))))

(define (prompt-delete prompt-id username)
  (catch #t
    (lambda _
      (let* [(query1 (string-append
                      internal-prefixes
                      "DELETE { GRAPH <" system-state-graph "> {"
                      " ?prompt_id ?p ?o . } } "
                      "WHERE { GRAPH <" system-state-graph "> { "
                      " ?prompt_id rdf:type <prompt://session> ;"
                      " sg:owner agent:" username " ; ?p ?o ."
                      " FILTER (?prompt_id = <prompt://" prompt-id ">) "
                      "} }"))
             (query2 (string-append
                      internal-prefixes
                      "DELETE { GRAPH <" system-state-graph "> {"
                      " ?triplet_id ?p ?o . } } "
                      "WHERE { GRAPH <" system-state-graph "> { "
                      " ?triplet_id sg:isPartOf ?prompt_id ; ?p ?o ."
                      " FILTER (?prompt_id = <prompt://" prompt-id ">) "
                      "} }"))]
        (and
         (receive (header port)
             (system-sparql-query query1)
           (= (response-code header) 200))
         (receive (header port)
             (system-sparql-query query2)
           (= (response-code header) 200)))))
    (lambda (key . args)
      (log-error "prompt-delete"
                 "Uknown exception thrown: ~a" key)
      #f)))

(define (prompt-delete-triplet triplet-id username)
  (catch #t
    (lambda _
      (let* [(query (string-append
                    internal-prefixes
                    "DELETE { GRAPH <" system-state-graph "> {"
                    " ?triplet_id ?p ?o . } } "
                    "WHERE { GRAPH <" system-state-graph "> { "
                    " ?prompt rdf:type <prompt://session> ;"
                    " sg:owner agent:" username " ."
                    " ?triplet_id sg:isPartOf ?prompt ; ?p ?o . "
                    " FILTER (?triplet_id = <triplet://" triplet-id ">)"
                    "} }"))]
        (receive (header port)
            (system-sparql-query query)
          (= (response-code header) 200))))
    (lambda (key . args)
      (log-error "prompt-delete-triplet"
                 "Uknown exception thrown: ~a" key)
      #f)))


(define (prompt-add-triplet prompt-id username subject predicate object)
  (catch #t
    (lambda _
      (let* [(triplet-id (generate-id "triplet-" subject predicate object))
             (query (string-append
                    internal-prefixes
                    "INSERT INTO <" system-state-graph "> {"
                    " <prompt://" prompt-id "> rdf:type <prompt://session> ;"
                    "                          sg:owner agent:" username " ."
                    " <triplet://" triplet-id "> sg:isPartOf <prompt://" prompt-id "> ;"
                    "                            <triplet://subject> " subject " ;"
                    "                            <triplet://predicate> " predicate " ;"
                    "                            <triplet://object> " object " . "
                    "}"))]
        (receive (header port)
            (system-sparql-query query)
          (if (= (response-code header) 200)
              triplet-id
              #f))))
    (lambda (key . args)
      (log-error "prompt-add-triplet" "Uknown exception thrown: ~a" key)
      #f)))

(define (prompt-get-triplets prompt-id username)
  (catch #t
    (lambda _
      (let* [(query (string-append
                     internal-prefixes
                     "SELECT (STRAFTER(STR(?triplet_id), 'triplet://') AS ?triplet_id) "
                     "?subject ?predicate ?object WHERE {"
                     " GRAPH <" system-state-graph "> {"
                     " <prompt://" prompt-id "> rdf:type <prompt://session> ;"
                     "                          sg:owner agent:" username " ."
                     " ?triplet_id sg:isPartOf <prompt://" prompt-id "> ;"
                     "             <triplet://subject>   ?subject ;"
                     "             <triplet://predicate> ?predicate ;"
                     "             <triplet://object>    ?object . "
                     "} }"))
             (results (query-results->alist (system-sparql-query query)))]
        results))
    (lambda (key . args)
      (log-error "prompt-get-triplets" "Uknown exception thrown: ~a" key)
      #f)))

;; PROMPT-COMMIT
;; ----------------------------------------------------------------------------

(define (prompt-commit prompt-id graph username token project-hash)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (let [(query (string-append
              internal-prefixes
              "INSERT { GRAPH <" graph "> { ?s ?p ?o . } } "
              "WHERE { GRAPH <" system-state-graph "> { "
              "?triplet sg:isPartOf <prompt://" prompt-id "> ; "
              "<triplet://subject> ?s ; "
              "<triplet://predicate> ?p ; "
              "<triplet://object> ?o . } }"))]
          (receive (header port)
              (system-sparql-query query)
            (cond
             [(= (response-code header) 200)
              (prompt-delete prompt-id username)]
             [(= (response-code header) 500)
              (log-error "prompt-commit" "Server responded 500:~%~a~%"
                         (get-string-all port))]
             [else
              (log-error "prompt-commit" "Unexpected return state: ~a"
                         (response-code header))]))))
      (lambda (key . args)
        (log-error "prompt-commit" "Unknown exception thrown ~a: ~a" key args)
        #f))))

;; PROMPTS-BY-USER
;; ----------------------------------------------------------------------------

(define (prompts-by-user username)
  (let* [(query (string-append
                 internal-prefixes
                 "SELECT (STRAFTER(STR(?prompt), \"prompt://\") AS ?id)"
                 " ?tag ?creationDate (COUNT(?triplet) AS ?triplets) "
                 "WHERE { GRAPH <" system-state-graph "> {"
                 " ?prompt rdf:type <prompt://session> ."
                 " OPTIONAL { ?prompt sg:promptTag ?tag . }"
                 " ?prompt sg:owner agent:" username " ."
                 " ?prompt dcterms:created ?creationDate ."
                 " OPTIONAL { ?triplet sg:isPartOf ?prompt . }"
                 "} }"))]
    (receive (header port)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          (query-results->alist
           (values header port))
          (begin
            (log-error "prompts-by-user"
                       "Unable to retrieve prompts for ~s due to a ~a response."
                       username
                       (response-code header))
            #f)))))
