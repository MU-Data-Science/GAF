;;; Copyright © 2018, 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www db exploratory)
  #:use-module (sparql util)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db cache)
  #:use-module ((www config) #:select (internal-prefixes
                                       system-state-graph
                                       system-sparql-query
                                       sparql-query-with-connection
                                       uri->shorthand-uri
                                       shorthand-uri->uri))
  #:use-module (ice-9 threads)
  #:use-module (logger)

  #:export (all-graphs-in-project
            all-predicates-in-graph
            all-types
            all-predicates
            hierarchical-tree-roots
            hierarchical-tree-children))

(define (sanitize-graph-name graph)
  (string-map (lambda (x)
                (if (or (eq? x #\.)
                        (eq? x #\/)
                        (eq? x #\:)
                        (eq? x #\#))
                    #\_ x))
   graph))

(define (all-graphs-in-project username connection-name project-hash)
  (let* [(id             (project-id (project-by-hash project-hash)))
         (query          (string-append
                          internal-prefixes
                          "SELECT DISTINCT ?graph ?isLocked ?connectionName "
                          "FROM <" system-state-graph "> "
                          "WHERE {"
                          " <" id "> sg:hasAssignedGraph ?graph . "
                          " ?graph sg:inConnection "
                          (if connection-name
                              (string-append
                               "\"" connection-name "\"^^xsd:string . ")
                              "?connectionName .")
                          " OPTIONAL { ?graph sg:isLocked ?lockState . }"
                          " BIND(IF(BOUND(?lockState), ?lockState,"
                          " \"false\"^^xsd:boolean) AS ?isLocked) "
                          (if connection-name
                              (string-append
                               "BIND (\"" connection-name "\" AS ?connectionName)")
                              "")
                          " } ORDER BY ASC(?graph)"))]
    (query-results->alist
     (system-sparql-query query))))

(define* (all-predicates username connection project-hash token
                         #:key (graph #f) (type #f))
  (catch #t
    (lambda _
      (let* [(query
              (string-append
               internal-prefixes
               "SELECT DISTINCT ?predicate WHERE { "
               (if graph
                   (string-append "GRAPH <" (shorthand-uri->uri graph) "> { ")
                   "")
               (if type
                   (string-append "?s rdf:type <" (shorthand-uri->uri type) "> . ")
                   "")
               "?s ?predicate ?o ."
               " OPTIONAL { ?o rdf:type ?o_type . }"
               " OPTIONAL { ?predicate sg:isSystemProperty ?systype . }"
               " BIND ((IF (isLiteral(?o), datatype(?o), ?o_type) AS ?datatype))"
               " FILTER (!BOUND(?systype) OR ?systype != 1)"
               (if graph "}" "")
               " FILTER (?predicate != rdf:type) }"
               " ORDER BY ASC(?predicate)"))
             (cached (cached-response-for-query query))]
        (if cached cached
            (let [(response (map (lambda (item)
                                   `((,(car (car item)) . ,(uri->shorthand-uri
                                                            (cdr (car item))))))
                                 (query-results->alist
                                  (sparql-query-with-connection
                                   connection query token project-hash))))]
              (call-with-new-thread
               (lambda _
                 (cache-response-for-query query response)))
              response))))
    (lambda (key . args)
      (log-error "all-predicates"
                 "Unknown exception thrown: ~a: ~s" key args)
      '())))

(define* (all-types username connection token project-hash)
  (let* [(query (string-append
                 internal-prefixes
                 "SELECT DISTINCT ?type WHERE { ?s rdf:type ?type . }"))
         (cached           (cached-response-for-query query))]
    (cond
     [cached cached]
     [connection
      (catch #t
        (lambda _
          (let [(result (sort
                         (apply append
                                (query-results->list
                                 (sparql-query-with-connection
                                  connection query token project-hash)
                                 #t)) string<))]
            (cache-response-for-query query result)
            result))
        (lambda (key . args)
          (log-error "all-types"
                     "Unknown exception thrown in ~a: ~a" key args) '()))]
     [else '()])))

;;
;; HIERARCHICAL TREE BUILDING
;; ----------------------------------------------------------------------------
;;

(define (hierarchical-tree-roots connection graph-uri token project-hash)
  "Returns data types that have no parents for GRAPH-URI in CONNECTION."
  (let* [(query
          (string-append internal-prefixes "\n"
                         "SELECT DISTINCT ?type { GRAPH <"
                         (shorthand-uri->uri graph-uri) "> {"
                         " ?s rdf:type ?type ."
                         " OPTIONAL { ?s sg:isPartOf ?parent . }"
                         " FILTER (! BOUND(?parent))"
                         " FILTER (?type != xml2rdf:XmlAttribute) } }"
                         " ORDER BY ASC(?type)"))
         (tree-roots
          (map uri->shorthand-uri
               (apply append
                      (query-results->list
                       (sparql-query-with-connection
                        connection query token project-hash) #t))))]
    tree-roots))

(define (hierarchical-tree-children connection token project-hash graph-uri
                                    tree-root)
  "Returns the direct children of TREE-ROOT for GRAPH-URI in CONNECTION."
  (catch #t
    (lambda _
      (let* [(query
              (string-append
               internal-prefixes
               "\n"
               "SELECT DISTINCT ?type { GRAPH <"
               (shorthand-uri->uri graph-uri) "> {"
               " ?s rdf:type <" (shorthand-uri->uri tree-root) "> ."
               " ?c sg:isPartOf ?s ; rdf:type ?type . } }"
               " ORDER BY ASC(?type)"))
             (children
              (map uri->shorthand-uri
                   (apply append
                          (query-results->list
                           (sparql-query-with-connection connection query token
                                                         project-hash) #t))))]
        children))
    (lambda (key . args)
      (log-error "hierarchical-tree-children" "Thrown: ~a: ~s" key args)
      '())))
