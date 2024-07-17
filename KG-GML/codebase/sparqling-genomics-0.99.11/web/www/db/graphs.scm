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

(define-module (www db graphs)
  #:use-module (www config)
  #:use-module (sparql util)

  #:export (graphs-assigned-to-user
            graphs-assigned-to-user-per-connection))

(define (graphs-assigned-to-user username)
  (let* [(query (string-append
                 internal-prefixes
                 "SELECT DISTINCT ?graph ?isLocked ?connectionName"
                 " FROM <" system-state-graph ">"
                 " WHERE {"
                 " ?agent     sg:isAssignedTo     ?projectId ."
                 " ?projectId sg:hasAssignedGraph ?graph ."
                 " ?graph     sg:inConnection     ?connectionName ."
                 " OPTIONAL { ?graph sg:isLocked ?lockState . }"
                 " BIND(IF(BOUND(?lockState), ?lockState, \"false\"^^xsd:boolean)"
                 " AS ?isLocked)"
                 " FILTER (?agent = agent:" username ")"
                 " }"))]
    (query-results->alist
     (system-sparql-query query))))

(define (graphs-assigned-to-user-per-connection username)

  (define* (restructure-connections-and-graphs input #:optional (output '()))
    (if (null? input)
        output
        (let* ((item         (car input))
               (connection   (assoc-ref item "connectionName"))
               (graph        (assoc-ref item "graph"))
               (output-entry (assoc-ref output connection)))
          (restructure-connections-and-graphs
           (cdr input)
           (assoc-set! output
                       connection
                       (cons graph (if output-entry output-entry '())))))))

  (restructure-connections-and-graphs
   (graphs-assigned-to-user username)))
