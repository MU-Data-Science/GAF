;;; Copyright © 2016, 2017, 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www pages query)
  #:use-module (www pages)
  #:use-module (www db connections)
  #:use-module (www db queries)
  #:use-module (www db projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www components query-history)
  #:use-module (srfi srfi-1)
  #:export (page-query))

(define* (page-query request-path username hash #:key (post-data #f))
  (page-root-template username "Query" request-path
   `((h2 "Query the database")
     ,(let* ((connections (connections-by-user username #:filter connection-name))
             (alist       (if post-data (post-data->alist post-data) '()))
             (query       (assoc-ref alist 'query))
             (project     (project-by-hash hash))
             (endpoint    (if (assoc-ref alist 'endpoint)
                              (assoc-ref alist 'endpoint)
                              "")))
        ;; Handle removal instructions.
        (when (assoc-ref alist 'remove)
          (query-remove (assoc-ref alist 'remove) username))
        (cond
         [(null? connections)
          ;; Before we can query, there must be a connection that we can query on.
          ;; The best we can do is refer to creating a connection at this point.
          `((h3 "Create a connection")
            (p "Please " (a (@ (href "/dashboard")) "create a connection") " first."))]
         [(null? project)
          `((h3 "Set active project")
            (p "Please set one of your " (a (@ (href "/projects")) "projects") " to active first."))]
         [else
          ;; Queries are executed on a connection, so we must give users the choice
          ;; to select the appropriate connection.
          `((h3 "Select a connection")
            (select (@ (id "connection"))
              ,(map (lambda (connection)
                      `(option (@ (value ,connection)
                                  ,(if (string= endpoint connection)
                                       `(selected "selected")
                                       '(class "not-selected")))
                               ,connection))
                    connections))

            (h3 "Query editor")
            (p "Use " (strong "Ctrl + Enter") " to execute the query. ("
               (strong "Cmd + Enter") " for the unfortunate MacOS users.)")
            (div (@ (id "editor"))
                 ,(if query
                      query
                      (format #f "~a~%SELECT ?s ?p ?o { ?s ?p ?o }~%LIMIT 100~%"
                              default-prefixes)))

            (div (@ (id "execute-query-button")
                    (onclick ,(js "execute_query(editor)")))
                 (a (@ (href "#")) "Execute query"))

            (h3 "History")

            (p "The table below contains queries that were previously "
               "executed. For compactness, all " (code "PREFIX") " "
               "declarations and empty lines are not shown.")

            ,(query-history-component username hash)
            (script (@ (src "/static/js/query-editor.js")) ""))])))
   #:dependencies '(ace jquery datatables)))
