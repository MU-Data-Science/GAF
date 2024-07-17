;;; Copyright © 2019 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www pages import)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (sparql driver)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www db api)
  #:use-module (www db projects)
  #:use-module (www db sessions)
  #:use-module (www db exploratory)
  #:use-module (www pages)
  #:use-module (www util)
  #:export (page-import))

(define* (page-import request-path username hash #:key (post-data ""))
  (page-root-template username "Import RDF" request-path
   `((h2 "Import RDF")

     (p "The following three-step procedure helps to form the command to "
        "upload data to an RDF store.")

     (h3 "Step 1: Choose the graph to upload data to.")
     (p "")
     ,(let ((graphs (all-graphs-in-project username #f hash)))
        (if (null? graphs)
            `(div (@ (id "choose-graph"))
                  (p "Before data can be imported, make a graph on the "
                     (a (@ (href ,(string-append "/project-details/" hash))) "Overview")
                     " page, and make sure it is in the " (em "unlocked") " state."))
            `(form
              (select (@ (id "select-graph")
                         (onchange "javascript:update_command(); return false;"))
                (option (@ (value "")) "Select a graph")
                ,(map (lambda (graph)
                        (if (string= (assoc-ref graph "isLocked") "1")
                            '()
                            `(option (@ (value ,(string-append
                                                 (assoc-ref graph "graph") " "
                                                 (assoc-ref graph "connectionName"))))
                                     ,(string-append
                                       (assoc-ref graph "graph")
                                       " (" (assoc-ref graph "connectionName") ")"))))
                      graphs)))))

     (h3 "Step 2: Choose an access token")
     (p "This token will be used to authenticate with to the endpoint.")

     (form
      (select (@ (id "select-token")
                 (onchange "javascript:update_command(); return false;"))
        (option (@ (value "")) "Select an authentication token")
        ,(let [(sessions (sessions-by-username username))]
           (map (lambda (session)
                  (let [(name  (session-name session))
                        (token (session-token session))]
                    `(option (@ (value ,token)) ,name)))
                sessions))))

     (h3 "Step 3: Choose the data format")
     (p "Which type of file are you going to upload?")

     (form
      (select (@ (id "select-format")
                 (onchange "javascript:update_command(); return false;"))
        (option (@ (value "")) "Select a data format")
        ,(let [(sessions (sessions-by-username username))]
           (map (lambda (fmt)
                  (let [(name (car fmt))
                        (mime (cdr fmt))]
                    `(option (@ (value ,mime)) ,name)))
                (rdf-formats)))))

     (p "The command to import data is:")
     (pre (@ (id "command-box") (class "terminal-style")) ""))
   #:dependencies '(jquery import)))
