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

(define-module (www pages prompt)
  #:use-module (www pages)
  #:use-module (www db connections)
  #:use-module (www db queries)
  #:use-module (www db projects)
  #:use-module (www db prompt)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:export (page-prompt))

(define (page-prompt-layout request-path username hash insert-successfully?)
  (page-root-template username "Prompt" request-path
    (let [(connections (connections-by-user username #:filter connection-name))
          (prompt-id   (prompt-with-tag "web-interface" username #:create? #t))]
      `((h2 "Prompt")

        ,(cond
          [(not insert-successfully?)
           `(div (@ (class "message-box failure"))
                 (p "Inserting the triplet failed."))]

          [(not (default-connection username))
           `(p "Please " (a (@ (href "/dashboard"))
                            "set a connection as default")
               " first.")]
          [(not prompt-id)
           `(div (@ (class "message-box failure"))
                 (p "Could not initiate a prompt session."))]
          [else
           `((form
              (div (@ (id "prompt-wrapper"))
                   (table
                    (tr (td (@ (class "prompt")) "#")
                        (td (@ (class "input-field"))
                            (input (@ (type "hidden")
                                      (id "prompt-id")
                                      (value ,prompt-id)))
                            (input (@ (type "text")
                                      (id "prompt-field")
                                      (name "prompt-field")
                                      (autocomplete "off")
                                      (class "search-field")
                                      (placeholder "Triplets work best"))))))))

             (h3 "Session"
                 ,(h2-button
                   #:id      "clear-prompt-session"
                   #:class    "smaller-action remove-btn"
                   #:onclick (js "clear_session()")
                   #:content (icon 'x-white)))
             (p "")
             (table (@ (id "prompt-session-table")
                       (class "item-table"))
                    (tr (th "Subject")
                        (th "Predicate")
                        (th "Object")))
             (p "Store the session to the following graph:")
             (form (@ (onsubmit "javascript:commit_session(); return false;")
                      (method "post"))
                   ,(let [(graphs (writable-graphs-for-user-in-project username hash))]
                      (if (null? graphs)
                          `(p "To save this session, unlock at least one graph "
                              "in your active project.")
                          `((select (@ (id "select-graph")
                                       (name "select-graph"))
                                    ,(map (lambda (graph)
                                            `(option (@ (value ,(assoc-ref graph "graph")))
                                                     ,(assoc-ref graph "graph")))
                                          graphs))
                            (button (@ (id "add-field-button")
                                      (style "margin-left: 5pt;")
                                      (type "submit")
                                      (value ""))
                                   ,(icon 'save #t)))))))])
      (script "
jQuery(document).ready(function(){
  enable_prompt('#prompt-field');
  jQuery('#prompt-field').focus();

  jQuery.get('/prompt-session-table', function(data){
    jQuery('#prompt-session-table').replaceWith(data);
  });
});")))
    #:dependencies '(jquery prompt)))

(define* (page-prompt request-path username hash #:key (post-data ""))
  (if (not (string= post-data ""))
    (let* [(data      (json-string->scm post-data))
           (subject   (hash-ref data "subject"))
           (predicate (hash-ref data "predicate"))
           (object    (hash-ref data "object"))
           (prompt-id (prompt-with-tag "web-interface" username #:create? #t))]
      (page-prompt-layout request-path username hash
        (prompt-add-triplet prompt-id username subject predicate object)))
    (page-prompt-layout request-path username hash #t)))
