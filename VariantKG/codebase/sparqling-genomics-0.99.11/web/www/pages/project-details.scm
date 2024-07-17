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

(define-module (www pages project-details)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db projects)
  #:use-module (www db connections)
  #:use-module (www components project-graphs)
  #:use-module (www components query-history)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-project-details))

;; ----------------------------------------------------------------------------
;; PAGE-PROJECT-DETAILS
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define-syntax-rule (add-member-button)
  (h2-button #:id      "add-assigned-member"
             #:class   "smaller-action smaller-action-add"
             #:href    "#"
             #:onclick (js "ui_insert_member_form()")
             #:content (icon 'plus)))

(define-syntax-rule (add-graph-button)
  (h2-button #:id      "add-assigned-graph"
             #:class   "smaller-action smaller-action-add"
             #:href    "#"
             #:onclick (js "ui_insert_graph_form()")
             #:content (icon 'plus)))

(define-syntax-rule (remove-project-button project-id)
  (h2-button #:id "remove-project"
             #:class "action-title-btn remove-btn"
             #:onclick (js "ui_remove_project('" project-id "')")
             #:content "Remove"))

(define* (page-project-details request-path username #:key (post-data ""))
  (let* [(hash    (last (string-split request-path #\/)))
         (project (project-by-hash hash))
         (title   (project-name project))
         (message
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (post-data->alist (uri-decode post-data))))
                    (match alist
                      [(('assign-graph . a) ('connection . b))
                       (project-assign-graph! (project-id project) a b username)]
                      [(('assign-member . a))
                       (project-assign-member! (project-id project) a username)]
                      [(('remove-assigned-graph . a))
                       (project-forget-graph! (project-id project) a)]
                      [(('lock-assigned-graph . a))
                       (project-lock-assigned-graph! (project-id project) a)]
                      [(('unlock-assigned-graph . a))
                       (project-unlock-assigned-graph! (project-id project) a)]
                      [(('remove-assigned-member . a))
                       (project-forget-member! (project-id project) a)]
                      [=> (values #t "")]))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f))]
    (page-root-template username title request-path
     `((h2 ,title ,(remove-project-button (project-id project)))

       ;; When an action occurred (like “the project was modified”), we
       ;; display the success or error message accordingly.
       ,(if message message '())

       (h3 "Members" ,(add-member-button))
       ,(project-members-table username hash)

       (h3 "Assigned graphs" ,(add-graph-button))
       (p "Members of this project have access to the following graphs.")
       ,(assigned-graphs-table username hash)

       (h3 "Queries")
       ,(query-history-component username hash)

       (script "
function ui_insert_graph_form () {
  jQuery('#assigned-graphs tbody:last-child').append('"
         (tr (td (@ (colspan "4"))
                 (form (@ (action "/project-details/" ,hash)
                          (method "post"))
                       (table (tr (td (@ (style "width: 100%"))
                                      (input (@ (type "text")
                                                (id "add-uri-field")
                                                (name "assign-graph")
                                                (placeholder "URI"))))
                                  (td (select (@ (id "select-connection")
                                                 (name "connection"))
                                        ,(map (lambda (connection)
                                                `(option (@ (value ,connection))
                                                         ,connection))
                                              (map connection-name
                                                   (load-system-wide-connections)))))
                                  (td (@ (class "item-table-right"))
                                      (button (@ (id "add-field-button")
                                                 (type "submit"))
                                              ,(icon 'return-white #t)))))))) "');
  jQuery('#add-field').focus();
  jQuery('#add-assigned-graph').remove();
}

function ui_insert_member_form () {
  jQuery('#project-members tbody:last-child').append('"
         (tr (td (@ (colspan "4"))
                 (form (@ (action "/project-details/" ,hash)
                          (method "post"))
                       (table (tr (td (@ (style "width: 100%"))
                                      (input (@ (type "text")
                                                (id "add-username")
                                                (name "assign-member")
                                                (placeholder "Username"))))
                                  (td (@ (class "item-table-right"))
                                      (button (@ (id "add-field-button")
                                                 (type "submit"))
                                              ,(icon 'return-white #t)))))))) "');
  jQuery('#add-field').focus();
  jQuery('#add-assigned-member').remove();
}
"))
     #:dependencies '(jquery projects))))
