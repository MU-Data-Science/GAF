;;; Copyright © 2019, 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www components project-graphs)
  #:use-module (www util)
  #:use-module (www db projects)
  #:use-module (www db connections)

  #:export (assigned-graphs-table
            project-members-table))

;; ----------------------------------------------------------------------------
;; ASSIGNED-GRAPHS-TABLE
;; ----------------------------------------------------------------------------

(define (assigned-graphs-table username hash)
  (let* [(project     (project-by-hash hash))
         (project-uri (project-id project))
         (graphs      (project-assigned-graphs project-uri))]
    `((table (@ (id "assigned-graphs")
                (class "item-table"))
             (tr (th "Graph")
                 (th (@ (style "width: 150pt; text-align: right;")
                        (colspan "3")) "Actions"))
             ,(map (lambda (item)
                     `(tr (td ,(format #f "~a (~a)"
                                       (assoc-ref item "graph")
                                       (assoc-ref item "connectionName")))
                          (td "")
                          (td (@ (class "button-column left-button-column"))
                              (form (@ (action ,(string-append "/project-details/" hash))
                                       (method "post"))
                                    (button (@ (type "submit")
                                               (class "action-btn action-btn-lock")
                                               (name ,(if (string= (assoc-ref item "isLocked") "0")
                                                          "lock-assigned-graph"
                                                          "unlock-assigned-graph"))
                                               (value ,(assoc-ref item "graph")))
                                            ,(if (string= (assoc-ref item "isLocked") "0")
                                                 (icon 'unlocked #t)
                                                 (icon 'locked #t)))))
                          (td (@ (class "button-column right-button-column"))
                              (form (@ (action ,(string-append "/project-details/" hash))
                                       (method "post"))
                                    (button (@ (type "submit")
                                               (class "action-btn remove-btn")
                                               (name "remove-assigned-graph")
                                               (value ,(assoc-ref item "graph")))
                                            ,(icon 'x-white #t))))))
                   graphs)))))

;; ----------------------------------------------------------------------------
;; PROJECT-MEMBERS-TABLE
;; ----------------------------------------------------------------------------

(define (project-members-table username hash)
  (let* [(project     (project-by-hash hash))
         (project-uri (project-id project))
         (is-creator? (project-is-created-by? project-uri username))
         (members     (project-members project-uri))]
    (if (null? members)
        '(p "Could not derive project members from queries.")
        `((table (@ (id "project-members")
                    (class "item-table"))
                 (tr (th "Name")
                     (th "# Queries")
                     ,(if is-creator?
                          '(th (@ (style "width: 150pt; text-align: right;"))
                               "Actions")
                          '(th "")))
                 ,(map (lambda (item)
                         `(tr (td ,(assoc-ref item "user"))
                              (td ,(assoc-ref item "queries"))
                              ,(if (and is-creator?
                                        (not (string= (assoc-ref item "user") username)))
                                   `(td (@ (class "button-column left-button-column"))
                                        (form (@ (action ,(string-append "/project-details/" hash))
                                                 (method "post"))
                                              (button (@ (type "submit")
                                                         (class "action-btn remove-btn")
                                                         (name "remove-assigned-member")
                                                         (value ,(assoc-ref item "user")))
                                                      ,(icon 'x-white #t))))
                                   '(td ""))))
                       members))))))
