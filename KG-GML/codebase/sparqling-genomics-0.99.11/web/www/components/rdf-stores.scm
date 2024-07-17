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

(define-module (www components rdf-stores)
  #:use-module ((sparql driver) #:select (sparql-available-backends))
  #:use-module (www db connections)
  #:use-module (www util)

  #:export (rdf-stores-component
            connections-table))

;; ----------------------------------------------------------------------------
;; CONNECTIONS-TABLE
;; ----------------------------------------------------------------------------
;;
;; This function queries the internal state database and constructs an SXML
;; equivalent for a HTML table that can be inserted into the page.
;;

(define (connections-table username)
  `(table (@ (id "connections-table")
             (class "item-table"))
          (tr (th (@ (class "item-table-left")) "Connection"
                  ;; The “Add connection” button is on the same line as the title.
                  ;; The corresponding CSS makes sure it looks like a button.
                  (div (@ (id "add-connection")
                          (class "small-action"))
                       (a (@ (href "#")
                             (onclick "javascript:ui_insert_connection_form(); return false;"))
                          ,(icon 'plus))))
              (th (@ (style "min-width: 30pt")
                     (colspan "2")) "Actions"))
     ,(map (lambda (record)
             (let [(name            (connection-name    record))
                   (uri             (connection-uri     record))
                   (is-default?     (connection-is-default? record username))
                   (is-system-wide? (system-wide-connection? record))]
               `(tr (td ,(if is-system-wide?
                             (string-append name " (" uri ")")
                             `(a (@ (href ,(string-append "/edit-connection/" name)))
                                 ,(string-append name " (" uri ")"))))
                    (td (@ (class "button-column"))
                        ,(if (or is-default? is-system-wide?)
                             '()
                             (table-button
                              #:type    'remove
                              #:onclick (js "ui_remove_connection('" name "')")
                              #:content (icon 'x-white))))
                    (td (@ (class "button-column"))
                        ,(if (not is-default?)
                             (table-button
                              #:type    'active
                              #:onclick (js "ui_set_default_connection('" name "')")
                              #:content (icon 'check-white))
                             '())))))
           (connections-by-user username))))

(define* (rdf-stores-component redirect-url username #:optional (message #f))
  `(;; When an action failed (like “a connection couldn't be added”), we display
    ;; the error message accordingly.
    ,(if message message '())
    
    ;; Display the main table.
    ,(connections-table username)
    
    ;; The following javascript code adds the form fields to the table.
    (script "
function ui_insert_connection_form () {
  jQuery('#connections-table tbody:last-child').append('"
            (tr (td (@ (colspan "3"))
                    (form (@ (action "/add-connection")
                             (method "post"))
                          (table
                           (tr (td (input (@ (type "text")
                                             (id "add-name-field")
                                             (name "name")
                                             (placeholder "Name"))))
                               (td (input (@ (type "text")
                                             (id "add-uri-field")
                                             (name "uri")
                                             (placeholder "http://example.org:8890/sparql"))))
                               (td (input (@ (type "text")
                                             (id "add-username-field")
                                             (name "username")
                                             (placeholder "Username (optional)"))))
                               (td (input (@ (type "password")
                                             (id "add-password-field")
                                             (name "password")
                                             (placeholder "Password (optional)"))))
                               (td (select
                                    (@ (name "backend"))
                                    ,(map (lambda (backend)
                                            `(option (@ (value ,backend)) ,backend))
                                          (map symbol->string (sparql-available-backends)))))
                               (td (@ (style "width: 32px"))
                                   (button (@ (id "add-field-button")
                                              (type "submit"))
                                           ,(icon 'return-white #t)))))))) "');
  jQuery('#add-field').focus();
  jQuery('#add-connection').remove();
}

jQuery(document).ready(function(){ jQuery('add-connection').focus(); });")))
