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

(define-module (www pages edit-connection)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-edit-connection))

;; ----------------------------------------------------------------------------
;; PAGE-EDIT-CONNECTION
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-edit-connection request-path username #:key (post-data ""))
  (let* ((name       (last (string-split request-path #\/)))
         (message
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (post-data->alist (uri-decode post-data))))
                    (match alist
                      ((('backend . a) ('name . b) ('password . c) ('uri . d) ('username . e))
                       (connection-edit (alist->connection alist) username))
                      ((('name . a) ('uri . b))
                       (connection-edit (alist->connection alist) username))
                      (else
                       (values #f "Invalid form data."))))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f))
         (connection (user-connection-by-name name username))
         (title (string-append "Edit “" name "”")))
    (page-root-template username title request-path
     `((h2 ,title)
       ;; When an action occurred (like “the connection was modified”), we
       ;; display the success or error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       (form (@ (action ,(string-append "/edit-connection/" name))
                (method "post"))
             (input (@ (type "hidden")
                       (name "name")
                       (value ,(connection-name connection))))
             (table (tr
                     (td "Name")
                     (td (@ (style "width: 90%"))
                         (input (@ (type "text")
                                      (id "add-name-field")
                                      (name "name-disabled")
                                      (value ,(connection-name connection))
                                      (placeholder "Name")
                                      (disabled "disabled")))))
                    (tr
                     (td "URI")
                     (td (input (@ (type "text")
                                      (id "add-uri-field")
                                      (name "uri")
                                      (value ,(connection-uri connection))
                                      (placeholder "https://example.com:8890/sparql")))))
                    (tr
                     (td "Username")
                     (td (input (@ (type "text")
                                      (id "add-username-field")
                                      (name "username")
                                      ,(if (connection-username connection)
                                           `(value ,(connection-username connection))
                                           `(placeholder "Username (optional)"))))))
                    (tr
                     (td "Password")
                     (td (input (@ (type "password")
                                      (id "add-password-field")
                                      (name "password")
                                      ,(if (connection-password connection)
                                          `(value ,(connection-password connection))
                                          `(placeholder "Password (optional)"))))))
                    (tr
                     (td "Back-end")
                     (td (select
                          (@ (name "backend"))
                          ,(map (lambda (backend)
                                  (if (eq? (connection-backend connection)
                                           (string->symbol backend))
                                      `(option (@ (value ,backend)
                                                  (selected "selected"))
                                               ,backend)
                                      `(option (@ (value ,backend))
                                               ,backend)))
                                (map symbol->string (sparql-available-backends))))))
                    (tr
                     (td "")
                     (td (@ (class "item-table-right"))
                            (input (@ (id "edit-button")
                                      (type "submit")
                                      (value "Save modifications"))))))))
     #:dependencies '(jquery))))
