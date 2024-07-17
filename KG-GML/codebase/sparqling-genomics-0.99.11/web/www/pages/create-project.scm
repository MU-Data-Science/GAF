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

(define-module (www pages create-project)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db projects)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-create-project))

;; ----------------------------------------------------------------------------
;; PAGE-CREATE-PROJECT
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-create-project request-path username
                              #:key (message #f)
                                    (post-data ""))
  (page-root-template username "New project" request-path
   `((h2 "New project")

     ;; When an action occurred (like “a project was added”), we display
     ;; the success or error message accordingly.
     ,(if message message '())

     ;; Display the main form.
     (form (@ (onsubmit "javascript:ui_create_project_form(); return false;")
              (method "post"))
           (table (tr (td (@ (style "width: 100%"))
                          (input (@ (type "text")
                                    (id "add-name-field")
                                    (name "name")
                                    (placeholder "Name"))))
                      (td (@ (class "item-table-right"))
                          (button (@ (id "add-field-button")
                                     (type "submit")
                                     (value ""))
                                 ,(icon 'return-white #t)))))))
   #:dependencies '(jquery projects)))
