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

(define-module (www components sessions)
  #:use-module (www db sessions)
  #:use-module (www util)

  #:export (sessions-table))

;; ----------------------------------------------------------------------------
;; SESSIONS-TABLE
;; ----------------------------------------------------------------------------

(define (sessions-table username)
  (let [(sessions (sessions-by-username username))]
    `((table (@ (id "sessions-table")
                (class "item-table"))
             (tr (th (@ (class "item-table-minsize")) "Name"
                     (div (@ (id "add-session")
                             (class "small-action"))
                          (a (@ (href "#")
                                (onclick "javascript:ui_insert_session_form(); return false;"))
                             ,(icon 'plus))))
                 (th (@ (class "item-table-left")) "Token")
                 (th (@ (style "min-width: 30pt") (colspan 2)) "Actions"))
             ,(map (lambda (session)
                     `(tr (td ,(session-name session))
                          (td (pre ,(session-token session)))
                          (td (@ (class "button-column")) "")
                          (td (@ (class "button-column"))
                              ,(table-button
                                #:type   'remove
                                #:onclick (js "ui_remove_session('" (session-token session) "')")
                                #:content (icon 'x-white)))))
                   sessions))
      (script "
function ui_insert_session_form () {
  jQuery('#sessions-table tbody:last-child').append('"
              (tr (td (form (@ (onsubmit ,(js "ui_submit_session_form()")))
                            (input (@ (type "text")
                                      (id "session-name")
                                      (name "session-name")))))
                  (td "The token will be generated for you.")
                  (td (@ (class "button-column")) "")
                  (td ,(table-button
                        #:type    'submit
                        #:onclick (js "ui_submit_session_form()")
                        #:content (icon 'return-white))))
              "');
  jQuery('#session-name').focus();
  jQuery('#add-session').remove();
}

jQuery(document).ready(function(){ jQuery('add-connection').focus(); });"))))
  
