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

(define-module (www pages prompt-session-table)
  #:use-module (www db prompt)
  #:use-module (www util)

  #:export (page-prompt-session-table))

(define* (page-prompt-session-table request-path username #:key (post-data #f))
  (let* [(prompt-id (prompt-with-tag "web-interface" username))
         (triplets  (if prompt-id
                        (prompt-get-triplets prompt-id username)
                        '()))]
    `(table (@ (id "prompt-session-table")
               (class "item-table"))
            (tr (th "Subject")
                (th "Predicate")
                (th "Object")
                (th ""))
            ,(if (null? triplets)
                 `(tr (td (@ (colspan "4")) "There are no triplets in this prompt session."))
                 (map (lambda (row)
                        `(tr (td ,(assoc-ref row "subject"))
                             (td ,(assoc-ref row "predicate"))
                             (td ,(assoc-ref row "object"))
                             (td (@ (class "button-column"))
                                 ,(table-button
                                   #:type 'remove
                                   #:name "remove"
                                   #:content (icon 'x-white)
                                   #:onclick (js "remove_triplet('"
                                                 (assoc-ref row "triplet_id")
                                                 "')")))))
                      triplets)))))
