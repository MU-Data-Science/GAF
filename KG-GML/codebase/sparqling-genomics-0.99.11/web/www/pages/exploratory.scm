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

(define-module (www pages exploratory)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-exploratory))

(define* (page-exploratory request-path username hash #:key (post-data ""))
  (page-root-template username "Exploratory" request-path
   `((h2 "Exploratory")
     (table (@ (id "exploratory-table")
               (class "item-table"))
       (thead
        (tr (th (@ (style "width: 25%;")) "Connections")
            (th (@ (style "width: 25%;")) "Graphs")
            (th (@ (style "width: 25%;")) "Types")
            (th (@ (style "width: 25%;")) "Predicates")))
       (tbody
        (tr (td (div (@ (id "connections")
                        (class "connection-selector multiple-selector")) ""))
            (td (div (@ (id "graphs")
                        (class "graphs-selector multiple-selector")) ""))
            (td (div (@ (id "types")
                        (class "types-selector multiple-selector")) ""))
            (td (div (@ (id "predicates")
                        (class "predicates-selector multiple-selector")) ""))))))
   #:dependencies '(jquery exploratory)))
