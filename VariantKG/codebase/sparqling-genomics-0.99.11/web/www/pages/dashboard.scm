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

(define-module (www pages dashboard)
  #:use-module (www pages)
  #:use-module (www components rdf-stores)
  #:use-module (www components sessions)
  #:export (page-dashboard))

(define* (page-dashboard request-path username #:key (post-data ""))
  (page-root-template username "Dashboard" request-path
    `((h2 "Dashboard")
      (h3 "Session tokens")
      (p "Session tokens can be used to authenticate with the API.  The API "
         "expects the token to be sent as a HTTP cookie in the form "
         (code "SGSession=<token>") ".")
      ,(sessions-table username)
      (h3 "RDF stores")
      (p "The main data sources for SPARQLing-genomics are RDF stores.")
      ,(rdf-stores-component "/dashboard" username))
    #:dependencies '(jquery sessions connections)))
