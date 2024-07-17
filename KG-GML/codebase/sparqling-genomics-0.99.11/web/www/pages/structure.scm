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

(define-module (www pages structure)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db projects)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-structure))

(define* (page-structure request-path username hash #:key (post-data ""))
  (page-root-template username "Collect" request-path
   `((h2 "Structure")
     (h3 "Exploratory")
     (p "The exploratory provides a four-step method to dig into the "
        "structure of data available at each connection.")
     (div (@ (class "large-action-btn"))
          (a (@ (href ,(string-append "/exploratory/" hash)))
             "Go to the Exploratory"))
   
     (h3 "Prompt")
     (p "The prompt provides “command-line interface”-style access to "
        "build structures using RDF triplets.")
     (div (@ (class "large-action-btn"))
          (a (@ (href ,(string-append "/prompt/" hash)))
             "Go to the Prompt")))))

