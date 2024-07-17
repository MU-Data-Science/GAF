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

(define-module (www requests-beacon)
  #:use-module (rnrs bytevectors)
  #:use-module (sparql util)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www db api)
  #:use-module (www util)

  #:export (request-beacon-handler))

(define* (request-beacon-handler request request-path client-port
                                 #:key (username #f))
  (let [(request-body (read-request-body request))
        (accept-type  (request-accept request))
        (content-type (request-content-type request))
        (method       (request-method request))]
    (cond
     [(not (api-serveable-format? accept-type))
      (respond-406 client-port)]

     ;; INFO
     ;; -----------------------------------------------------------------------
     ;; This method provides information about the BEACON endpoint.  It's
     ;; mostly a static response, except for the list of datasets that are
     ;; available for querying.
     ;
     [(or (string= request-path "/beacon")
          (string= request-path "/beacon/"))
      (if (eq? method 'GET)
          (respond-200 client-port accept-type
            `((id           . "org.sparqling-genomics.beacon")
              (name         . "SPARQLing-genomics Beacon")
              (apiVersion   . "1.1.0")
              (organization
               (id          . ,(beacon-organization-id))
               (name        . ,(beacon-organization-name))
               (description . ,(beacon-organization-description))
               (address     . ,(beacon-organization-address))
               (welcomeUrl  . ,(beacon-organization-welcome-url))
               (contactUrl  . ,(beacon-organization-contact-url))
               (logoUrl     . ,(beacon-organization-logo-url))
               (info        . ,(beacon-organization-info)))
              (description  . ,(beacon-organization-description))))
          (respond-405 client-port '(GET)))]

     ;; QUERY
     ;; -----------------------------------------------------------------------
     ;; This method provides actual information about a variant, gene, or
     ;; other annotated resource.
     ;
     [(string= request-path "/beacon/query")
      (if (or (eq? method 'POST) (eq? method 'GET))
          (let* [(data (if (eq? method 'POST)
                           (api-request-data->alist
                            content-type (utf8->string request-body))
                           (post-data->alist
                            (utf8->string request-body))))
                 (reference-name  (assoc-ref data 'referenceName))
                 (start           (assoc-ref data 'start))
                 (end             (assoc-ref data 'end))
                 (start-min       (assoc-ref data 'startMin))
                 (start-max       (assoc-ref data 'startMax))
                 (end-min         (assoc-ref data 'endMin))
                 (end-max         (assoc-ref data 'endMax))
                 (reference-bases (assoc-ref data 'referenceBases))
                 (alternate-bases (assoc-ref data 'alternateBases))

                 ;; The unprecise GRC notation (GRCh37, GRCh38).
                 ;; Maybe we could also allow precise NCBI IDs?
                 (assembly-id     (assoc-ref data "assemblyId"))
                 (dataset-ids     (assoc-ref data "datasetIds"))

                 ;; Should be either "ALL", "HIT", "MISS", or "NONE".
                 ;; When it isn't specified, "NONE" is assumed.
                 (include-dataset-responses
                  (assoc-ref data "includeDatasetResponses"))]
            (cond
             [(and (not (null? reference-name))
                   (not (null? start))
                   (not (null? end))
                   (not (null? reference-bases))
                   (not (null? alternate-bases)))
                ;; Search for SNPs.
                (let* [(query (string-append
                               internal-prefixes
                               "SELECT ?graph ?chromosome ?position ?ref ?alt"
                               "WHERE { GRAPH ?graph {"
                               "  ?v rdf:type vcf2rdf:VariantCall ;"
                               "     faldo:reference ?chromosome ;"
                               "     faldo:position  ?position ;"
                               "     vc:REF ?ref ;"
                               "     vc:ALT ?alt ."
                               "  }"
                               "  FILTER (?position >= " (number->string start) ")"
                               "  FILTER (?position <= " (number->string end) ")"
                               "  FILTER (?ref = seq:" reference-bases ")"
                               "  FILTER (?alt = seq:" alternate-bases ")"
                               "}"))
                       (results (query-results->alist
                                 (beacon-sparql-query query)))]
                  (respond-200 client-port accept-type results))]
             [(or (null? reference-name)
                  (null? start)
                  (null? reference-bases)
                  (null? alternate-bases)
                  (null? assembly-id))
              (respond-400 client-port accept-type
                           (string-append
                            "The following parameters must be provided:"
                            " 'referenceName', 'start', 'end', "
                            " 'referenceBases', 'alternateBases'."))]
             [else
              (respond-400 client-port accept-type
                           (string-append
                            "Something went wrong."))]))
          (respond-405 client-port '(POST)))]

     [else
      (respond-403 client-port accept-type "This resource is off limits.")])))
