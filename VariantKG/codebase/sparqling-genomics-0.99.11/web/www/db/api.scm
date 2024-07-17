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

(define-module (www db api)
  #:use-module (www util)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (sxml simple)
  #:use-module (logger)

  #:export (api-format
            api-is-rdf-format?
            api-request-data->alist
            api-serveable-format?
            first-acceptable-format
            rdf-formats))

(define-syntax-rule (is-format a b)
  (or (equal? a b)
      (member a b)))

(define (rdf-formats)
  '((N-triples . application/n-triples)
    (Turtle    . text/turtle)
    (RDFXML    . application/rdf+xml)))

(define (api-is-rdf-format? fmt)
  (cond
   [(is-format '(application/n-triples) fmt)               #t]
   [(is-format '(text/turtle) fmt)                         #t]
   [(is-format '(application/x-turtle) fmt)                #t]
   [(is-format '(application/rdf+xml) fmt)                 #t]
   [else                                                   #f]))

(define (api-serveable-format? fmt)
  "This function returns #t when FMT can be served, #f otherwise."
  (cond
   [(is-format '(*/*) fmt)                                 #t]
   [(is-format '(text/html) fmt)                           #t]
   [(is-format '(text/csv) fmt)                            #t]
   [(is-format '(application/json) fmt)                    #t]
   [(is-format '(application/xml) fmt)                     #t]
   [(is-format '(application/s-expression) fmt)            #t]
   [else                                                   #f]))

(define (alist->csv alist out)
  "Writes the association list ALIST as comma-seperated values to OUT."
  (if (and (>= (length alist) 2)
           (eq? (car alist) 'error)
           (eq? (caadr alist) 'message))
      (format out "~a" (cadr (cadr alist)))
      (let [(columns (map car (car alist)))]
        (format out "~s~{,~s~}~%" (car columns) (cdr columns))
        (for-each (lambda (row)
                    (format out "~s~{,~s~}~%" (assoc-ref row (car columns))
                            (map (lambda (col)
                                   (assoc-ref row col))
                                 (cdr columns))))
                  alist))))

(define (alist->html alist out)
  "Writes the association list ALIST as an HTML table to OUT."
  (if (and (>= (length alist) 2)
           (eq? (car alist) 'error)
           (eq? (caadr alist) 'message))
      (format out "<p>~a</p>" (cadr (cadr alist)))
      (let [(columns (map car (car alist)))]
        (format out "<html><body><table><tr>~{<th>~a</th>~}</tr>~%" columns)
        (for-each (lambda (row)
                    (format out "<tr>~{<td>~a</td>~}</tr>~%"
                            (map (lambda (col)
                                   (assoc-ref row col))
                                 columns)))
                  alist)
        (format out "</table></body></html>"))))

(define* (alist->sxml input #:optional (inside-list? #f))
  "This function transforms an ALIST or a list of ALISTs into S-expressions
that can be transformed by SXML->XML."
  (cond
   [(and (list? input)
         (list? (car input))
         (not inside-list?))
    `(results
      ,(map (lambda (item)
              (cons 'result
                    (map (lambda (token)
                           (alist->sxml token #t))
                         item)))
            input))]
   [(and (list? input)
         (not inside-list?))
    `(results
      ,(cons 'result
             (map (lambda (item) (alist->sxml item #t)) input)))]
   [else
    (let [(return-match (lambda (a b)
                          (if (string? a)
                              `(,(string->symbol a) ,b)
                              `(,a ,b))))]
      (match input
        ((a b)    (return-match a b))
        ((a . b)  (return-match a b))
        (else     #f)))]))

(define (api-format fmt data)
  (cond
   [(equal? fmt '(text/csv))
    (call-with-output-string
      (lambda (port) (alist->csv data port)))]
   [(equal? fmt '(text/html))
    (call-with-output-string
      (lambda (port) (alist->html data port)))]
   [(equal? fmt '(application/json))
    (scm->json-string data)]
   [(equal? fmt '(application/xml))
    (call-with-output-string
      (lambda (port) (sxml->xml (alist->sxml data) port)))]
   [(equal? fmt '(application/s-expression))
    (call-with-output-string
      (lambda (port) (write data port)))]
   [(equal? fmt '(*/*))
    (api-format '(application/s-expression) data)]
   [else #f]))

(define (first-acceptable-format fmts)
  (if (api-serveable-format? (car fmts))
      (if (equal? (car fmts) '(*/*))
          '(application/s-expression)
          (car fmts))
      (first-acceptable-format (cdr fmts))))

(define (api-request-data->alist fmt data)
  "This function parses DATA and returns an ALIST of its contents."
  (cond
   [(or (equal? fmt '(application/json))
        (member 'application/json fmt))
    (let [(json-data (json-string->scm data))]
      ;; This only works for one level of key-value pairs.
      (hash-map->list (lambda (key value)
                        `(,(string->symbol key) . ,value))
                      json-data))]
   [(or (equal? fmt '(application/xml))
        (member 'application/xml fmt))
    ;; It's unclear how this would work without introducing a top-level
    ;; keyword like <parameters>, so that we can write:
    ;; <parameters>
    ;;   <username>...</username>
    ;;   <password>...</password>
    ;; </parameters>
    (map (lambda (item) `(,(car item) . ,(cadr item)))
         (assoc-ref (xml->sxml data) 'parameters))]
   [(or (equal? fmt '(application/s-expression))
        (member 'application/s-expression fmt))
    ;; We want to end up with an S-expression, so we don't need to do
    ;; anything unless the input was read as a string.
    (if (string? data)
        (call-with-input-string data read)
        data)]
   [(equal? fmt '(application/x-www-form-urlencoded))
    (post-data->alist data)]
   [else
    (log-error "api-request-data->alist" "Unknown format: ~s" fmt)
    #f]))

