;;; Copyright © 2016, 2017, 2018, 2019  Roel Janssen <roel@gnu.org>
;;; Copyright © 2016  Ricardo Wurmus <rekado@elephly.net>
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

(define-module (www util)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (sparql driver)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www db api)
  #:export (file-extension
            string-replace-occurrence
            suffix-iri
            string-is-longer-than
            post-data->alist
            alist-symbol-key<?
            mkdir-p
            multi-line-trim
            string->sha256sum
            generate-id
            is-uri?
            respond-to-client
            respond-200
            respond-200-with-cookie
            respond-201
            respond-202
            respond-204
            respond-303
            respond-400
            respond-401
            respond-403
            respond-404
            respond-405
            respond-406
            respond-500
            respond-503
            icon
            icon-src
            js
            h2-button
            table-button))

(define (string-is-longer-than str length)
  (catch 'out-of-range
    (lambda _ (if (string-ref str length) #t))
    (lambda (key . args) #f)))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (string-replace-occurrence str occurrence alternative)
  "Replaces every OCCURRENCE in STR with ALTERNATIVE."
  (string-map (lambda (x) (if (eq? x occurrence) alternative x)) str))

(define (suffix-iri input)
  (if input
      (string-trim-both
       (string-drop input
                    (1+ (string-rindex input #\/))) #\")
      "unknown"))

(define (post-data->alist post-data)
  (catch #t
    (lambda _
      (map (lambda (item)
             (let ((pair (string-split item #\=)))
               `(,(string->symbol (uri-decode (car pair))) . ,(uri-decode (cadr pair)))))
           (sort (string-split post-data #\&) string<?)))
    (lambda (key . args) '())))

(define (alist-symbol-key<? a b)
          (string<? (symbol->string (car a))
                    (symbol->string (car b))))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define* (multi-line-trim input #:optional (position 0)
                                           (output '()))
  (if (>= position (string-length input))
      (string-trim-both
       (list->string (map (lambda (char)
                            (if (eq? char #\newline)
                                #\space
                                char))
                          (reverse output))))
      (let [(char (string-ref input position))
            (prev-char (if (= position 0)
                           #\X
                           (string-ref input (- position 1))))]
        (if (and (char-whitespace? char)
                 (char-whitespace? prev-char))
            (multi-line-trim input (+ position 1) output)
            (multi-line-trim input (+ position 1) (cons char output))))))

(define (string->sha256sum input)
  (let* [(command (format #f "printf ~s | sha256sum" input))
         (port    (open-pipe command OPEN_READ))
         (result  (read-delimited " " port))]
    (close-pipe port)
    result))

(define (generate-id . arguments)
  (string->sha256sum (format #f "~{~a~}" arguments)))

(define (is-uri? string)
  (catch #t
    (lambda _
      (or (string-prefix? "http://" string)
          (string-prefix? "https://" string)
          (and (string-prefix? "<" string )
               (string-suffix? ">" string))))
    (lambda (key . args)
      #f)))

(define (respond-to-client response-code client-port content-type body)
  (set-port-encoding! client-port "utf8")
  (let [(content (string->utf8 body))]
    (write-response
     (build-response
      #:code response-code
      #:headers `((content-type   . ,content-type)
                  (content-length . ,(bytevector-length content))))
     client-port)
    (put-bytevector client-port content)))

(define (respond-with-error-message code client-port accept-type message)
  (let [(response-type (first-acceptable-format accept-type))]
    (if response-type
        (respond-to-client code client-port response-type
          (api-format response-type `(error (message ,message))))
        (respond-406 client-port))))

(define (respond-200 client-port accept-type data)
  (let [(response-type (first-acceptable-format accept-type))]
    (if response-type
        (respond-to-client 200 client-port response-type
          (api-format response-type data))
        (respond-406 client-port))))

(define (respond-200-with-cookie client-port cookie)
  (write-response
   (build-response
    #:code 200
    #:headers `((Set-Cookie . ,cookie)))
   client-port))

(define (respond-201 client-port)
  (write-response (build-response #:code 201) client-port))

(define (respond-202 client-port)
  (write-response (build-response #:code 202) client-port))

(define (respond-204 client-port)
  (write-response (build-response #:code 204) client-port))

(define (respond-303 client-port location cookie)
  (write-response
   (if cookie
       (build-response
        #:code 303
        #:headers `((Location   . ,location)
                    (Set-Cookie . ,cookie)))
       (build-response
        #:code 303
        #:headers `((Location   . ,location))))
   client-port))

(define (respond-400 client-port accept-type message)
  (respond-with-error-message 400 client-port accept-type message))

(define (respond-401 client-port accept-type message)
  (respond-with-error-message 401 client-port accept-type message))

(define (respond-403 client-port accept-type message)
  (respond-with-error-message 403 client-port accept-type message))

(define (respond-404 client-port accept-type message)
  (respond-with-error-message 404 client-port accept-type message))

(define (respond-405 client-port allowed-methods)
  (write-response
   (build-response #:code 405
                   #:headers `((Allow . ,(format #f "~a~{, ~a~}~%"
                                                 (car allowed-methods)
                                                 (cdr allowed-methods)))))
   client-port))

(define (respond-406 client-port)
  (respond-to-client 406 client-port '(text/plain)
    (format #f "No acceptable format.~%")))

(define (respond-500 client-port accept-type message)
  (respond-with-error-message 500 client-port accept-type message))

(define (respond-503 client-port accept-type message)
  (respond-with-error-message 500 client-port accept-type message))

(define* (icon symbol #:optional (is-button? #f))
  `(img (@ (src    ,(format #f "/static/images/icons/~a.png" symbol))
           (srcset ,(format #f "/static/images/icons/~a.svg" symbol))
           (class  ,(if is-button? "svg-icon-btn" "svg-icon"))
           (alt    ,symbol))))

(define* (icon-src symbol #:optional (type 'svg))
  (format #f "/static/images/icons/~a.~a" symbol type))

(define (js . args)
  (format #f "javascript:~{~a~}; return false;" args))

(define* (table-button #:key type (onclick "") (href #f) (name "") (content ""))
  `(div (@ (class ,(format #f "small-action action-btn-~a" type))
           (onclick ,onclick)
           (name ,name))
        (a (@ (href ,(if href href "#")))
           ,content)))

(define* (h2-button #:key id class onclick href content)
  `(span (@ (id ,id)
            (class ,class)
            (onclick ,onclick))
         (a (@ (href ,href))
            ,content)))
