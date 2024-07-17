;;; Copyright © 2018, 2019, 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www db cache)
  #:use-module (ice-9 ftw)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www util)

  #:export (cached-query-response
            cache-query-response
            remove-query-response-cache

            cache-response-for-query
            cached-response-for-query
            remove-cached-response-for-query))

;; WHOLE-QUERY-RESULTS CACHE
;; ----------------------------------------------------------------------------
;;
;; The following functions implement a file-system caching mechanism for
;; storing query results.
;;

(define (cache-location username key)
  (string-append (www-cache-root) "/query-cache/" username "/" key))

(define (cached-query-response username property)
  (let [(cached-file (cache-location username property))]
    (if (file-exists? cached-file)
        (call-with-input-file cached-file read)
        #f)))

(define (cache-query-response username property value)
  (let [(cache-file (cache-location username property))]
    (mkdir-p (dirname cache-file))
    (call-with-output-file cache-file
      (lambda (port) (write value port)))))

(define (remove-query-response-cache username)
  (let [(cache-dir (cache-location username ""))]
    (when (file-exists? cache-dir)
      (map delete-file (scandir cache-dir))
      (rmdir cache-dir))
    #t))

(define (cache-response-for-query query value)
  (let [(sha (string-append "query-" (string->sha256sum query)))]
    (cache-query-response "sgweb" sha value)))

(define (cached-response-for-query query)
  (let [(sha (string-append "query-" (string->sha256sum query)))]
    (cached-query-response "sgweb" sha)))

(define (remove-cached-response-for-query query)
  (let* [(sha (string-append "query-" (string->sha256sum query)))
         (cache-file (cache-location "sgweb" sha))]
    (when (file-exists? cache-file)
      (delete-file cache-file))
    #t))
