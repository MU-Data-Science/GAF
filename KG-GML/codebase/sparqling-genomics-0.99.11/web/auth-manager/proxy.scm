;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
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

(define-module (auth-manager proxy)
  #:use-module (auth-manager config)
  #:use-module (ice-9 receive)
  #:use-module (logger)
  #:use-module (web client)
  #:use-module (web response)

  #:export (token->user
            apply-permission
            sync-permissions
            passthrough-query
            import-file))

(define (token->user token)
  "Resolves TOKEN to a username, or #f if the token is invalid."
  (catch #t
    (lambda _
      (if (string? token)
          (receive (header port)
              (http-get (string-append (sg-web-uri) "/api/user-info")
                        #:streaming? #t
                        #:headers `((user-agent . ,%user-agent)
                                    (Cookie . ,token)
                                    (accept . ((application/s-expression)))))
            (cond
             [(= (response-code header) 200)
              (let [(data (read port))]
                (assoc-ref data 'username))]
             [else
              #f]))
          #f))
    (lambda (key . args)
      (log-error "token->user" "~a: ~a" key args)
      #f)))

(define (receive-file port)
  "Receives data from PORT and returns the MD5 sum of the received contents."
  #f)

(define (import-file-to-graph md5sum graph-uri)
  #f)

(define (sync-permissions)
  #t)
