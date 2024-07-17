;;; Copyright © 2019, 2020 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (logger)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:export (backtrace-on-error?
            set-backtrace-on-error!

            log-error
            log-warning
            log-debug
            log-access

            default-debug-port
            default-error-port
            default-warning-port

            set-default-debug-port!
            set-default-error-port!
            set-default-warning-port!))

(define %log-mutex (make-mutex))

(define %default-debug-port '())
(define %default-error-port '())
(define %default-warning-port '())
(define %backtrace-on-error #f)

(define (default-debug-port)   %default-debug-port)
(define (default-warning-port) %default-warning-port)
(define (default-error-port)   %default-error-port)
(define (backtrace-on-error?)  %backtrace-on-error)

(define (set-default-debug-port! port)
  (set! %default-debug-port port))

(define (set-default-warning-port! port)
  (set! %default-warning-port port))

(define (set-default-error-port! port)
  (set! %default-error-port port))

(define (set-backtrace-on-error! arg)
  (set! %backtrace-on-error arg))

(define (log-any type port function fmt . rst)
  (unless (null? port)
    (lock-mutex %log-mutex)
    (format port "[ ~a ] ~a: ~a: " type
            (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))
            (if (string? function) function "unknown"))
    (apply format (append (list port fmt) rst))
    (newline port)
    (force-output port)
    (unlock-mutex %log-mutex)))

(define (log-debug function fmt . rst)
  (apply log-any
         (append (list "DEBUG" (default-debug-port) function fmt) rst)))

(define (log-warning function fmt . rst)
  (apply log-any
         (append (list "WARNING" (default-warning-port) function fmt) rst)))

(define (log-error function fmt . rst)
  (apply log-any
         (append (list "ERROR" (default-error-port) function fmt) rst))
  (when (backtrace-on-error?)
    (log-any "ERROR" (default-error-port) "log-error" "--- Begin backtrace ---")
    (display-backtrace (make-stack #t) (default-error-port) 2)
    (log-any "ERROR" (default-error-port) "log-error" "---  End backtrace  ---")))

(define (log-access username fmt . rst)
  (apply log-any
         (append (list "ACCESS" (default-debug-port) username fmt) rst)))
