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

(define-module (auth-manager virtuoso)
  #:use-module (auth-manager config)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (www base64)
  #:use-module (www util)

  #:export (create-user
            enable-read-access
            enable-write-access

            stage-file
            start-bulk-load))

;; MODULE-SPECIFIC CONSTANTS
;; ----------------------------------------------------------------------------
(define %db-virtuoso-users '())

;; VIRTUOSO-SPECIFIC CONSTANTS
;; ----------------------------------------------------------------------------
(define READ_PERMISSION               1)
(define WRITE_PERMISSION              2)
(define WRITE_FROM_NETWORK_PERMISSION 3)

;; VIRTUOSO-USER RECORD TYPE
;; ----------------------------------------------------------------------------
(define-record-type <virtuoso-user>
  (make-virtuoso-user username password)
  virtuoso-user?
  (username   virtuoso-user-username  set-virtuoso-user-username!)
  (password   virtuoso-user-password  set-virtuoso-user-password!))

;; RECORD TYPE RELATED FUNCTIONS
;; ----------------------------------------------------------------------------
(define (alist->virtuoso-user input)
  (catch #t
    (lambda _
      (let ((obj (make-virtuoso-user (assoc-ref input 'username)
                                     (assoc-ref input 'password))))
        (unless (string? (virtuoso-user-username obj))
          (throw 'no-username "A virtuoso user needs to have a username."))
        ;; Generate a strong password when none is given.
        (unless (string? (virtuoso-user-password obj))
          set-virtuoso-user-password! obj
          (string-replace-occurrence
           (string-replace-occurrence
            (base64-encode
             (u8-list->bytevector
              (map (lambda _ (random 256)) (iota 63))))
            #\/ #\0)
           #\+ #\1))))
    (lambda (key . args)
      (log-error "alist->virtuoso-user" "~a: ~a" key args)
      #f)))

(define (virtuoso-user->alist record)
  `((username . ,(virtuoso-user-username record))
    (password . ,(virtuoso-user-password record))))

;; VIRTUOSO-USER PERSISTENCE
;; ----------------------------------------------------------------------------
(define (load-virtuoso-users)
  (catch #t
    (lambda _
      (let ((filename (string-append (www-cache-root) "/virtuoso-users.scm")))
        (when (file-exists? filename)
          (call-with-input-file filename
            (lambda (port)
              (set! %db-virtuoso-users
                    (map alist->virtuoso-user (read port))))))))
    (lambda (key . args)
      #f)))

(define (persist-virtuoso-users)
  (let ((filename (string-append (www-cache-root) "/virtuoso-users.scm")))
    (call-with-output-file filename
      (lambda (port)
        ;; Before writing to the file under 'port', chmod it so that
        ;; only the user this process runs as can read its contents.
        (chmod port #o600)
        (format port ";; This file was generated by sparqling-genomics.~%")
        (format port ";; Please do not edit this file manually.~%")
        (write (map virtuoso-user->alist %db-virtuoso-users) port)))))

(define (virtuoso-user-by-username username)
  "Returns a virtuoso-user for USERNAME."
  (let ((item (filter (lambda (virtuoso-user)
                        (string= (virtuoso-user-username virtuoso-user) username))
                      %db-virtuoso-users)))
    (if (null? item)
        #f
        item)))

(define (enable-permission graph user permission)
  "This function instructs Virtuoso to assign PERMISSION to USER for GRAPH."
  (system (format #f "printf \"DB.DBA.RDF_GRAPH_USER_PERMS_SET ('~a', '~a', ~a);\n\" | ~a ~a -U ~a -P ~a~%"
                  graph user permission
                  (isql-bin) (isql-port) (rdf-store-username) (rdf-store-password))))

(define (enable-read-access graph user)
  (enable-permission graph user 1))

(define (enable-write-access graph user)
  ;; Set read (1) and write (2) bits, write-from-network (4) (+ 1 2 4) => 7
  (enable-permission graph user 7))

(define (stage-file filename graph-uri)
  (zero?
   (system (format #f
            "printf \"ld_dir ('~a', '~a', '~a');\n\" | ~a ~a -U ~a -P ~a~%"
            (dirname filename) (basename filename) graph-uri
            (isql-bin) (isql-port)
            (rdf-store-username) (rdf-store-password)))))

(define (start-bulk-load)
  (zero?
   (system (format #f "printf \"rdf_loader_run ();\n\" | ~a ~a -U ~a -P ~a~%"
                   (isql-bin) (isql-port)
                   (rdf-store-username) (rdf-store-password)))))

(define (load-file filename graph-uri)
  (stage-file filename graph-uri)
  (start-bulk-load))

(define (create-user username)
  "This function sets up a new user in Virtuoso."

  (let [(record (virtuoso-user-by-username username))]
    (if record
        record
        ;; TODO: Do proper error-checking.
        (let* [(record   (alist->virtuoso-user `((username . ,username)
                                                 (password . #f))))
               (password (virtuoso-user-password record))]
          (set! %db-virtuoso-users (cons record %db-virtuoso-users))
          (persist-virtuoso-users)
          (system (string-append
                   "echo \"DB.DBA.USER_CREATE ('" username "', '" password "'); "
                   "DB.DBA.RDF_DEFAULT_USER_PERMS_SET ('" username "', 0); "
                   "GRANT SPARQL_UPDATE TO \\\"" username "\\\";\" "
                   "| " (isql-bin) " " (number->string (isql-port))
                   " -U " (rdf-store-username) " -P '" (rdf-store-password) "'~%"))
          record))))
