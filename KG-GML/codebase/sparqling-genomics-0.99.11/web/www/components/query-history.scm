;;; Copyright © 2018, 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www components query-history)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (www util)

  #:export (query-history-component))

(define (strip-prefix-lines query)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (line)
                  (unless (or (string-prefix? "prefix" line)
                              (string-prefix? "PREFIX" line)
                              (string= line ""))
                    (format port "~a~%" line)))
                (string-split query #\newline)))))

(define* (query-history-component username hash)
  (let* [(queries     (queries-by-project (project-id
                                           (project-by-hash hash))))]
    `((table (@ (id "query-history-table")
                (class "item-table"))
             (tr (th (@ (style "width: auto;")) "Query")
                 (th "Connection")
                 (th (@ (style "white-space: nowrap;")) "Executed by")
                 (th (@ (style "white-space: nowrap;")) "Duration (in seconds)")
                 (th (@ (style "white-space: nowrap; text-align: right;")
                        (colspan "4"))
                     "Actions "
                     (span (@ (class "table-header-small-text"))
                           "(" (a (@ (href ,(string-append
                                             "/query-history-clear/" hash)))
                                  "Remove unselected") ")")))
             ,(map (lambda (query)
                     `(tr (td (pre ,(strip-prefix-lines (query-content query))))
                          (td ,(query-endpoint query))
                          (td ,(query-username query))
                          (td ,(if (query-execution-time query)
                                   (query-execution-time query)
                                   "Unknown"))
                          (td "")
                          (td (@ (class "button-column left-button-column"))
                              (form (@ (action ,(string-append "/query/" hash))
                                       (method "post"))
                                    (div (@ (class "mark-box-wrapper"))
                                         (input (@ (type "checkbox")
                                                   (id ,(string-append
                                                         "mark-"
                                                         (basename (query-id query))))
                                                   (class "mark-box")
                                                   ,(if (query-marked? query)
                                                        '(checked "checked")
                                                        '(name "favorite"))
                                                   (onchange ,(string-append
                                                               "javascript:toggle_marker('"
                                                               (basename (query-id query))
                                                               "', '"
                                                               (query-id query)
                                                               "'); return false"))
                                                   (value ,(query-id query))))
                                         (label (@ (for ,(string-append
                                                          "mark-"
                                                          (basename (query-id query))))) ""))))
                          (td (@ (class "button-column left-button-column"))
                              (form (@ (action ,(string-append "/query/" hash))
                                       (method "post"))
                                    (button (@ (type "submit")
                                               (class "action-btn remove-btn")
                                               (name "remove")
                                               (value ,(query-id query)))
                                            ,(icon 'x-white #t))))
                          (td (@ (class "button-column left-button-column"))
                              (form (@ (action ,(string-append "/query/" hash))
                                       (method "post"))
                                    (input (@ (type "hidden")
                                              (name "endpoint")
                                              (value ,(query-endpoint query))))
                                    (button (@ (type "submit")
                                               (class "action-btn insert-btn")
                                               (name "query")
                                               (value ,(query-content query)))
                                            ,(icon 'up-white #t))))))
                   queries))
      (script (@ (src "/static/js/query-history.js")) ""))))
