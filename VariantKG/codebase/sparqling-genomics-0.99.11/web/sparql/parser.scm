;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (sparql parser)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (logger)

  #:export (<query>

            query-type
            set-query-type!

            query-base
            set-query-base!

            query-prefixes
            set-query-prefixes!

            query-quads
            set-query-quads!

            query-construct-patterns
            set-query-construct-patterns!

            query-modify-patterns
            set-query-modify-patterns!

            query-quads
            set-query-quads!

            query-global-graphs
            set-query-global-graphs!

            query-out-variables
            set-query-out-variables!

            parse-query))

(define-class <query> ()

  (type            #:init-value #nil
                   #:getter query-type
                   #:setter set-query-type!)

  (base            #:init-value #f
                   #:getter query-base
                   #:setter set-query-base!)

  (prefixes        #:init-value '()
                   #:getter query-prefixes
                   #:setter set-query-prefixes!)

  (global-graphs   #:init-value '()
                   #:getter query-global-graphs
                   #:setter set-query-global-graphs!)

  (out-variables   #:init-value '()
                   #:getter query-out-variables
                   #:setter set-query-out-variables!)

  (quads           #:init-value '()
                   #:getter query-quads
                   #:setter set-query-quads!)

  (construct-patterns #:init-value '()
                      #:getter query-construct-patterns
                      #:setter set-query-construct-patterns!)

  (modify-patterns #:init-value '()
                   #:getter query-modify-patterns
                   #:setter set-query-modify-patterns!))

(define-method (write (query <query>) out)
  (format out "#<<query> ~a, prefixes: ~a, global graphs: ~a, quads: ~a>"
          (query-type query)
          (length (query-prefixes query))
          (length (query-global-graphs query))
          (length (query-quads query))))

(define (parse-query query)
  "Returns an instace of <query>."

  (define (string-is-longer-than str length)
    (catch 'out-of-range
      (lambda _ (if (string-ref str length) #t))
      (lambda (key . args) #f)))

  (define* (string-pred-index pred str #:optional (start 0))
    (if (pred (string-ref str start))
        start
        (string-pred-index pred str (+ start 1))))

  (define* (string-non-pred-index pred str #:optional (start 0))
    (if (not (pred (string-ref str start)))
        start
        (string-non-pred-index pred str (+ start 1))))

  (define* (is-absolute-uri? str)
    (if (and (string? str)
             (string-contains str "://"))
        #t
        #f))

  (define (string-contains-ci-surrounded-by-whitespace s1 s2 start)
    (let [(pstart (string-contains-ci s1 s2 start))]
      (cond
       [(not pstart)                            #f]
       [(= pstart 0)                            pstart]
       [(and (> pstart 0)

             (char-whitespace?
              (string-ref s1 (- pstart 1)))

             (catch 'out-of-range
               (lambda _
                 (char-whitespace?
                  (string-ref s1
                    (+ pstart
                       (string-length s2)))))
               (lambda _ #t)))                   pstart]
       [else                                     #f])))

  (define (parse-uri-token out token)
    (if (eq? (string-ref token 0) #\<)

        ;; Deal with absolute URIs.
        ;; --------------------------------------------------------------------
        (let* [(uri-start 0)
               (uri-end   (string-index token #\> (+ uri-start 1)))]
          (if uri-end
              (let [(out-token (string-copy token (+ uri-start 1) uri-end))]
                (if (and (query-base out)
                         (not (is-absolute-uri? out-token)))
                    (string-append (query-base out) out-token)
                    out-token))
              #f))

        ;; Deal with shorthand URIs.
        ;; --------------------------------------------------------------------
        (let [(shortcode-end (string-index token #\:))]
          (cond
           [(not shortcode-end)
            #f]
           [(> shortcode-end 0)
            (let* [(sym      (string-trim (string-copy token 0 shortcode-end)))
                   (sym-len  (string-length sym))
                   (prefixes (query-prefixes out))
                   (prefix   (assoc-ref prefixes sym))]
              (cond
               [(= sym-len 0)
                (parse-uri-token out (string-trim token))]
               [prefix
                (string-append prefix (substring token (+ shortcode-end 1)))]
               [else
                #f]))]
           [else
            (let* [(prefixes     (query-prefixes out))
                   (empty-prefix (assoc-ref prefixes ""))]
              (if empty-prefix
                  (string-append empty-prefix (substring token 1))
                  #f))]))))

  (define* (remove-comments text #:optional (position 0)
                                            (modes    '(none))
                                            (output   '()))
    (if (or (not position)
            (not (string-is-longer-than text position)))
        (list->string (reverse output))
        (let [(buffer (string-ref text position))]
          (cond
           [(eq? buffer #\#)
            (if (or (eq? (car modes) 'single-quoted)
                    (eq? (car modes) 'double-quoted)
                    (and (> position 0)
                         (string-is-longer-than text (+ position 1))
                         (not (char-whitespace?
                               (string-ref text (- position 1))))
                         (eq? #\> (string-ref text (+ position 1)))))
                (remove-comments text
                                 (+ position 1)
                                 modes
                                 (cons buffer output))
                (remove-comments text
                                 (string-index text #\newline position)
                                 modes
                                 output))]
           [(eq? buffer #\")
            (remove-comments text
                             (+ position 1)
                             (if (eq? (car modes) 'double-quoted)
                                 (cdr modes)
                                 (cons 'double-quoted modes))
                             (cons buffer output))]
           [(eq? buffer #\')
            (remove-comments text
                             (+ position 1)
                             (if (eq? (car modes) 'single-quoted)
                                 (cdr modes)
                                 (cons 'single-quoted modes))
                             (cons buffer output))]
           [else
            (remove-comments text
                             (+ position 1)
                             modes
                             (cons buffer output))]))))

  (define* (read-base out text #:optional (start 0))
    ;; We expect the URI to come after BASE to be an absolute URI because
    ;; this is what the SPARQL spec has to say about it:
    ;; “Base IRIs declared with the BASE keyword must be absolute IRIs.”.
    (let* [(base-start  (string-contains-ci-surrounded-by-whitespace
                         text "base" start))
           (uri-start   (when base-start
                          (string-index text #\< (+ base-start 4))))
           (uri-end     (unless (unspecified? uri-start)
                          (string-index text #\> (+ uri-start 1))))]
      (if (unspecified? uri-end)
          start
          (begin
            (set-query-base! out (string-copy text (+ uri-start 1) uri-end))
            (+ uri-end 1)))))

  (define (read-prefixes out text start)
    (let* [(prefix-start  (string-contains-ci-surrounded-by-whitespace
                           text "prefix" start))
           (shortcode-end (when prefix-start
                            (string-index text #\: (+ prefix-start 7))))
           (uri-start     (unless (unspecified? shortcode-end)
                            (let [(uri-open (string-non-pred-index
                                             char-whitespace?
                                             text (+ shortcode-end 1)))]
                              (if uri-open
                                  uri-open
                                  (string-non-pred-index
                                   char-whitespace?
                                   text (+ shortcode-end 1))))))
           (uri-end       (unless (unspecified? uri-start)
                            (if (eq? (string-ref text uri-start) #\<)
                                (string-index text #\> (+ uri-start 1))
                                (string-pred-index char-whitespace?
                                                   text (+ uri-start 1)))))]
      (if (unspecified? uri-end)
          start
          (let [(uri-token (parse-uri-token out
                             (string-trim-both
                              (string-copy text uri-start (+ uri-end 1)))))]
            (set-query-prefixes! out
              (cons `(;; Cut out the shortcode.
                      ,(string-trim-both
                        (string-copy text (+ prefix-start 7) shortcode-end))
                      .
                      ;; Cut out the URI.
                      ,uri-token)
                    (query-prefixes out)))
            (read-prefixes out text (+ uri-end 1))))))

  (define (read-prologue out query start)
    (let* [(after-base     (read-base out query 0))
           (after-prefixes (read-prefixes out query 0))]
      after-prefixes))

  (define (determine-query-type out text start)
    (let* [(types  (filter
                    (lambda (item) (cdr item))
                    `((ASK       . ,(string-contains-ci-surrounded-by-whitespace
                                     text "ask" start))
                      (CLEAR     . ,(string-contains-ci-surrounded-by-whitespace
                                     text "clear graph" start))
                      (CONSTRUCT . ,(string-contains-ci-surrounded-by-whitespace
                                     text "construct" start))
                      (DELETE    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "delete" start))
                      (DESCRIBE  . ,(string-contains-ci-surrounded-by-whitespace
                                     text "describe" start))
                      (INSERT    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "insert" start))
                      (SELECT    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "select" start)))))
           (cars  (map car types))
           (type  (cond
                   [(null? cars)          'UNKNOWN]
                   [(> (length cars) 1)   (apply symbol-append cars)]
                   [else                  (car cars)]))]
      (set-query-type! out type)
      (if (eq? type 'UNKNOWN)
          start
          (assoc-ref types type))))

  (define* (tokenize-triplet-pattern out text #:optional (cursor  0)
                                              #:key      (modes   '(none))
                                                         (current '())
                                                         (graph   #f)
                                                         (quads   '())
                                                         (tokens  '()))

    (define (graph-test)
      (and (> (length tokens) 1)
           (string? (car tokens))
           (string-ci= (cadr tokens) "graph")))

    (define (not-in-quotes mode)
      (and (not (eq? modes 'double-quoted))
           (not (eq? modes 'single-quoted))))

    (define (cons-token out lst tokens)
      (let [(token      (list->string (reverse lst)))]
        (cond
         [(null? lst)
          tokens]
         [(string-ci= token "optional")
          tokens]
         [else
          (cons token tokens)])))

    (define (finalize-parser cursor)
      (if (> (length tokens) 2)
          (values (cons (list
                         (if (> (length tokens) 3)
                             (list-ref tokens 3)
                             #f)
                         (list-ref tokens 2)
                         (list-ref tokens 1)
                         (list-ref tokens 0)) quads)
                  cursor)
          (values quads cursor)))

    (define (process-quad tokens quads)
      (let [(rev (map (lambda (token)
                        (let [(uri (parse-uri-token out token))]
                          (if uri uri token)))
                      (reverse tokens)))]
        (cond
         [(null? rev)
          (values tokens quads)]

         ;; A triplet patterns wrapped inside a GRAPH.
         [(and (> (length rev) 4)
               (string-ci= "graph" (list-ref rev 0)))
          (values (drop tokens 3)
                  (cons (list (list-ref rev 1)
                              (list-ref rev 2)
                              (list-ref rev 3)
                              (list-ref rev 4))
                        quads))]

         ;; When no GRAPH clause is given, we emit a quad without a graph.
         ;; It may be further confined by the list of global graphs.
         [(= (length rev) 3)
          (values (drop tokens 2)
                  (cons (list #f
                              (list-ref rev 0)
                              (list-ref rev 1)
                              (list-ref rev 2))
                        quads))]
         [else
          (values tokens quads)])))

    (if (or (not cursor)
            (not (string-is-longer-than text cursor)))
        (finalize-parser cursor)
        (let [(buffer (string-ref text cursor))]
          (cond
           [(and (eq? buffer #\{)
                 (eq? (car modes) 'none))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cons 'initial-scope modes)
              #:current current
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]
           [(and (eq? buffer #\})
                 (eq? (car modes) 'initial-scope))
            (finalize-parser (+ cursor 1))]
           [(and (eq? buffer #\{)
                 (not-in-quotes (car modes)))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cons 'in-context modes)
              #:current current
              #:quads   quads
              #:graph   (if (graph-test)
                            (parse-uri-token out (car tokens))
                            graph)
              #:tokens  (cons-token out current tokens))]
           [(and (eq? buffer #\})
                 (eq? (car modes) 'in-context))
            (call-with-values (lambda _ (process-quad tokens quads))
              (lambda (tokens-without-quad updated-quads)
                (tokenize-triplet-pattern out text (+ cursor 1)
                  #:modes   (cdr modes)
                  #:current '()
                  #:quads   updated-quads
                  #:graph   #f
                  #:tokens  (if (and (not (eq? (cadr modes) 'in-context))
                                     (>= (length tokens-without-quad) 2))
                                (drop tokens-without-quad 2)
                                tokens-without-quad))))]

           ;; TODO: Make sure whatever occurs between #\( and #\) is
           ;; treated as a single token.
           [(and (eq? buffer #\()
                 (not (null? tokens))
                 (or (string-ci= (car tokens) "filter")
                     (string-ci= (car tokens) "bind")))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cons 'black-mode modes)
              #:current '()
              #:quads   quads
              #:graph   graph
              #:tokens  (cdr tokens))]
           [(eq? buffer #\()
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cons 'black-mode modes)
              #:current '()
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]

           [(and (eq? buffer #\))
                 (eq? (car modes) 'black-mode))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cdr modes)
              #:current '()
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]

           [(eq? buffer #\")
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (if (eq? (car modes) 'double-quoted)
                            (cdr modes)
                            (cons 'double-quoted modes))
              #:current (cons buffer current)
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]
           [(eq? buffer #\')
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (if (eq? (car modes) 'single-quoted)
                            (cdr modes)
                            (cons 'single-quoted modes))
              #:current (cons buffer current)
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]

           [(and (eq? buffer #\.)
                 (not-in-quotes (car modes))
                 (not (eq? (car modes) 'in-uri))
                 (> (length tokens) 2))
            (call-with-values (lambda _ (process-quad tokens quads))
              (lambda (tokens-without-quad updated-quads)
                (tokenize-triplet-pattern out text (+ cursor 1)
                  #:modes   modes
                  #:current '()
                  #:quads   updated-quads
                  #:graph   graph
                  #:tokens  (drop tokens 3))))]
           [(and (eq? buffer #\;)
                 (not-in-quotes (car modes)))
            (call-with-values (lambda _ (process-quad tokens quads))
              (lambda (tokens-without-quad updated-quads)
                (tokenize-triplet-pattern out text (+ cursor 1)
                  #:modes   modes
                  #:current '()
                  #:quads   updated-quads
                  #:graph   #f
                  #:tokens  (drop tokens 2))))]
           [(char-whitespace? buffer)
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   modes
              #:current '()
              #:quads   quads
              #:graph   #f
              #:tokens  (cons-token out current tokens))]
           [(and (eq? buffer #\<)
                 (not (eq? (car modes) 'black-mode)))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cons 'in-uri modes)
              #:current (cons buffer current)
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]
           [(and (eq? buffer #\>)
                 (eq? (car modes) 'in-uri))
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   (cdr modes)
              #:current (cons buffer current)
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]
           [else
            (tokenize-triplet-pattern out text (+ cursor 1)
              #:modes   modes
              #:current (if (eq? (car modes) 'black-mode)
                            current
                            (cons buffer current))
              #:quads   quads
              #:graph   graph
              #:tokens  tokens)]))))

  (define* (tokenize-query-header out text #:optional (cursor  0)
                                           #:key      (modes   '(none))
                                                      (current '())
                                                      (tokens  '()))
    (define (cons-token out lst tokens)
      (let [(token     (list->string (reverse lst)))
            (from-test (and (not (null? tokens))
                            (string? (car tokens))
                            (or (string-ci= (car tokens) "from")
                                (string-ci= (car tokens) "into"))))]
        (if (or (string= token "")
                (and from-test
                     (string-ci= token "named")))
            tokens
            (if from-test
                (cons `(,(car tokens) . ,(parse-uri-token out token))
                      (cdr tokens))
                (cons token tokens)))))

    (if (or (not cursor)
            (not (string-is-longer-than text cursor)))
        (values tokens cursor)
        (let [(buffer (string-ref text cursor))]
          (cond
           [(eq? buffer #\()
            (tokenize-query-header out text (+ cursor 1)
              #:modes   (cons 'in-function modes)
              #:current current
              #:tokens  tokens)]
           [(and (eq? buffer #\))
                 (eq? (car modes) 'in-function))
            (tokenize-query-header out text (+ cursor 1)
              #:modes   (cdr modes)
              #:current '()
              #:tokens  (cons-token out current tokens))]
           [(eq? buffer #\")
            (tokenize-query-header out text (+ cursor 1)
              #:modes   (if (eq? (car modes) 'double-quoted)
                            (cdr modes)
                            (cons 'double-quoted modes))
              #:current (cons buffer current)
              #:tokens  tokens)]
           [(eq? buffer #\')
            (tokenize-query-header out text (+ cursor 1)
              #:modes   (if (eq? (car modes) 'single-quoted)
                            (cdr modes)
                            (cons 'single-quoted modes))
              #:current (cons buffer current)
              #:tokens  tokens)]

           [(and (char-whitespace? buffer)
                 (or (eq? (car modes) 'none)
                     (eq? (car modes) 'in-function)))
            (tokenize-query-header out text (+ cursor 1)
              #:modes   modes
              #:current '()
              #:tokens  (cons-token out current tokens))]
           [(and (eq? buffer #\{)
                 (eq? (car modes) 'none))
            (values (cons (list->string (reverse current)) tokens)
                    cursor)]
           [else
            (tokenize-query-header out text (+ cursor 1)
              #:modes   modes
              #:current (cons buffer current)
              #:tokens  tokens)]))))

  (define (read-global-graphs out tokens)

    (define (process-global-graph uri out)
      (cond
       [(is-absolute-uri? uri)
        (set-query-global-graphs! out
          (cons uri (query-global-graphs out)))]
       [else
        (set-query-global-graphs! out
          (cons (parse-uri-token out uri)
                (query-global-graphs out)))]))

    (for-each (lambda (item)
                (match item
                  (("FROM" . uri)
                   (process-global-graph uri out))
                  (("INTO" . uri)
                   (process-global-graph uri out))
                  (else #f)))
              tokens))

  (define (read-out-variables out tokens)
    (set-query-out-variables! out
     (reverse
      (delete #f (map (lambda (item-index)
                        (catch #t
                          (lambda _
                            (let [(item (list-ref tokens item-index))
                                  (next (list-ref tokens (- item-index 1)))]
                              (cond
                               [(and (string? item)
                                     (not (string= item ""))
                                     (not (string-ci= next "as"))
                                     (eq? (string-ref item 0) #\?))
                                item]
                               [else
                                #f])))
                          (lambda (key . args)
                            (let [(item (list-ref tokens item-index))]
                              (if (and (string? item)
                                       (not (string= item ""))
                                       (eq? (string-ref item 0) #\?))
                                  item
                                  #f)))))
                      (iota (length tokens)))))))

  (define (parse-select-query out query cursor)
    (call-with-values (lambda _ (tokenize-query-header out query cursor))
      (lambda (tokens cursor)
        (read-out-variables out tokens)
        (read-global-graphs out tokens)
        (call-with-values (lambda _ (tokenize-triplet-pattern out query cursor))
          (lambda (tokens cursor)
            (set-query-quads! out (reverse tokens)))))))

  (define (parse-ask-query out query cursor)
    (call-with-values (lambda _ (tokenize-triplet-pattern out query cursor))
      (lambda (tokens cursor)
        (set-query-quads! out (reverse tokens)))))

  (define (parse-clear-query out query cursor)
    (let* [(tokens (string-tokenize (substring query cursor)))
           (uri    (if (= (length tokens) 3)
                       (parse-uri-token out (list-ref tokens 2))
                       #f))]
      (if uri
          (set-query-global-graphs! out uri)
          #f)))

  (define (parse-describe-query out query cursor)
    (parse-select-query out query cursor))

  (define (parse-construct-query out query cursor)
    (call-with-values (lambda _ (tokenize-triplet-pattern out query cursor))
      (lambda (tokens cursor)
        (set-query-construct-patterns! out (reverse tokens))
        (parse-select-query out query cursor))))

  (define (parse-modify-query out query cursor)
    (call-with-values (lambda _ (tokenize-query-header out query cursor))
      (lambda (tokens cursor)
        (read-global-graphs out tokens)
        (call-with-values (lambda _ (tokenize-triplet-pattern out query cursor))
          (lambda (tokens cursor)
            (set-query-modify-patterns! out (reverse tokens))
            (parse-select-query out query cursor))))))

  (catch #t
    (lambda _
      (let* [(out (make <query>))]
        ;; The following functions write their findings to ‘out’ as
        ;; side-effects.
        (let* [(q              (remove-comments query))
               (after-prologue (read-prologue out q 0))
               (cursor         (determine-query-type out q after-prologue))]
          (match (query-type out)
            ('ASK           (parse-ask-query out q (+ cursor 3)))
            ('CLEAR         (parse-clear-query out q cursor))
            ('CONSTRUCT     (parse-construct-query out query (+ cursor 9)))
            ('DELETE        (parse-modify-query out query (+ cursor 6)))
            ('DELETEINSERT  (throw 'unimplemented-query-type #f))
            ('DESCRIBE      (parse-describe-query out query (+ cursor 8)))
            ('INSERT        (parse-modify-query out query (+ cursor 6)))
            ('SELECT        (parse-select-query out q (+ cursor 6)))
            (else           (throw 'unknown-query-type #f))))
        out))
    (lambda (key . args)
      (log-error "parse-query" "Thrown: ~a: ~s" key args)
      #f)))
