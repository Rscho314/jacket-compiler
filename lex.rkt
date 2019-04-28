#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j)

;; super chobo, must improve!
(define (read-vec s)
  (define sv
    (string-join (list s) #:before-first "(" #:after-last ")"))
  (read (open-input-string sv)))

(define-lex-abbrevs
  ;; arrays
  [vecnum (:: (:+ numeric) (:* (:: #\space (:+ numeric))))]
  ;; base values
  )

(define lexer/j
  (lexer
   ;; arrays
   [vecnum (read-vec lexeme)]

   [#\^ '^]

   [#\( 'lparen]
   [#\) 'rparen]
   [#\newline '§]
   [#\space (lexer/j input-port)]
   [(eof) '()]))

(define (lex/j ip)
  (define (run acc)
    (let ([tok (lexer/j ip)])
      (if (equal? tok '())
          acc
          (run (cons tok acc)))))
  (run '(§))) ;start with an start-of-line marker (see J parsing and exec II)

(module+ test
  (require rackunit)
  (check-equal? (lex/j (open-input-string "10 2 3 () \n")) '()))