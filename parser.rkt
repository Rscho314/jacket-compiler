#lang racket

(require "lex.rkt"
         parser-tools/yacc)

(define j-parser
  (parser
   (start start)
   (end EOF)
   (tokens val prim)
   (error (lambda (tok-ok? tok-name tok-value)
            (display (list tok-ok? tok-name tok-value))))
   (grammar
    (start
     [(e) $1]
     [() #f])
    (e [(f d) `(apply ,$1 ,$2)])
    (d [(ONE) `(string->number ,$1)]
       [(ZERO) `(string->number ,$1)])
    (f [(PLUS) '+]))))

(define (parse p l s)
(let ([ip (open-input-string s)])
  (define (run)
    (port-count-lines! ip)
    (p (λ () (l ip))))
  (run)))

(define (j-parse s)
  (parse j-parser j-lexer s))

(module+ test
  (require rackunit)

  (check-equal? (j-parse "+1") '(apply + (string->number "1")))
  (check-equal? (j-parse "+0") '(apply + (string->number "0")))
  (check-equal? (j-parse "+ 1") '(apply + (string->number "1"))))