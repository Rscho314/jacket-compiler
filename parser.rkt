#lang racket

(require "lex.rkt"
         parser-tools/yacc)

(provide parse/j)

(define parser/j
  (parser
   (start start)
   (end EOF)
   (tokens tok)
   (error (lambda (tok-ok? tok-name tok-value)
            (display (list tok-ok? tok-name tok-value))))
   (grammar
    (start
     [(e) $1]
     [() #f])
    (e [(f a) `(,$1 ,$2)])
    (a [(ONE) '(array 1)]
       [(ZERO) '(array 0)]
       [(a ONE) (append $1 '(1))]
       [(a ZERO) (append $1 '(0))])
    (f [(PLUS) '+]))))

(define (parse p l s)
#;(define ip (open-input-string s))
  (define (run)
    (port-count-lines! s)
    (p (λ () (l s))))
  (run))

(define (parse/j s)
  (parse parser/j lexer/j s))

(module+ test
  (require rackunit)

  (check-equal? (parse/j (open-input-string "+1")) '(+ 1))
  (check-equal? (parse/j (open-input-string "+0")) '(+ 0))
  (check-equal? (parse/j (open-input-string "+ 1")) '(+ 1))
  (check-equal? (parse/j (open-input-string "+ 1 0 1")) '(+ 1 0 1)))