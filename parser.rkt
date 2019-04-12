#lang racket

(require "lex.rkt"
         parser-tools/yacc)

(provide parse/j)

(define parser/j
  ;; parser's only use is to parse arrays as a single entity (full lang unparsable)
  (parser
   (start start)
   (end EOF)
   (tokens tok)
   (error (lambda (tok-ok? tok-name tok-value)
            (display (list tok-ok? tok-name tok-value))))
   (grammar
    (start
     [(expr) $1]
     [() #f])
    (expr [(a) `(,$1)]
          [(non-a) `(,$1)]
          [(non-a expr) `,(cons $1 $2)]
          [(a non-a expr) `,(cons $1 (cons $2 $3))])
    (a [(ONE) '(1)]
       [(ZERO) '(0)]
       [(a ONE) `,(append $1 '(1))]
       [(a ZERO) `,(append $1 '(0))])
    (non-a [(PLUS) '+]))))

(define (parse p l s)
  (define (run)
    (port-count-lines! s)
    (p (λ () (l s))))
  (run))

(define (parse/j s)
  (parse parser/j lexer/j s))

(module+ test
  (require rackunit)

  (check-equal? (parse/j (open-input-string "0")) '((0)))
  (check-equal? (parse/j (open-input-string "1")) '((1)))
  (check-equal? (parse/j (open-input-string "1 0 0")) '((1 0 0)))
  (check-equal? (parse/j (open-input-string "+")) '(+))
  (check-equal? (parse/j (open-input-string "+1")) '(+ (1)))
  (check-equal? (parse/j (open-input-string "+0")) '(+ (0)))
  (check-equal? (parse/j (open-input-string "+ 1")) '(+ (1)))
  (check-equal? (parse/j (open-input-string "+ 1 0 1")) '(+ (1 0 1)))
  (check-equal? (parse/j (open-input-string "0 + 1 0 1")) '((0) + (1 0 1)))
  (check-equal? (parse/j (open-input-string "1 + 0 + 1 0 1")) '((1) + (0) + (1 0 1)))
  (check-equal? (parse/j (open-input-string "+ + 1 0 1")) '(+ + (1 0 1)))
  (check-equal? (parse/j (open-input-string "0 + + 1 0 1")) '((0) + + (1 0 1))))