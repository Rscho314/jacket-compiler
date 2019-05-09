#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (datum->syntax #f `(module mod/j "expander.rkt" ,(lex/j port))))

(module+ test
  (require rackunit)
  (check-equal? (read-syntax "" (open-input-string "^3^2 5"))
                '()))