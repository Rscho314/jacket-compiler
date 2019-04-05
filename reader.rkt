#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt")

(provide read-syntax)

#;(define (read-syntax _ port)
  (datum->syntax #f '(module j-mod "expander.rkt" (display "I work"))))
(define (read-syntax _ port)
  (define token-list (rest (lex/j port)))
  (define module-datum `(module j-mod "expander.rkt" (handle-args ,@token-list)))
  (datum->syntax #f module-datum))

(module+ test
  (require rackunit)
  (check-eqv? (read-syntax "" (open-input-string "+01"))
                (datum->syntax
                 #f
                 '(module j-mod "expander.rkt" (handle-args ONE ZERO PLUS)))))