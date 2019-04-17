#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt")

(provide read-syntax)

(define (read-syntax _ port)
  (define semi-ast (lex/j port))
  #;(define module-datum `(module j-mod "expander.rkt" ,semi-ast))
  #;(datum->syntax #f module-datum)
  semi-ast)

(module+ test
  (require rackunit)
  (check-eqv? (read-syntax "" (open-input-string "0 1"))
                (datum->syntax
                 #f
                 '(module j-mod "expander.rkt" (handle-args ONE ZERO PLUS)))))