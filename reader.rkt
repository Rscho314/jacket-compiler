#lang racket

;; There is no parser bc J can't be parsed
;; Lexing still useful (can group multiple chars in single token)
(require "lex.rkt"
         "pre-expander.rkt")

(provide read-syntax)

;; the whole program is a list
;; each line is read as a sublist
#;(define (read-source port)
  (map
   (λ (s) (port->list read (open-input-string s)))
   (port->lines port #:close? #f)))

(define (read-source port)
  (port->list read port))

#;(define (read-source port)
  (define (rec port program)
    (let ([dat (read port)])
      (if (eof-object? dat)
          program
          (rec port (cons dat program)))))
  (rec port '()))

(define (read-syntax _ port)
  #;(define semi-ast (lex/j port))
  (define module-datum `(module mod/j "expander.rkt" ,(read-source port)))
  (datum->syntax #f module-datum))

(module+ test
  (require rackunit)
  (check-equal? (read-syntax "" (open-input-string "17 \n 18"))
                '()))