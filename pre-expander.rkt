#lang racket

(require syntax/parse)

(provide pre-expand)

(define (make-syntax-list arglist)
  (map
   (λ (arg) (datum->syntax #f arg))
   arglist))

(define (pre-expand arglist)
  (define syntax-list (make-syntax-list arglist))
  (map
   (λ (s) (apply-type s))
   syntax-list))

(define apply-type
  (syntax-parser
    [((~literal o) i:integer ...+) 'VecOne]
    [((~literal z) i:integer ...+) 'VecZero]
    [((~literal oz) i:integer ...+) 'VecOZ]))

(module+ test
  (require rackunit)
  (check-equal? (pre-expand '((oz 1 0))) '())
  (check-equal? (pre-expand '((oz 1 0) (z 0 0 0))) '()))