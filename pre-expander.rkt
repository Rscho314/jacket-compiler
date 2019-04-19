#| This works as a basic demo, but the type-expander lib uses typed/racket
   and has no 'require/typed', so only the base language is available.
   solution 1: bear with that, implement everything with syntax-case and friends
               (no libs, but more secure type expansion).
   solution 2: do not use type-expander, and instead expand to types from scratch
               (all libs, but less secure type expansion).
   Since we're using the official typed/racket expander anyway, solution 2 is
   probably better since:
    - type errors or ill-formed programs caught anyway
    - more freedom (type-expander cannot do true recursive types)
    - less dependencies, use only basic racket libs
|#

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
    [((~literal o) i:integer ...+) (syntax->datum #'(list 'VecOne i ...))]
    [((~literal z) i:integer ...+) #'VecZero]
    [((~literal oz) i:integer ...+) #'VecOZ]))

(module+ test
  (require rackunit)
  (check-equal? (pre-expand '((oz 1 0))) '())
  (check-equal? (pre-expand '((oz 1 0) (z 0 0 0))) '()))