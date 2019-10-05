#lang typed/racket

; For simplicity, everything is an array fo now.
; This will have to change in the future.
; There is no type checking whatsoever here.

(require math/array
         (for-syntax syntax/parse)
         "syntax-classes.rkt")

(provide prim-env-ref/j)

;FUNCTION DISPATCH
(define-syntax (prim-env-ref/j stx)
  (syntax-parse stx
    #:datum-literals (/ + ^ =:)
    ;adverbs
    [(_ vn /)
     #`(//j vn)]
    ;assignment
    [(_ n =: e:cav/j)
     #`(=:/j n #,(local-expand #`(prim-env-ref/j e) 'expression #f))]  ; literal case
    [(_ n =: e)
     #`(=:/j n e)]
    ;verbs
    [(_ + n ...)
     #`(+/j n ...)]
    [(_ ^ n ...)
     #`(^/j n ...)]))

;IMPLEMENTATIONS
;adverbs
; (array #[1 'a]) is a legal program. Arrays are heterogenous in Racket.
(define-syntax (//j stx)
  (syntax-parse stx
    [(_ vn)
     #`((ann (λ (f n) (λ (a) (array-axis-fold a n f))) (All (T) (-> (-> T T T) Index (-> (Array T) (Array T)))))
        #,(local-expand #`(prim-env-ref/j vn) 'expression #f) 0)])) ; '0' as here bc ranks not implemented yet

;assignment
(define-syntax (=:/j stx)
  (syntax-parse stx
    [(_ n e)
     #`(begin (define n e))]))

;verbs
(define-syntax (^/j stx)
  (syntax-parse stx
    [(_ v1 v2)
     #`(array-map expt v1 v2)]
    [(_ v)
     #`(array-map exp v)]
    [(_)
     #`(λ ([v1 : Number] [v2 : Number]) (expt v2 v1))]))