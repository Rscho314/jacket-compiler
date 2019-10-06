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
    #:datum-literals (/ @: ^ =: monad dyad)
    ;adverbs
    [(_ /)
     #`(//j)]
    ;assignment
    [(_ n =: e:cav/j)
     #`(=:/j n #,(local-expand #`(prim-env-ref/j e) 'expression #f))]  ; literal case
    [(_ n =: e)
     #`(=:/j n e)]
    ;conjunctions
    [(_ u @: v)
     #`(@:/j u v)]
    ;verbs
    [(_ monad)
     #`(ann (λ (v y) (array-map v y)) (All (A) (-> (-> A A) (Array A) (Array A))))]
    [(_ dyad)
     #`(ann (λ (v x y) (array-map v x y)) (All (A) (-> (-> A A A) (Array A) (Array A) (Array A))))]
    [(_ ^) ; this is unfortunately necessary bc array-axis-fold reverses arguments for whatever reason...
     #`(^/j)]
    [(_ ^ _)
     #`(^/j _)]
    [(_ ^ _ _)
     #`(^/j _ _)]))

;IMPLEMENTATIONS
;adverbs
; (array #[1 'a]) is a legal program. Arrays are heterogenous in Racket.
(define-syntax (//j stx)
  (syntax-parse stx
    [(_)
     #`array-axis-fold]))

;assignment
(define-syntax (=:/j stx)
  (syntax-parse stx
    [(_ n e)
     #`(begin (define n e))]))

;conjunctions
(define-syntax (@:/j stx)
  (syntax-parse stx
    [(_ u v)
     #`(compose1
        #,(local-expand #'(prim-env-ref/j u _) 'expression #f)
        #,(local-expand #'(prim-env-ref/j v _) 'expression #f))]))

;verbs
(define-syntax (^/j stx)
  (syntax-parse stx
    [(_)
     #`(λ ([x : Number] [y : Number]) (expt y x))]
    [(_ _ _)
     #'(ann expt (-> Number Number Number))]
    [(_ _)
     #'(ann exp (-> Number Number))]))