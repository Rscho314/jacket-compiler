#lang typed/racket

(provide pre-expand)

(define stack : (Listof Any) '())

(define specialize
  (case-lambda
    [([stack : (Listof Any)]
      [args : (Listof Any)])
     (first
      (for/fold ([stack : (Listof Any) empty])
                ([arg (in-list args)])
        (cond
          [((make-predicate (Immutable-Vectorof Zero)) arg)
           (cons arg stack)]
          [((make-predicate (Immutable-Vectorof One)) arg)
           (cons arg stack)]
          [else (error "error")])))]
    ;; case for empty program
    [([stack : (Listof Any)]
      [args : Void])
     (void)]))

(: pre-expand (-> (Listof Any) Any))
(define (pre-expand args)
  (specialize '()
              args))