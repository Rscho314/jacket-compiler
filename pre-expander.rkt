#lang typed/racket

(provide pre-expand)

(: pre-expand (-> (Listof Any) Any))
(define (pre-expand args)
  (unless (null? args)
    (first (for/fold ([stack : (Listof Any) empty])
                     ([arg (in-list args)]
                      #:unless (void? arg))
             (cond
               [((make-predicate (Immutable-Vectorof Zero)) arg)
                (cons arg stack)]
               [((make-predicate (Immutable-Vectorof One)) arg)
                (cons arg stack)]
               #;[((make-predicate Any) arg) (cons arg stack)]
               [else (error "not the droid you're looking for.")])))))