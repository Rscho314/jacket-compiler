#lang typed/racket #:with-refinements

(require (only-in racket/unsafe/ops
                  unsafe-vector-ref))


(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Natural])
                #:pre (v n)
                (< n (vector-length v))
                A)))
(define safe-vector-ref unsafe-vector-ref)