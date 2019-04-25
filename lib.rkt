#lang type-expander #:with-refinements

(require (only-in racket/unsafe/ops
                  unsafe-vector-ref)
         (for-syntax syntax/stx
                     racket))


(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Natural])
                #:pre (v n)
                (< n (vector-length v))
                A)))
(define safe-vector-ref unsafe-vector-ref)

(define-type-expander (HomogeneousList stx)
  (syntax-case stx ()
    [(_ t n)
     (number? (syntax-e #'n))
     (with-syntax ([(t ...) (stx-map (const #'t)
                                      (range (syntax-e #'n)))])
       #'(List t ...))]))