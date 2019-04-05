#lang racket

(provide (rename-out [module-begin/j #%module-begin])
         handle-args
         +)

(define-syntax-rule (module-begin/j exp)
  #'(#%module-begin
     (display (first exp))))

(define (handle-args . args)
  (for/fold ([stack-acc empty])
            ([arg (in-list args)]
             #:unless (void? arg))
    (cond
      [(equal? 'ZERO arg) (cons 0 stack-acc)]
      [(equal? 'ONE arg) (cons 1 stack-acc)]
      [(equal? 'PLUS arg)
       (define op-result
         (+ (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))])))


#;(handle-args 'ONE 'ZERO 'PLUS 'ONE 'PLUS)