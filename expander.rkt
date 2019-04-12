#lang turnstile



#;'((provide (rename-out [module-begin/j #%module-begin])         
         #%app
         #%datum
         #%top)

(require syntax/wrap-modbeg
         (for-syntax syntax/parse
                     racket))

(define-syntax module-begin/j
  (make-wrapping-module-begin #'handle-sentence))

(define-syntax (handle-sentence stx)
  (syntax-case stx ()
    [(handle-sentence a) #`((current-print) (#,interpret a))]))

(define-for-syntax (interpret args)
  (first (for/fold ([stack-acc empty])
                   ([arg args]
                    #:unless (void? arg))
           (cond
             [(equal? 'ZERO arg) (cons 0 stack-acc)]
             [(equal? 'ONE arg) (cons 1 stack-acc)]
             [(equal? 'PLUS arg)
              (define op-result
                (+ (first stack-acc) (second stack-acc)))
              (cons op-result (drop stack-acc 2))]
             [else (error "not the droid you're looking for.")])))))