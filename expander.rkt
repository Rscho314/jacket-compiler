#| The execution stack won't exist at runtime, so can't use types
   in the expander (hence using #lang typed/racket not allowed.
   So, verification of the well-typedness of the stack at compile
   time forces us to use symbols. However, we then expand to type
   executable fragments with type-expander/lang, which enables a
   certain amount of a posteriori type-checking after expansion. |#

#lang racket

(require (prefix-in te: typed/racket)
         (for-syntax syntax/parse
                     syntax/stx))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define-syntax-class noun/j
    [pattern (n:number ...+)
             #:attr v #`#(n ...)
             #:attr t #'(te:Vectorof te:Number)]))

(begin-for-syntax
 (define-syntax-class verb/j
  [pattern (~datum +)
           #:attr v #`'+]))

(begin-for-syntax
  (define-syntax-class adverb/j
    [pattern (~literal dot)
             #:attr v #`dot])) ;cannot use "." bc belongs to racket syntax

(begin-for-syntax
 (define-syntax-class conjunction/j
  [pattern (~datum &)
           #:attr v #`'&]))

;; no punctuation syntax class, since "(" and ")" should behave the same as in racket (order of execution?)

(begin-for-syntax
 (define-syntax-class copula/j
  [pattern (~datum =:)
           #:attr v #`'=:]))

(begin-for-syntax
  (define-syntax-class pos/j
    [pattern e:noun/j
             #:attr val #`e.v
             #:attr ty  #`e.t
             #:attr pos #''noun]
    #;[pattern e:verb/j
             #:attr val #`e.v
             #:attr ty  #`e.t
             #:attr pos #''verb]
    #;[pattern e:adverb/j
             #:attr val #`e.v
             #:attr ty  #`e.t
             #:attr pos #''adverb]
    #;[pattern e:conjunction/j
             #:attr val #`e.v
             #:attr ty  #`e.t
             #:attr pos #''conjunction]
    #;[pattern e:copula/j
             #:attr val #`e.v
             #:attr ty  #`e.t
             #:attr pos #''copula]))

(define-for-syntax (stx-cons stx stx-list)
  (cons stx (syntax->list stx-list)))

(define-syntax (interpret-syntax stx) ;stx: value value-stack pos-stack
  ; Maintain a manual pos-stack: part-of-speech cannot be made a
  ; type because types are unavailable before the end of expansion.
  ; macros that handle executable patterns
  ; 1- fetch the relevant bindings in the environment
  ; 2- use bindings to write the code and keep the result on the value stack
  ; 3- compute the part-of-speech of the resulting fragment and adjust pos-stack
  ; 4- recur to seek another executable pattern
  (syntax-parse stx
    #:datum-literals (noun verb adverb conjunction copula § lparen)
    [(_ _ _ ((~or § copula lparen) verb noun (~optional _)))
     #`(monadic-pattern-zero value value-stack pos-stack)]
    [(_ e:pos/j vs ps)
     #`(#,(stx-cons #`(te:ann e.val e.ty) #`vs) . #,(stx-cons #`e.pos #`ps))]))

(define-syntax (expand/j stx)
  ; like expand/j, interpret-syntax has 1 syntax arg composed of 3 fragments
  (syntax-parse stx
    [(_ value-stack pos-stack (e:noun/j ~rest r))
     (let* ([new-stx #`#,(local-expand #`(interpret-syntax e value-stack pos-stack)
                                     'expression #f)]
            [new-value-stack #`#,(stx-car new-stx)]
            [new-pos-stack #`#,(stx-cdr new-stx)])
       #`'#,new-value-stack)]

    #;[(_ s (e:verb/j ~rest r))
     (let ([new-stack (interpret-syntax #`e.v 'verb (syntax->datum #`s))]) #`(expand/j #,new-stack r))]
    #;[(_ s (§ ~rest r))
     (let ([new-stack (interpret-syntax #`'§ 'start-of-line (syntax->datum #`s))]) #`(expand/j #,new-stack r))]
    #;[(_ s ())
     (interpret-syntax '() '() (syntax->datum #`s))]))



(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (§)) #`(te:#%module-begin)]
    [(_ es) #`(te:#%module-begin
               (expand/j () () es))])) ;this does in fact pass a single syntax fragment

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))
