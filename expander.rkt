#| The execution stack won't exist at runtime, so can't use types
   in the expander (hence using #lang typed/racket not allowed.
   So, verification of the well-typedness of the stack at compile
   time forces us to use symbols. However, we then expand to type
   executable fragments with type-expander/lang, which enables a
   certain amount of a posteriori type-checking after expansion. |#

#lang racket

(require (prefix-in te: typed/racket)
         (for-syntax syntax/parse
                     syntax/apply-transformer
                     syntax/stx))

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define-syntax-class noun/j
    [pattern (n:number ...+)
             #:attr typed-expansion #`#(n ...)]))

(begin-for-syntax
 (define-syntax-class verb/j
  [pattern (~datum +)]))

(begin-for-syntax
  (define-syntax-class pos/j
    [pattern e:noun/j
             #:attr pos #'noun]
    [pattern e:verb/j
             #:attr pos #'verb]))

(define-syntax (interpret-syntax-fragment/j stx) ;stx: value value-stack pos-stack
  ; Maintain a manual pos-stack: part-of-speech cannot be made a
  ; type because types are unavailable before the end of expansion.
  ; macros that handle executable patterns
  ; 1- fetch the relevant bindings in the environment
  ; 2- use bindings to write the code and keep the result on the value stack
  ; 3- compute the part-of-speech of the resulting fragment and adjust pos-stack
  ; 4- recur to seek another executable pattern
  (syntax-parse stx
    #:datum-literals (noun verb §)
    ; monadic application (pattern 0)
    [(_ (vs ...) (§ verb noun) e)
     #;#`(monadic-pattern-zero vs ps e)
     #`'monadic]
    ;  termination condition (single value for now)
    [(_ (v) ((~literal §) (~or noun verb)) ())
     #`'v]
    ; end-of-line encounter (§)
    [(_ (vs ...) (ps ...) ((~literal §) ~rest r))
     #`(interpret-syntax-fragment/j (vs ...) (§ ps ...) r)]
    ; part-of-speech encounter
    [(_ (vs ...) (ps ...) (e:pos/j ~rest r))
     #`(interpret-syntax-fragment/j (e vs ...) (e.pos ps ...) r)]
    ; debug
    [(_ vs ps e)
     #`(raise-syntax-error 'interpret-syntax-fragment
                           "debug condition"
                           (list 'vs 'ps 'e))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    [(_ (§)) #`(te:#%module-begin)]
    [(_ es) #`(te:#%module-begin (interpret-syntax-fragment/j () () es))]))

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))
