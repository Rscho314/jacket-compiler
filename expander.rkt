#| Untyped version
   ---------------
   This uses symbols to enforce "well-typedness".
   All "type checking" happens in syntax-classes.rkt.

   TODO
   ----
   + Macro stepper internal error: bug? typed/racket required?
   + How to handle the "anything" pattern in the J parse & execution table?
     - always null?
   + Handle names better (not by elimination as not part-of-speech)|#

#lang racket

(require #;(prefix-in tr:
                    (only-in typed/racket
                             #%module-begin
                             #%top-interaction))
         (for-syntax syntax/parse)
         "syntax-classes.rkt"
         "lib.rkt")

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction]))

;; SYNTAX INTERPRETER DATASTRUCTURES
(begin-for-syntax
  (define-literal-set parts-of-speech+names
    #:for-syntax #:datum-literals (newline-marker
                                   =.
                                   =:
                                   lparen
                                   rparen
                                   noun
                                   verb
                                   adverb
                                   conjunction
                                   name) ())

  (define syntactic-environment
    ; env needs to be mutable, otherwise
    ; requires to explicitely pass it throughout
    ; the syntactic interpreter
    (make-hash)))

;; SYNTAX INTERPRETER PROCEDURES
(define-syntax (monadic-zero/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (v e1:verb/j e2 e3 ...)
        (pv _ _ pe3 ...)
        e)
     #`(interpret-syntax-fragment/j
        (v #,(local-expand #`(stx-env-ref/j e1 e2) 'expression #f) e3 ...)
        (pv noun pe3 ...) e)]))

(define-syntax (dyadic-two/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (v e1 e2:verb/j e3)
        (pv _ ...)
        e)
     #`(interpret-syntax-fragment/j
        (v #,(local-expand #`(stx-env-ref/j e2 e1 e3) 'expression #f))
        (pv noun) e)]))

(define-syntax (is-seven/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (n:name/j =: x _ ...)
        (name =: (~and (~var pos) (~or* noun verb)) p ...)
        e)
     #`(begin ; side-effect: insert binding into environment
         (interpret-syntax-fragment/j
          (#,(local-expand #`(stx-env-ref/j n =: x) 'expression #f))
          (name p ...)
          e)
         #,(hash-set! syntactic-environment (syntax-e #`n) (syntax-e #`pos))
         #;(displayln syntactic-environment))]))

(define-syntax (interpret-syntax-fragment/j stx)
  ; + Maintain a manual pos-stack: part-of-speech cannot be made a
  ;   type because types are unavailable before the end of expansion.
  ; + The value stack contains expanded program fragments as well as
  ;   unexpanded fragments, and therefore cannot use syntax class patterns
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    ; monadic application (pattern 0)
    [(_ vs (~and (~var ps) ((~or* newline-marker =:) verb noun _ ...)) e)
     #`(monadic-zero/j vs ps e)]
    ; dyadic application (pattern 2)
    [(_ vs (~and (~var ps) ((~or* newline-marker =: verb noun) noun verb noun _ ...)) e)
     #`(dyadic-two/j vs ps e)]
    ; assignment (pattern 7)
    [(_ vs (~and (~var ps) (name =: (~or* verb noun) _ ...)) e)
     #`(is-seven/j vs ps e)]
    ; termination condition for values (single value for now)
    [(_ (newline-marker v) (newline-marker (~or* noun verb name)) ())
     #`v]
    ; empty line
    [(_ (newline-marker) (newline-marker) ())
     #`(void)]
    ; end-of-line encounter (newline-marker) with blank line skip (= multiple newline-markers)
    [(_ (vs ...) (ps ...) (:newline-marker/j ...+ ~rest r))
     #`(interpret-syntax-fragment/j (newline-marker vs ...) (newline-marker ps ...) r)]
    ; noun encounter: immediate expansion to racket array
    [(_ (vs ...) (ps ...) (e:noun/j ~rest r))
     #`(interpret-syntax-fragment/j (e.expansion vs ...) (noun ps ...) r)]
    ; verb encounter: placement on stack as-is
    [(_ (vs ...) (ps ...) (e:verb/j ~rest r))
     #`(interpret-syntax-fragment/j (e vs ...) (verb ps ...) r)]
    ; name encounter WRONG: names MUST be replaced by a part-of-speech,
    ; otherwise impossible to further recognize execution patterns!
    ; i.e. there must be a purely syntactic environment
    ; assignment case
    [(_ (=: vs ...) (=: ps ...) (e:name/j ~rest r))
     #`(interpret-syntax-fragment/j (e =: vs ...) (name =: ps ...) r)]
    ; reference case
    [(_ (vs ...) (ps ...) (e:name/j ~rest r))
     #`(interpret-syntax-fragment/j (e vs ...)
                                    (#,(hash-ref syntactic-environment (syntax-e #`e)) ps ...)
                                    r)]
    ; assignment symbol encounter
    ; =: corresponds to define & provide
    ; =. corresponds to scoped define or let form
    [(_ (vs ...) (ps ...) (=: ~rest r))
     #`(interpret-syntax-fragment/j (=: vs ...) (=: ps ...) r)]
    ; debug
    [(_ vs ps e)
     #`(raise-syntax-error 'interpret-syntax-fragment/j
                           "debug condition"
                           (begin (displayln 'vs) (displayln 'ps) (displayln 'e)))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (newline-marker)) #`(#%module-begin)] ; empty program
    [(_ es) #`(#%module-begin
               (interpret-syntax-fragment/j () () es))]))

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))
