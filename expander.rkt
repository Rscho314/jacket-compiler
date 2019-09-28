#| Untyped version
   ---------------
   This uses symbols to enforce "well-typedness".
   All "type checking" happens in syntax-classes.rkt.

   TODO
   ----
   + Macro stepper internal error: bug in math/array when using untyped racket
     (https://github.com/racket/math/issues/24)
   + How to handle the "anything" pattern in the J parse & execution table?
     - always null?
   + Handle names better (not by elimination as not part-of-speech)|#

#lang racket

(require (prefix-in tr:
                    (only-in typed/racket
                             #%module-begin
                             #%top-interaction))
         (for-syntax syntax/parse
                     "lex.rkt")
         "syntax-classes.rkt"
         "lib.rkt")

(provide (rename-out [module-begin/j #%module-begin]
                     [top-interaction/j #%top-interaction])
         j)

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
                                   name) ()))

;; SYNTAX INTERPRETER PROCEDURES
(define-syntax (monadic-zero/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (v expr1 expr2 expr3 ...)
        (pv _ _ pe3 ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        (v #,(local-expand #`(stx-env-ref/j expr1 expr2) 'expression #f) expr3 ...)
        (pv noun pe3 ...)
        expr
        env)]))

(define-syntax (monadic-one/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (v expr1 expr2 expr3)
        (pv _ _ _)
        expr
        env)
     #`(interpret-syntax-fragment/j
        (v expr1 #,(local-expand #`(stx-env-ref/j expr2 expr3) 'expression #f))
        (pv verb noun)
        expr
        env)]))

(define-syntax (dyadic-two/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (v expr1 expr2 expr3 r ...)
        (pv _ ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        (v #,(local-expand #`(stx-env-ref/j expr2 expr1 expr3) 'expression #f) r ...)
        (pv noun)
        expr
        env)]))

(define-syntax (is-seven/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (n:name/j =: x r ...)
        (name =: (~and (~var pos) (~or* noun verb)) p ...)
        expr
        env)
     #`(interpret-syntax-fragment/j
        (#,(local-expand #`(stx-env-ref/j n =: x) 'expression #f) r ...)
        (name p ...)
        expr
        #,(hash-set (syntax-e #`env) (syntax-e #`n) (syntax-e #`pos)))]))

;; SYNTAX INTERPRETER MAIN LOOP
(define-syntax (interpret-syntax-fragment/j stx)
  ; + Maintain a manual pos-stack: part-of-speech cannot be made a
  ;   type because types are unavailable before the end of expansion.
  ; + The value stack contains expanded program fragments as well as
  ;   unexpanded fragments, and therefore cannot use syntax class patterns
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    ; monadic application (pattern 0)
    [(_ vs (~and (~var ps) ((~or* newline-marker =:) verb noun _ ...)) expr env)
     #`(monadic-zero/j vs ps expr env)]
    ; monadic application (pattern 1)
    [(_ vs (~and (~var ps) ((~or* newline-marker =. =: adverb verb noun) verb verb noun)) expr env)
     #`(monadic-one/j vs ps expr env)]
    ; dyadic application (pattern 2)
    [(_ vs (~and (~var ps) ((~or* newline-marker =: verb noun) noun verb noun _ ...)) expr env)
     #`(dyadic-two/j vs ps expr env)]
    ; assignment (pattern 7)
    [(_ vs (~and (~var ps) (name =: (~or* verb noun) _ ...)) expr env)
     #`(is-seven/j vs ps expr env)]
    ; program termination condition
    [(_ (newline-marker v ...) (newline-marker (~or* noun verb name)) () env)
     #`(begin v ...)]
    ; empty program
    [(_ (newline-marker) (newline-marker) () env)
     #`(void)]
    ; program ending with a newline
    [(_ (newline-marker) (newline-marker) (~rest r) env)
     #`(interpret-syntax-fragment/j () () r env)]
    ; line termination condition for values
    ; place line result on value stack, reset pos stack, and continue
    [(_ (newline-marker v ...) (newline-marker (~or* noun verb name)) expr env)
     #`(interpret-syntax-fragment/j (v ...) () expr env)]
    ; end-of-line encounter (newline-marker) with blank line skip (= multiple newline-markers)
    [(_ (vs ...) (ps ...) (:newline-marker/j ...+ ~rest r) env)
     #`(interpret-syntax-fragment/j (newline-marker vs ...) (newline-marker ps ...) r env)]
    ; noun encounter: immediate expansion to racket array
    [(_ (vs ...) (ps ...) (e:noun/j ~rest r) env)
     #`(interpret-syntax-fragment/j (e.expansion vs ...) (noun ps ...) r env)]
    ; verb encounter: placement on stack as-is (execution context currently unknown)
    [(_ (vs ...) (ps ...) (e:verb/j ~rest r) env)
     #`(interpret-syntax-fragment/j (e vs ...) (verb ps ...) r env)]
    ; name encounter WRONG: names MUST be replaced by a part-of-speech,
    ; otherwise impossible to further recognize execution patterns!
    ; i.e. there must be a purely syntactic environment
    ; assignment case
    [(_ (=: vs ...) (=: ps ...) (e:name/j ~rest r) env)
     #`(interpret-syntax-fragment/j (e =: vs ...) (name =: ps ...) r env)]
    ; reference case
    [(_ (vs ...) (ps ...) (e:name/j ~rest r) env)
     #`(interpret-syntax-fragment/j (e vs ...)
                                    (#,(hash-ref (syntax-e #`env) (syntax-e #`e)) ps ...)
                                    r
                                    env)]
    ; assignment symbol encounter
    ; =: corresponds to define & provide
    ; =. corresponds to scoped define or let form
    [(_ (vs ...) (ps ...) (=: ~rest r) env)
     #`(interpret-syntax-fragment/j (=: vs ...) (=: ps ...) r env)]
    ; debug
    [(_ vs ps expr env)
     #`(raise-syntax-error 'interpret-syntax-fragment/j
                           (string-append "debug condition" "\n\t"
                                          "value stack:\t" (~a 'vs) "\n\t"
                                          "p-o-s stack:\t" (~a 'ps) "\n\t"
                                          "current exp:\t" (~a 'expr) "\n\t"
                                          "syntax env:\t" (~a 'env)))]))

(define-syntax (module-begin/j stx)
  (syntax-parse stx
    #:literal-sets (parts-of-speech+names)
    [(_ (newline-marker)) #`(tr:#%module-begin)] ; empty program
    [(_ exprs) #`(tr:#%module-begin
               (interpret-syntax-fragment/j () () exprs #,(hasheq)))]))

(define-syntax (top-interaction/j stx)
  (syntax-case stx ()
    [(_ . a) #`a]))

; This allows embedding into racket as a string (also useful for rackunit tests)
; However, breaking hygiene with syntax->datum is required (possible to restrict breakage to only 'define')?
; see https://stackoverflow.com/questions/49669142/what-is-difference-between-datum-syntax-and-syntax-in-define-syntax-body
; possibly required to manage '"': https://docs.racket-lang.org/udelim/index.html?q=udelim
(define-syntax (j stx)
      #`(interpret-syntax-fragment/j
         ()
         ()
         #,(datum->syntax stx (lex/j (open-input-string (cadr (syntax->datum stx)))))
         #,(hasheq)))