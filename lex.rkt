#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex/j)

;; super chobo, must improve!
(define (read-vec s)
  (define sv
    (string-join (list s) #:before-first "(" #:after-last ")"))
  (read (open-input-string sv)))

(define-lex-abbrevs
  ;; arrays
  [vecone (:: #\1 (:* (:: #\space #\1)))]
  [veczero (:: #\0 (:* (:: #\space #\0)))]
  [vecouz (::
           (:or
            (:: vecone #\space veczero)
            (:: veczero #\space vecone))
           (:*
            (:or
             (:: vecone #\space veczero)
             (:: veczero #\space vecone))))]
  ;; base values
  )

(define lexer/j
  (lexer
   ;; arrays
   [vecouz (read-vec lexeme)]
   [vecone (read-vec lexeme)]
   [veczero (read-vec lexeme)]

   [#\+ '+]
   
   [#\space (lexer/j input-port)]
   [(eof) '()]))

(define (lex/j ip)
  (define (run acc)
    (let ([tok (lexer/j ip)])
      (if (equal? tok '())
          acc
          (run (cons tok acc)))))
  (run '()))

(module+ test
  (require rackunit)
  (check-equal? (lex/j (open-input-string "+0 1")) '()))