#lang racket/base

(module+ test
  (require rackunit
           "expander.rkt"
           math/array)
  (check-equal? (j "3") (array #[3]))
  (check-equal? (j "^0") (array #[1]))
  (check-equal? (j "3^2") (array #[9])))

(module+ main)

(module+ reader
  (require "reader.rkt")
  (provide read-syntax))