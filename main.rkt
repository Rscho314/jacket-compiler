#lang racket/base

(module+ test
  (require rackunit
           "expander.rkt"
           math/array)
  (check-equal? (j "a=:0") (array #(3))))

(module+ main)

(module+ reader
  (require "reader.rkt")
  (provide read-syntax))