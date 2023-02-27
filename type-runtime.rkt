#lang racket/base

(provide 
  (all-defined-out)
  (all-from-out leo/typed/type)
  (all-from-out leo/typed/racket))

(require 
  leo/typed/racket
  leo/typed/type
  leo/typed/types)

(define boolean boolean-type)
(define number number-type)
(define fixnum fixnum-type)
(define flonum flonum-type)
(define string string-type)
