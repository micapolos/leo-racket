#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type 
  leo/typed/racket)

(define void-type (racket `void))
(define boolean-type (racket `boolean))
(define number-type (racket `number))
(define fixnum-type (racket `fixnum))
(define flonum-type (racket `flonum))
(define string-type (racket `string))
