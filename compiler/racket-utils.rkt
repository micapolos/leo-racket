#lang typed/racket/base

(provide (all-defined-out))

(require leo/compiler/racket)

(define boolean-racket (racket `boolean))
(define number-racket (racket `number))
(define fixnum-racket (racket `fixnum))
(define flonum-racket (racket `flonum))
(define string-racket (racket `string))
