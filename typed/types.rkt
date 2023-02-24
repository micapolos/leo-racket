#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/type)

(define void-type (native-type `void))
(define boolean-type (native-type `boolean))
(define number-type (native-type `number))
(define fixnum-type (native-type `fixnum))
(define flonum-type (native-type `flonum))
(define string-type (native-type `string))
