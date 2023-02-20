#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/type)

(define void-type (native-type `void))
(define boolean-type (native-type `boolean))
(define number-type (native-type `number))
(define fixnum-type (native-type `fixnum))
(define flonum-type (native-type `flonum))
(define string-type (native-type `string))

(define void-type-body (struct-type-body null))
(define nothing-type-body (choice-type-body null))

(define (void-field-type ($symbol : Symbol))
  (field-type $symbol void-type-body))
