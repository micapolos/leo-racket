#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/type)

(define void-type (native-type `void))
(define boolean-type (native-type `boolean))
(define number-type (native-type `number))
(define string-type (native-type `string))

(define void-type-body (struct-type-body null))
(define nothing-type-body (choice-type-body null))

(define (void-field-type ($symbol : Symbol))
  (field-type $symbol void-type-body))
