#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/scope
  leo/compiler/binding
  leo/compiler/type
  leo/compiler/type-utils)

(define base-scope
  (scope
    (binding
      (arrow
        (structure 
          int-type
          (field `plus (structure int-type)))
        (structure int-type))
      `unsafe-fx+)
    (binding
      (arrow
        (structure 
          int-type
          (field `text null-structure))
        (structure text-type))
      `number->string)
    (binding
      (arrow
        (structure 
          text-type
          (field `plus (structure text-type)))
        (structure text-type))
      `string-append)))
