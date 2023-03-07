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
          number-type
          (field `plus (structure number-type)))
        (structure number-type))
      #`+)
    (binding
      (arrow
        (structure 
          number-type
          (field `minus (structure number-type)))
        (structure number-type))
      #`-)
    (binding
      (arrow
        (structure 
          number-type
          (field `times (structure number-type)))
        (structure number-type))
      #`*)
    (binding
      (arrow
        (structure 
          number-type
          (a text-type))
        (structure text-type))
      #`number->string)
    (binding
      (arrow
        (structure 
          int-type
          (a text-type))
        (structure text-type))
      #`number->string)
    (binding
      (arrow
        (structure 
          int-type
          (field `plus (structure int-type)))
        (structure int-type))
      #`unsafe-fx+)
    (binding
      (arrow
        (structure 
          int-type
          (field `minus (structure int-type)))
        (structure int-type))
      #`unsafe-fx-)
    (binding
      (arrow
        (structure 
          int-type
          (field `times (structure int-type)))
        (structure int-type))
      #`unsafe-fx*)
    (binding
      (arrow
        (structure 
          int-type
          (a text-type))
        (structure text-type))
      #`number->string)
    (binding
      (arrow
        (structure 
          text-type
          (field `plus (structure text-type)))
        (structure text-type))
      #`string-append)
    (binding
      (arrow
        (structure 
          text-type
          (null-field `length))
        (structure 
          (field `length (structure number-type))))
      #`string-length)))
