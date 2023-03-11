#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/type
  leo/compiler/type-utils)

(define (stack-of-type ($type : Type))
  (recursive
    (field! `stack
      (choice! 
        (field! `empty)
        (field! `link 
          (field! `top $type)
          (field! `tail (variable 0)))))))

(define stack-type 
  (stack-of-type (variable 1)))

(define racket-type
  (field! `racket))

(define field-type 
  (field! `field text-type (stack-of-type (variable 1))))

(define choice-type
  (field! `choice (stack-of-type (variable 1))))

(define arrow-type
  (field! `arrow
    (field! `from (variable 0))
    (field! `to (variable 0))))

(define a-type
  (field! `a (variable 0)))

(define specification-type
  (field! `specification 
    (field! `generic (variable 0))
    (field! `argument (variable 0))))

(define recursive-type
  (field! `recursive (variable 0)))

(define variable-type
  (field! `variable number-type))

(define type-type 
  (recursive
    (field! `type
      (choice!
        racket-type
        field-type
        arrow-type
        a-type
        specification-type
        recursive-type
        variable-type))))
