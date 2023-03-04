#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-check? ($actual : Type) ($expected : Type)) : Boolean
  (equal? $actual $expected))

(define (structure-check? 
  ($actual : Structure)
  ($expected : Structure)) 
  : Boolean
  (and 
    (= (length $actual) (length $expected))
    (andmap type-check? $actual $expected)))

(define (type-apply-structure
  ($type : Type)
  ($structure : Structure))
  : (Option Structure)
  (and
    (arrow? $type)
    (structure-check? $structure (arrow-lhs-structure $type))
    (arrow-rhs-structure $type)))

(check-equal?
  (type-apply-structure
    (arrow (structure type-a) (structure type-b))
    (structure type-a))
  (structure type-b))

(check-equal?
  (type-apply-structure
    (arrow (structure type-a) (structure type-b))
    (structure type-b))
  #f)
