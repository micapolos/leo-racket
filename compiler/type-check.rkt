#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-check? ($actual : Type) ($expected : Type)) : Boolean
  (type-stack-type-check? null $actual $expected))

(define (structure-check? ($actual : Structure) ($expected : Structure)) : Boolean
  (type-stack-structure-check? null $actual $expected))

(define (type-stack-type-check? ($type-stack : (Stackof Type)) ($actual : Type) ($expected : Type)) : Boolean
  (or
    (and 
      (racket? $expected) 
      (racket? $actual))
    (and 
      (value? $expected) 
      (value? $actual)
      ; TODO: Should I use $type-stack here, or assume value types are top-level?
      (type-check? (value-type $actual) (value-type $expected))
      (equal? (value-any $actual) (value-any $expected)))
    (and 
      (field? $expected) 
      (field? $actual) 
      (equal? 
        (field-symbol $actual) 
        (field-symbol $expected))
      (type-stack-structure-check? 
        $type-stack
        (field-structure $actual) 
        (field-structure $expected)))
    (and
      (choice? $expected)
      (choice? $actual)
      (type-stack-structure-check?
        $type-stack
        (choice-type-stack $actual)
        (choice-type-stack $expected)))
    (and 
      (arrow? $expected)
      (arrow? $actual)
      (type-stack-structure-check? 
        $type-stack
        (arrow-from-structure $actual)
        (arrow-from-structure $expected))
      (type-stack-structure-check? 
        $type-stack
        (arrow-to-structure $actual)
        (arrow-to-structure $expected)))
    (and 
      (generic? $expected)
      #f) ; TODO: Push unique on the stack
    (and 
      (specific? $expected)
      (type-stack-type-check? 
        (push $type-stack (specific-argument-type $expected))
        $actual
        (specific-type $expected)))
    (and
      (recursive? $expected)
      (recursive? $actual)
      (type-stack-type-check? 
        (push $type-stack $expected)
        (recursive-type $actual)
        (recursive-type $expected)))
    (and 
      (variable? $expected)
      (type-stack-type-check?
        $type-stack
        $actual
        (list-ref $type-stack (variable-index $expected))))
    (and
      (universe? $expected)
      (universe? $actual)
      (= 
        (universe-index $actual)
        (universe-index $expected)))))

(define (type-check-symbol? ($type : Type) ($symbol : Symbol)) : Boolean
  (and
    (field? $type)
    (equal? (field-symbol $type) $symbol)))

(define (type-check-selector? ($type : Type) ($selector : Type)) : Boolean
  (or
    ;(and (a? $selector) (type-check? $type (a-type $selector)))
    (and
      (field? $selector) 
      (and (null? (field-structure $selector)))
      (type-check-symbol? $type (field-symbol $selector)))))

(define (type-stack-structure-check? 
  ($type-stack : (Stackof Type))
  ($actual : Structure)
  ($expected : Structure)) 
  : Boolean
  (and 
    (= (length $actual) (length $expected))
    (andmap (curry type-stack-type-check? $type-stack) $actual $expected)))

(define (type-apply-structure
  ($type : Type)
  ($structure : Structure))
  : (Option Structure)
  (and
    (arrow? $type)
    (structure-check? $structure (arrow-from-structure $type))
    (arrow-to-structure $type)))

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
