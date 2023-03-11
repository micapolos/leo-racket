#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  racket/list
  leo/typed/option
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils)

(define-type Match (Stackof (Option Type)))

(define (type-check? ($actual : Type) ($expected : Type)) : Boolean
  (and (type-match null $actual $expected) #t))

(define (structure-check? ($actual : Structure) ($expected : Structure)) : Boolean
  (and (structure-match null $actual $expected) #t))

(define (type-match ($match : Match) ($actual : Type) ($expected : Type)) : (Option Match)
  (or
    (and 
      (racket? $expected) 
      (racket? $actual)
      $match)
    (and 
      (value? $expected) 
      (value? $actual)
      (type-check? (value-type $actual) (value-type $expected))
      (equal? (value-any $actual) (value-any $expected))
      $match)
    (and 
      (field? $expected) 
      (field? $actual) 
      (equal? 
        (field-symbol $actual) 
        (field-symbol $expected))
      (structure-match
        $match
        (field-structure $actual) 
        (field-structure $expected))
      $match)
    (and
      (choice? $expected)
      (choice? $actual)
      (type-stack-match
        $match
        (choice-type-stack $actual)
        (choice-type-stack $expected)))
    (and 
      (arrow? $expected)
      (arrow? $actual)
      (option-app structure-match
        (structure-match
          $match
          (arrow-from-structure $actual)
          (arrow-from-structure $expected))
        (arrow-to-structure $actual)
        (arrow-to-structure $expected)))
    (and 
      (generic? $expected)
      (type-match
        (push $match #f)
        $actual
        (generic-type $expected)))
    (and 
      (specific? $expected)
      (type-match
        (push $match (specific-argument-type $expected))
        $actual
        (specific-type $expected)))
    (and
      (recursive? $expected)
      (recursive? $actual)
      (type-match
        (push $match $expected)
        (recursive-type $actual)
        (recursive-type $expected)))
    (and 
      (variable? $expected)
      (bind $type-option (list-ref $match (variable-index $expected))
        (or
          (and $type-option (type-match $match $actual $type-option))
          (list-set
            $match
            (variable-index $expected)
            $actual))))
    (and
      (universe? $expected)
      (universe? $actual)
      (= 
        (universe-index $actual)
        (universe-index $expected))
      $match)))

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

(define (type-stack-match
  ($match : Match)
  ($actual-type-stack : (Stackof Type))
  ($expected-type-stack : (Stackof Type)))
  : (Option Match)
  (and 
    (= (length $actual-type-stack) (length $expected-type-stack))
    (foldl
      (lambda (($actual-type : Type) ($expected-type : Type) ($match-option : (Option Match)))
        (and $match-option
          (type-match $match-option $actual-type $expected-type)))
      $match
      $actual-type-stack
      $expected-type-stack)))

(define structure-match type-stack-match)

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
