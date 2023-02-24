#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type 
  leo/typed/testing)

(define (type-matches? ($actual : Type) ($expected : Type)) : Boolean
  (or
    (thing-type? $expected)
    (cond
      ((native-type? $actual) (equal? $actual $expected))
      ((and (list? $actual) (list? $expected)) (types-match? $actual $expected))
      ((arrow-type? $actual) (equal? $actual $expected))
      ((type-type? $actual) (equal? $actual $expected))
      ((thing-type? $actual) (equal? $actual $expected))
      (else (equal? $actual $expected)))))

(define (types-match? ($actual : (Listof Type)) ($expected : (Listof Type))) : Boolean
  (and
    (= (length $actual) (length $expected))
    (andmap type-matches? $actual $expected)))

(define
  (type-list-match? 
    ($types : (Listof Type)) 
    ($other-types : (Listof Type)))
  : Boolean
  (and
    (= (length $types) (length $other-types))
    (andmap type-matches? $types $other-types)))

(define
  (arg-types-match-arrow-type?
    ($arg-types : (Listof Type)) 
    ($arrow-type : ArrowType))
  : Boolean
  (type-list-match? $arg-types (arrow-type-lhs-types $arrow-type)))

(check-equal? (type-matches? (native-type `foo) (thing-type)) #t)
(check-equal? (type-matches? `foo (thing-type)) #t)
(check-equal? (type-matches? `(foo ,(native-type `number)) (thing-type)) #t)
(check-equal? (type-matches? (arrow-type null null) (thing-type)) #t)
(check-equal? (type-matches? (type-type (native-type `foo)) (thing-type)) #t)
(check-equal? (type-matches? (thing-type) (thing-type)) #t)

(check-equal? (type-matches? (native-type `foo) (native-type `foo)) #t)
(check-equal? (type-matches? (native-type `foo) (native-type `not-foo)) #f)

(check-equal? 
  (type-matches? 
    `(foo ,(native-type `foo))
    `(foo ,(native-type `foo)))
  #t)

(check-equal?
  (type-matches? 
    `(foo ,(native-type `foo))
    `(not-foo ,(native-type `foo)))
  #f)

(check-equal?
  (type-matches? 
    `(foo ,(native-type `foo))
    `(foo ,(native-type `not-foo)))
  #f)

(check-equal?
  (type-matches? 
    `(foo ,(native-type `foo))
    `(foo ,(native-type `foo) ,(native-type `bar)))
  #f)

; -----------------------------------------------------------------------

(define (type-match (actual : Type) (expected : Type))
  (if (type-matches? actual expected) 
    (void)
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))

(define (type-matching (actual : Type) (expected : Type)) : Type
  (if (type-matches? actual expected) 
    actual
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))
