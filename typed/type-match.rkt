#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type 
  leo/typed/testing)

(define (type-matches? ($actual : Type) ($expected : Type)) : Boolean
  (or
    (thing? $expected)
    (cond
      ((racket? $actual) (equal? $actual $expected))
      ((tuple? $actual) 
        (and 
          (tuple? $expected) 
          (equal? (tuple-symbol $actual) (tuple-symbol $expected))
          (types-match? (tuple-type-list $actual) (tuple-type-list $expected))))
      ((arrow? $actual) (equal? $actual $expected))
      ((any? $actual) (equal? $actual $expected))
      ((thing? $actual) (equal? $actual $expected)))))

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
  (arg-types-match-arrow?
    ($arg-types : (Listof Type)) 
    ($giving : Arrow))
  : Boolean
  (type-list-match? $arg-types (arrow-lhs-types $giving)))

(check-equal? (type-matches? (racket `foo) (thing)) #t)
(check-equal? (type-matches? (tuple `foo null) (thing)) #t)
(check-equal? (type-matches? (tuple `foo (list (racket `number))) (thing)) #t)
(check-equal? (type-matches? (arrow null (racket `number)) (thing)) #t)
(check-equal? (type-matches? (any (racket `foo)) (thing)) #t)
(check-equal? (type-matches? (thing) (thing)) #t)

(check-equal? (type-matches? (racket `foo) (racket `foo)) #t)
(check-equal? (type-matches? (racket `foo) (racket `not-foo)) #f)

(check-equal? 
  (type-matches? 
    (tuple `foo (list (racket `foo)))
    (tuple `foo (list (racket `foo))))
  #t)

(check-equal?
  (type-matches? 
    (tuple `foo (list (racket `foo)))
    (tuple `not-foo (list (racket `foo))))
  #f)

(check-equal?
  (type-matches? 
    (tuple `foo (list (racket `foo)))
    (tuple `foo (list (racket `not-foo))))
  #f)

(check-equal?
  (type-matches? 
    (tuple `foo (list (racket `foo)))
    (tuple `foo (list (racket `foo) (racket `bar))))
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
