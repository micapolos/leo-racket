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
      ((and (list? $actual) (list? $expected)) (types-match? $actual $expected))
      ((giving? $actual) (equal? $actual $expected))
      ((any? $actual) (equal? $actual $expected))
      ((thing? $actual) (equal? $actual $expected))
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
  (arg-types-match-giving?
    ($arg-types : (Listof Type)) 
    ($giving : Arrow))
  : Boolean
  (type-list-match? $arg-types (giving-lhs-types $giving)))

(check-equal? (type-matches? (racket `foo) (thing)) #t)
(check-equal? (type-matches? `foo (thing)) #t)
(check-equal? (type-matches? `(foo ,(racket `number)) (thing)) #t)
(check-equal? (type-matches? (giving null null) (thing)) #t)
(check-equal? (type-matches? (any (racket `foo)) (thing)) #t)
(check-equal? (type-matches? (thing) (thing)) #t)

(check-equal? (type-matches? (racket `foo) (racket `foo)) #t)
(check-equal? (type-matches? (racket `foo) (racket `not-foo)) #f)

(check-equal? 
  (type-matches? 
    `(foo ,(racket `foo))
    `(foo ,(racket `foo)))
  #t)

(check-equal?
  (type-matches? 
    `(foo ,(racket `foo))
    `(not-foo ,(racket `foo)))
  #f)

(check-equal?
  (type-matches? 
    `(foo ,(racket `foo))
    `(foo ,(racket `not-foo)))
  #f)

(check-equal?
  (type-matches? 
    `(foo ,(racket `foo))
    `(foo ,(racket `foo) ,(racket `bar)))
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
