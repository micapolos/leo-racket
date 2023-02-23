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
      ((symbol-type? $actual) (equal? $actual $expected))
      ((field-type? $actual) 
        (and 
          (field-type? $expected)
          (equal? (field-type-symbol $actual) (field-type-symbol $expected))
          (type-body-matches? (field-type-body $actual) (field-type-body $expected))))
      ((arrow-type? $actual) (equal? $actual $expected))
      ((type-type? $actual) (equal? $actual $expected))
      ((thing-type? $actual) (equal? $actual $expected)))))

(define (type-body-matches? ($actual : TypeBody) ($expected : TypeBody)) : Boolean
  (cond
    ((struct-type-body? $actual)
      (and
        (struct-type-body? $expected)
        (=
          (length (struct-type-body-type-list $actual))
          (length (struct-type-body-type-list $expected)))
        (andmap type-matches? 
          (struct-type-body-type-list $actual)
          (struct-type-body-type-list $expected))))
    ((choice-type-body? $actual)
      (error "TODO: Choice"))))

(define (type-match (actual : Type) (expected : Type))
  (if (type-matches? actual expected) 
    (void)
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))

(define (type-matching (actual : Type) (expected : Type)) : Type
  (if (type-matches? actual expected) 
    actual
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))

(check-equal? (type-matches? (native-type `foo) (thing-type)) #t)
(check-equal? (type-matches? (symbol-type `foo) (thing-type)) #t)
(check-equal? (type-matches? (field-type `foo (struct-type-body null)) (thing-type)) #t)
(check-equal? (type-matches? (arrow-type null null) (thing-type)) #t)
(check-equal? (type-matches? (type-type (native-type `foo)) (thing-type)) #t)
(check-equal? (type-matches? (thing-type) (thing-type)) #t)

(check-equal? (type-matches? (native-type `foo) (native-type `foo)) #t)
(check-equal? (type-matches? (native-type `foo) (native-type `not-foo)) #f)

(check-equal? 
  (type-matches? 
    (field-type `foo (struct-type-body (list (native-type `foo))))
    (field-type `foo (struct-type-body (list (native-type `foo)))))
  #t)

(check-equal?
  (type-matches? 
    (field-type `foo (struct-type-body (list (native-type `foo))))
    (field-type `not-foo (struct-type-body (list (native-type `foo)))))
  #f)

(check-equal?
  (type-matches? 
    (field-type `foo (struct-type-body (list (native-type `foo))))
    (field-type `foo (struct-type-body (list (native-type `not-foo)))))
  #f)

(check-equal?
  (type-matches? 
    (field-type `foo (struct-type-body (list (native-type `foo))))
    (field-type `foo (struct-type-body (list (native-type `foo) (native-type `bar)))))
  #f)
