#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-symbol ($type : Type)) : (Option Symbol)
  (cond
    ((native-type? $type) 
      (define $any (native-type-any $type))
      (if (symbol? $any) $any #f))
    ((symbol-type? $type) (symbol-type-symbol $type))
    ((field-type? $type)
      (field-type-symbol $type))
    ((arrow-type? $type) `function)
    ((type-type? $type) `any)))

(check-equal? (type-symbol (native-type `foo)) `foo)
(check-equal? (type-symbol (native-type "non-symbol")) #f)
(check-equal? (type-symbol (symbol-type `foo)) `foo)
(check-equal? (type-symbol (void-field-type `foo)) `foo)
(check-equal? (type-symbol (arrow-type null null)) `function)
(check-equal? (type-symbol (type-type number-type)) `any)
