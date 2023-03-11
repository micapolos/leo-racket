#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type)

(define (type-symbol ($type : Type)) : Symbol
  (symbol-stack-type-symbol null $type))

(define (symbol-stack-type-symbol ($symbol-stack : (Stackof Symbol)) ($type : Type)) : Symbol
  (cond
    ((field? $type) (field-symbol $type))
    ((choice? $type) `choice)
    ((racket? $type) `racket)
    ((arrow? $type) `recipe)
    ((specification? $type)
      (symbol-stack-type-symbol 
        (push $symbol-stack 
          (symbol-stack-type-symbol 
            $symbol-stack 
            (specification-argument-type $type)))
        (specification-generic-type $type)))
    ((recursive? $type) 
      (symbol-stack-type-symbol 
        (push $symbol-stack `variable)
        (recursive-type $type)))
    ((variable? $type) 
      (list-ref $symbol-stack (variable-index $type)))
    ((universe? $type) `universe)
    ((value? $type) `value)))

(check-equal? (type-symbol (field! `foo)) `foo)
(check-equal? (type-symbol (choice null)) `choice)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `recipe)
(check-equal? (type-symbol (specification (field! `foo) (field! `bar))) `foo)
(check-equal? (type-symbol (specification (variable 0) (field! `bar))) `bar)
(check-equal? (type-symbol (recursive (field! `foo))) `foo)
(check-equal? (type-symbol (recursive (variable 0))) `variable)
(check-equal? (type-symbol (universe 0)) `universe)
