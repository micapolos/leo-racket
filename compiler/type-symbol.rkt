#lang leo/typed

(require
  leo/compiler/type)

(define (type-symbol ($type : Type)) : Symbol
  (symbol-stack-type-symbol null $type))

(define (symbol-stack-type-symbol ($symbol-stack : (Stackof Symbol)) ($type : Type)) : Symbol
  (cond
    ((field? $type) (field-symbol $type))
    ((choice? $type) `choice)
    ((racket? $type) `racket)
    ((arrow? $type) `recipe)
    ((variable? $type)
      (stack-ref-default $symbol-stack (variable-index $type) `variable))
    ((generic? $type) 
      (symbol-stack-type-symbol $symbol-stack (generic-type $type)))
    ((specific? $type)
      (symbol-stack-type-symbol 
        (push $symbol-stack 
          (symbol-stack-type-symbol 
            $symbol-stack 
            (specific-argument-type $type)))
        (specific-type $type)))
    ((recursive? $type) 
      (symbol-stack-type-symbol 
        (push $symbol-stack `variable)
        (recursive-type $type)))
    ((universe? $type) `universe)
    ((value? $type) `value)))

(check-equal? (type-symbol (field! `foo)) `foo)
(check-equal? (type-symbol (choice null)) `choice)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `recipe)
(check-equal? (type-symbol (variable 0)) `variable)
(check-equal? (type-symbol (generic (field! `foo))) `foo)
(check-equal? (type-symbol (specific (field! `foo) (field! `bar))) `foo)
(check-equal? (type-symbol (specific (variable 0) (field! `bar))) `bar)
(check-equal? (type-symbol (recursive (field! `foo))) `foo)
(check-equal? (type-symbol (recursive (variable 0))) `variable)
(check-equal? (type-symbol (universe 0)) `universe)
