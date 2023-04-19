#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/types
  leo/compiler/expression-cast
  leo/compiler/expression-utils
  leo/compiler/syntax-utils)

(define (type-resolve ($type : Type) ($expression : Expression)) : (Option Expression)
  (expression-cast $expression $type))

(define (types-resolve ($types : Types) ($expression : Expression)) : (Option Expression)
  (bind $type-stack (types-stack $types)
    (cond
      ((null? $type-stack) #f)
      (else
        (or
          (type-resolve (car $type-stack) $expression)
          (types-resolve (types (cdr $type-stack)) $expression))))))

(define (types-apply ($types : Types) ($expression : Expression)) : Expression
  (or (types-resolve $types $expression) $expression))

(check-equal?
  (expression-sexp
    (types-apply
      (types!
        (choice! (field! `one) (field! `two))
        (recursive (field! `rec)))
      (expression syntax-a (field! `one))))
  (expression-sexp
    (expression
      #`#t
      (choice! (field! `one) (field! `two)))))

