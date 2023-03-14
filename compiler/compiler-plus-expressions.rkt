#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/compiler
  leo/compiler/scope
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-part
  leo/compiler/expressions-part-utils
  leo/compiler/expressions-part-sexp
  leo/compiler/scope-resolve
  leo/compiler/type
  leo/compiler/type-utils)

(define (compiler-plus-quoted-tuple
  ($compiler : Compiler) 
  ($tuple : Tuple)) : Compiler
  (struct-copy compiler $compiler
    (expressions-part 
      (expressions-part-plus-tuple 
        (compiler-expressions-part $compiler) 
        $tuple))))

(define (compiler-plus-expressions
  ($compiler : Compiler) 
  ($expressions : Expressions)) : Compiler
  (struct-copy compiler $compiler
    (expressions-part 
      (scope-apply-expressions-part 
        (compiler-scope $compiler)
        (expressions-part-plus 
          (compiler-expressions-part $compiler) 
          $expressions)))))

(define (scope-apply-expressions-part
  ($scope : Scope) 
  ($expressions-part : Expressions-Part)) : Expressions-Part
  (or
    (expressions-part-resolve-fn $expressions-part
      (curry scope-or-tuple-resolve-tuple $scope))
    $expressions-part))

(define (scope-or-tuple-resolve-tuple
  ($scope : Scope) 
  ($tuple : Tuple)) : (Option Expressions)
  (or
    (scope-resolve-tuple $scope $tuple)
    (tuple-resolve $tuple)))

(check-equal?
  (expressions-part-sexp
    (compiler-expressions-part
      (compiler-plus-expressions
        (compiler null-scope expressions-a)
        expressions-b)))
  `(expressions-part
    (expressions
      (let-values (((tmp-a) a)) (let-values (((tmp-b) b)) (values tmp-a tmp-b)))
      (structure (a racket) (b racket)))))

(check-equal?
  (expressions-part-sexp
    (compiler-expressions-part
      (compiler-plus-expressions
        (compiler
          (scope
            (binding
              (arrow
                (structure text-type (field `plus (structure text-type)))
                (structure text-type))
              #`string-append))
          (expression-expressions (text-expression "Hello, ")))
        (expression-expressions 
          (field-expression `plus 
            (tuple (text-expression "world!")))))))
  `(expressions-part
    (expressions
      (let-values (((tmp-text tmp-plus)
                  (let-values (((tmp-text) "Hello, "))
                    (let-values (((tmp-plus) "world!"))
                      (values tmp-text tmp-plus)))))
      (#%app string-append tmp-text tmp-plus))
    (structure text))))
