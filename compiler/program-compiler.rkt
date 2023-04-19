#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/expressions
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/program
  leo/compiler/syntax-utils
  leo/compiler/binder
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/compiler
  leo/compiler/compile-recursively
  leo/compiler/compiler-plus-syntax)

(data program-compiler
  (scope : Scope)
  (program : Program))

(define (program-compiler-sexp ($program-compiler : Program-Compiler)) : Sexp
  `(program
    (compiler
      ,(scope-sexp (program-compiler-scope $program-compiler))
      ,(program-sexp (program-compiler-program $program-compiler)))))

(define (program-compiler-plus-syntax
  ($program-compiler : Program-Compiler)
  ($syntax : Syntax))
: Program-Compiler
  (define $scope (program-compiler-scope $program-compiler))
  (define $program (program-compiler-program $program-compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $args
      (case $symbol
        ((use with)
          (define $ingredients (compile-ingredients-recursively $scope $args))
          (define $scoper-stack (ingredients-scoper-stack $ingredients))
          (define $entry-stack (filter-false (map scoper-entry-option $scoper-stack)))
          (define $scoper-scope (apply append (map scoper-scope $scoper-stack)))
          (program-compiler
            (push-stack $scope $scoper-scope)
            (program
              (push-stack (program-entry-stack $program) $entry-stack)
              (program-body-ingredients $program))))
        (else #f)))
    (program-compiler
      $scope
      (program
        (program-entry-stack $program)
        (compiler-ingredients
          (compiler-plus-syntax
            (compiler $scope (program-body-ingredients $program))
            $syntax))))))

(check-equal?
  (program-compiler-sexp
    (program-compiler-plus-syntax
      (program-compiler
        (scope (binding identifier-a dynamic-type-a))
        (program
          (stack
            (entry (stack #`tmp-b) syntax-b))
          (ingredients
            (expressions syntax-c (structure dynamic-type-c)))))
      #`"foo"))
  (program-compiler-sexp
    (program-compiler
      (scope (binding identifier-a dynamic-type-a))
      (program
        (stack
          (entry (stack #`tmp-b) syntax-b))
        (ingredients
          (expressions syntax-c (structure dynamic-type-c))
          (expressions #`"foo" (structure text-type)))))))

(check-equal?
  (program-compiler-sexp
    (program-compiler-plus-syntax
      (program-compiler
        (scope (binding identifier-a dynamic-type-a))
        (program
          (stack
            (entry (stack #`tmp-b) syntax-b))
          (ingredients
            (expressions syntax-c (structure dynamic-type-c)))))
      #`(use "foo" 128)))
  (program-compiler-sexp
    (program-compiler
      (scope
        (binding identifier-a dynamic-type-a)
        (binding #`tmp-a dynamic-type-a))
      (program
        (stack
          (entry (stack #`tmp-b) syntax-b)
          (entry (stack #`tmp-a) #'(compiled "foo" 128)))
        (ingredients
          (expressions syntax-c (structure dynamic-type-c)))))))

(define (program-compiler-ingredients ($program-compiler : Program-Compiler)) : Ingredients
  (program-ingredients (program-compiler-program $program-compiler)))
