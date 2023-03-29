#lang leo/typed

(require
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
  (tuple : Tuple)
  (program : Program))

(define (program-compiler-sexp ($program-compiler : Program-Compiler)) : Sexp
  `(program
    (compiler
      ,(tuple-sexp (program-compiler-tuple $program-compiler))
      ,(program-sexp (program-compiler-program $program-compiler)))))

(define (program-compiler-plus-syntax
  ($program-compiler : Program-Compiler)
  ($syntax : Syntax))
: Program-Compiler
  (define $tuple (program-compiler-tuple $program-compiler))
  (define $program (program-compiler-program $program-compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $args
      (case $symbol
        ((use with)
          (define $ingredients (compile-ingredients-recursively $tuple $args))
          (define $binder-stack (usage-ingredients-binder-stack `indirect $ingredients))
          (define $entry-stack (filter-false (map binder-entry-option $binder-stack)))
          (define $binder-tuple (apply append (map binder-tuple $binder-stack)))
          (program-compiler
            (push-stack $tuple $binder-tuple)
            (program
              (push-stack (program-entry-stack $program) $entry-stack)
              (program-ingredients $program))))
        (else #f)))
    (program-compiler
      $tuple
      (program
        (program-entry-stack $program)
        (compiler-ingredients
          (compiler-plus-syntax
            (compiler
              $tuple
              (program-ingredients $program))
            $syntax))))))

(check-equal?
  (program-compiler-sexp
    (program-compiler-plus-syntax
      (program-compiler
        (tuple (expression syntax-a dynamic-type-a))
        (program
          (stack
            (entry (stack #`tmp-b) syntax-b))
          (ingredients
            (expressions syntax-c (structure dynamic-type-c)))))
      #`"foo"))
  (program-compiler-sexp
    (program-compiler
      (tuple (expression syntax-a dynamic-type-a))
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
        (tuple (expression syntax-a dynamic-type-a))
        (program
          (stack
            (entry (stack #`tmp-b) syntax-b))
          (ingredients
            (expressions syntax-c (structure dynamic-type-c)))))
      #`(use "foo" 128)))
  (program-compiler-sexp
    (program-compiler
      (tuple
        (expression syntax-a dynamic-type-a)
        (expression #`tmp-a dynamic-type-a))
      (program
        (stack
          (entry (stack #`tmp-b) syntax-b)
          (entry (stack #`tmp-a) #'(compiled "foo" 128)))
        (ingredients
          (expressions syntax-c (structure dynamic-type-c)))))))

(define (program-compiler-ingredients ($program-compiler : Program-Compiler)) : Ingredients
  (define $program (program-compiler-program $program-compiler))
  (define $entry-stack (program-entry-stack $program))
  (define $expressions (ingredients-expressions (program-ingredients $program)))
  (ingredients
    (expressions
      (entry-stack-do-syntax $entry-stack (expressions-syntax $expressions))
      (expressions-structure $expressions))))
