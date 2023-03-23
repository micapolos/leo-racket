#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/syntax-match
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/program
  leo/compiler/binder
  leo/compiler/compiler
  leo/compiler/compile-ingredients
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
      (case $syntax
        ((use with)
          (define $ingredients (compile-ingredients $tuple $args))
          (define $binder-stack (usage-ingredients-binder-stack `indirect $ingredients))
          (define $entry-stack (filter-false (map binder-entry-option $binder-stack)))
          (define $binder-tuple (apply append (map binder-tuple $binder-stack)))
          (program-compiler
            (push-stack $tuple $binder-tuple)
            (program
              (push-stack (program-entry-stack $program) $entry-stack)
              (push-stack (program-ingredients $program) $ingredients))))
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
