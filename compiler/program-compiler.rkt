#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/syntax-match
  leo/compiler/expression
  leo/compiler/program
  leo/compiler/compiler
  leo/compiler/compiler-plus-syntax)

(data program-compiler
  (tuple : Tuple)
  (program : Program))

(define (program-compiler-plus-syntax
  ($program-compiler : Program-Compiler)
  ($syntax : Syntax))
: Program-Compiler
  (define $tuple (program-compiler-tuple $program-compiler))
  (define $program (program-compiler-program $program-compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $args
      (case $syntax
        ((use with) #f)
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
