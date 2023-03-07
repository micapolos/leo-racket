#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/module-syntax)

(define (package-top-level-syntax ($package : Package)) : Syntax
  (make-syntax 
    `(for-each writeln
      ,(expressions-syntax (package-top-level-expressions $package)))))

(define (package-top-level-expressions ($package : Package)) : Expressions
  (package-apply-fn $package tuple-top-level-expressions))

(define (tuple-top-level-expressions ($tuple : Tuple)) : Expressions
  (expressions 
    (tuple-top-level-syntax $tuple)
    (structure (racket))))

(define (tuple-top-level-syntax ($tuple : Tuple)) : Syntax
  (make-syntax
    `(list ,@(reverse (map expression-top-level-syntax $tuple)))))

(define (expression-top-level-syntax ($expression : Expression)) : Syntax
  (make-syntax
    `(value-sexp
      (value 
        ,(expression-syntax $expression) 
        ,(type-syntax (expression-type $expression))))))

(check-equal?
  (syntax->datum
    (package-top-level-syntax
      (package
        (expressions #`expr
          (structure number-type text-type)))))
  `(for-each
     writeln
     (let-values (((tmp-number tmp-text) expr))
       (list
        (value-sexp (value tmp-number (field 'number (structure (racket)))))
        (value-sexp (value tmp-text (field 'text (structure (racket)))))))))