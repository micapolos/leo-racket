#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-part
  leo/compiler/expressions-part-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/module-syntax)

(define top-level-string? : (Parameter Boolean) (make-parameter #f))

(define (expressions-part-top-level-syntax ($expressions-part : Expressions-Part)) : Syntax
  (cond
    ((top-level-string?)
      (make-syntax 
        `(for-each
          (lambda ($sexp) (displayln (sexp-string $sexp)))
          ,(expressions-syntax (expressions-part-top-level-expressions $expressions-part)))))
    (else
      (make-syntax 
        `(for-each writeln
          ,(expressions-syntax (expressions-part-top-level-expressions $expressions-part)))))))

(define (expressions-part-top-level-expressions ($expressions-part : Expressions-Part)) : Expressions
  (expressions-part-apply-fn $expressions-part tuple-top-level-expressions))

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
    (expressions-part-top-level-syntax
      (expressions-part
        (expressions #`expr
          (structure number-type text-type)))))
  `(for-each
     writeln
     (let-values (((tmp-number tmp-text) expr))
       (list
        (value-sexp (value tmp-number (field 'number (structure (racket)))))
        (value-sexp (value tmp-text (field 'text (structure (racket)))))))))