#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/testing
  leo/compiler/generate-temporary
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils)

(data entry
  (identifier-stack : (Stackof Identifier))
  (syntax : Syntax))

(data binder
  (entry-option : (Option Entry))
  (tuple : Tuple))

(define (entry-sexp ($entry : Entry)) : Sexp
  `(entry
    (identifiers ,@(reverse (map syntax->datum (entry-identifier-stack $entry))))
    (syntax ,(syntax->datum (entry-syntax $entry)))))

(define (binder-sexp ($binder : Binder)) : Sexp
  `(binder
    ,(option-app entry-sexp (binder-entry-option $binder))
    ,(tuple-sexp (binder-tuple $binder))))

(define (expressions-binder ($expressions : Expressions) ($single-use? : Boolean #f)) : Binder
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $type-option (single $structure))
  (cond
    ((and $single-use? $type-option)
      (binder #f 
        (tuple (expression $syntax $type-option))))
    ((syntax-atomic? $syntax) 
      (binder #f 
        (map (curry expression $syntax) $structure)))
    (else
      (define $tmp-option-stack (map type-generate-temporary-option $structure))
      (define $identifier-stack (filter-false $tmp-option-stack))
      (define $entry (entry $identifier-stack $syntax))
      (define $tuple
        (map 
          (lambda (($tmp-option : (Option Identifier)) ($type : Type)) : Expression
            (expression 
              (or $tmp-option null-syntax)
              $type))
          $tmp-option-stack
          $structure))
      (binder $entry $tuple))))

(check-equal?
  (binder-sexp
    (expressions-binder
      (expressions null-syntax 
        (structure static-type-a static-type-b))))
  `(binder #f 
    (tuple 
      (expression #f a) 
      (expression #f b))))

(check-equal?
  (binder-sexp
    (expressions-binder
      (expressions complex-syntax-a
        (structure dynamic-type-a))))
  `(binder
    (entry (identifiers tmp-a) #'(complex-a))
    (tuple (expression tmp-a (a racket)))))

(check-equal?
  (binder-sexp
    (expressions-binder
      (expressions complex-syntax-a
        (structure dynamic-type-a))
      #t))
  `(binder #f (tuple (expression (complex-a) (a racket)))))

(check-equal?
  (binder-sexp
    (expressions-binder
      (expressions atomic-syntax-a
        (structure dynamic-type-a dynamic-type-b))))
  `(binder #f
    (tuple 
      (expression atomic-a (a racket)) 
      (expression atomic-a (b racket)))))

(check-equal?
  (binder-sexp
    (expressions-binder
      (expressions complex-syntax-a
        (structure dynamic-type-a dynamic-type-b))))
  `(binder
    (entry (identifiers tmp-a tmp-b) #'(complex-a))
    (tuple 
      (expression tmp-a (a racket)) 
      (expression tmp-b (b racket)))))

(define (entry-let-syntax ($entry : Entry)) : Syntax
  (make-syntax
    `(
      ,(reverse (entry-identifier-stack $entry))
      ,(entry-syntax $entry))))
