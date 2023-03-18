#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
  leo/compiler/binder
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions-sexp
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/typed/srcloc
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/type-match)

(define (tuple-resolve-first-fn ($tuple : Tuple) ($fn : (-> Expression (Option Expressions)))) : (Option Expressions)
  (and
    (not (null? $tuple))
    (or
      ($fn (car $tuple))
      (tuple-resolve-first-fn (pop $tuple) $fn))))

; ----------------------------------------------------------------------------------------

(define (expression-resolve-selector
  ($expression : Expression)
  ($selector : Expression))
  : (Option Expression)
  (and
    (type-matches-selector? 
      (expression-type $expression) 
      (expression-type $selector))
    $expression))

; -----------------------------------------------------------------------

(define (expression-resolve-type
  ($expression : Expression)
  ($type : Type))
  : (Option Expression)
  (and
    (type-matches? $type (expression-type $expression))
    (expression (expression-syntax $expression) $type)))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-type (expression syntax-a type-a) type-a))
  (pair `a type-a))

(check-equal?
  (expression-resolve-type (expression syntax-a type-a) type-b)
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-get-a-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and
    (field? $type)
    (equal? (field-symbol $type) `get)
    (option-bind (single (field-structure $type)) $rhs-type
      (expression-resolve-get-a-expression
        $lhs-expression
        (expression (expression-syntax $rhs-expression) $rhs-type)))))

; -----------------------------------------------------------------------

(define (expression-resolve-get
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (or
    (option-bind (expression-symbol-content $rhs-expression `get) $selectors-expressions
      (option-bind (expressions-expression-option $selectors-expressions) $selector-expression
        (expression-resolve-selector $lhs-expression $selector-expression)))
    (expression-resolve-selector $lhs-expression $rhs-expression)))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-get
      (expression syntax-b (field `a (stack type-b))) 
      (expression syntax-a (field `get (structure (field! `a))))))
  (pair `b (field `a (stack type-b))))

(check-equal?
  (expression-resolve-get
    (expression syntax-b (field `get (structure (field! `b)) ))
    (expression syntax-a type-a))
  #f)

(check-equal?
  (expression-resolve-get
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `not-get (structure (field! `a)))))
  #f)

; -----------------------------------------------------------------------

(define (arrow-expression-resolve-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (define $expression-type (expression-type $lhs-expression))
  (define $structure (tuple-structure $rhs-tuple))
  (define $dynamic-syntax-stack (tuple-syntax-stack $rhs-tuple))
  (and 
    (arrow? $expression-type)
    (let ()
      (define $arrow $expression-type)
      (define $arrow-from-structure (arrow-from-structure $arrow))
      (define $arrow-to-structure (arrow-to-structure $arrow))
      (and 
        (structure-matches? $structure $arrow-from-structure)
        (expressions
          (make-syntax 
            `(#%app
              ,(expression-syntax $lhs-expression)
              ,@(reverse $dynamic-syntax-stack)))
          $arrow-to-structure)))))

(check-equal?
  (option-app expressions-sexp-structure
    (arrow-expression-resolve-tuple
      (expression syntax-d 
        (arrow 
          (stack type-a type-b) 
          (stack type-c type-d)))
      (stack expression-a expression-b)))
  (pair `(#%app d a b) (structure type-c type-d)))

(check-equal?
  (arrow-expression-resolve-tuple
    (expression syntax-d (arrow (stack type-a type-b) (stack type-c)))
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (expression-resolve-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (define $single-rhs-expression (single $rhs-tuple))
  (or
    (and
      $single-rhs-expression
      (option-app expression-expressions
        (expression-resolve-get $lhs-expression $single-rhs-expression)))
    (arrow-expression-resolve-tuple 
      $lhs-expression 
      $rhs-tuple)))

; -----------------------------------------------------------------------

(define (tuple-resolve-tuple
  ($lhs-tuple : Tuple)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (and 
    (not (null? $lhs-tuple))
    (or
      (expression-resolve-tuple
        (top $lhs-tuple)
        $rhs-tuple)
      (tuple-resolve-tuple 
        (pop $lhs-tuple) 
        $rhs-tuple))))

; -----------------------------------------------------------------------

(define (tuple-expression-resolve
  ($lhs-tuple : Tuple)
  ($rhs-expression : Expression))
  : (Option Expressions)
  (expressions-resolve-expression
    (tuple-expressions $lhs-tuple)
    $rhs-expression))

; -----------------------------------------------------------------------

(define (tuple-resolve ($tuple : Tuple)) : (Option Expressions)
  (and
    (>= (length $tuple) 2))
    (tuple-expression-resolve (cdr $tuple) (car $tuple)))

; -----------------------------------------------------------------------

(define (expression-resolve-fn
  ($expression : Expression) 
  ($fn : (-> Binding (Option Expressions))))
  : (Option Expressions)
  (define $type (expression-type $expression))
  (define $syntax (expression-syntax $expression))
  (define $binding (type-generate-binding $type))
  (define $tmp-option (binding-identifier-option $binding))
  (define $fn-expressions-option ($fn $binding))
  (option-bind $fn-expressions-option $fn-expressions
    (define $fn-syntax (expressions-syntax $fn-expressions))
    (define $fn-structure (expressions-structure $fn-expressions))
    (expressions
      (or
        (and $tmp-option (make-syntax `(let ((,$tmp-option ,$syntax)) ,$fn-syntax)))
        $fn-syntax)
      $fn-structure)))

(define (tuple-resolve-fn 
  ($tuple : Tuple) 
  ($fn : (-> Tuple (Option Expressions))))
  : (Option Expressions)
  (define $structure (tuple-structure $tuple))
  (define $binder-stack (map expression-binder $tuple))
  (define $binder-entry-stack (filter-false (map binder-entry-option $binder-stack)))
  (define $bound-syntax-stack (map binder-bound-syntax $binder-stack))
  (define $bound-tuple (map expression $bound-syntax-stack $structure))
  (define $fn-expressions-option ($fn $bound-tuple))
  (option-bind $fn-expressions-option $fn-expressions
    (define $fn-syntax (expressions-syntax $fn-expressions))
    (define $fn-structure (expressions-structure $fn-expressions))
    (make-expressions
      (make-syntax 
        (case (length $binder-entry-stack)
          ((0) $fn-syntax)
          (else 
            `(let
              (,@(reverse (map binder-entry-let-syntax $binder-entry-stack)))
              ,$fn-syntax))))
      $fn-structure)))

(define (tuple-do ($tuple : Tuple) ($fn : (-> Tuple Expressions))) : Expressions
  (option-or
    (tuple-resolve-fn $tuple $fn)
    (tuple-expressions $tuple)))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple
        (expression null-syntax static-type-a)
        (expression null-syntax static-type-b))
      tuple-default-apply-fn))
  `(expressions #f (structure (resolved a b))))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple
        (expression null-syntax static-type-a)
        (expression atomic-syntax-b dynamic-type-b))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `resolved $tuple)))))
  `(expressions atomic-b (structure (resolved a (b racket)))))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple 
        (expression null-syntax static-type-a)
        (expression atomic-syntax-b dynamic-type-b)
        (expression complex-syntax-c dynamic-type-c))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `resolved $tuple)))))
  `(expressions
    (let ((tmp-c (complex-c))) (cons atomic-b tmp-c))
    (structure (resolved a (b racket) (c racket)))))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple 
        (expression null-syntax static-type-a)
        (expression atomic-syntax-b dynamic-type-b)
        (expression complex-syntax-c dynamic-type-c)
        (expression complex-syntax-d dynamic-type-d))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `resolved $tuple)))))
  `(expressions
    (let ((tmp-c (complex-c)) 
          (tmp-d (complex-d)))
      (vector atomic-b tmp-c tmp-d))
    (structure 
      (resolved a (b racket) (c racket) (d racket)))))

(define (expressions-resolve-expression
  ($expressions : Expressions)
  ($expression : Expression))
  : (Option Expressions)
  (or
    (expressions-resolve-make $expressions $expression)
    (expressions-rhs-resolve-expression $expressions $expression)))

; ---------------------------------------------------------------------

(define (expressions-rhs-resolve-expression
  ($expressions : Expressions)
  ($expression : Expression))
  : (Option Expressions)
  (option-bind (expressions-rhs-option $expressions) $tuple
    (option-map
      (option-stack-first 
        (map
          (lambda (($lhs-expression : Expression)) 
            (expression-resolve-get $lhs-expression $expression))
          $tuple))
      expression-expressions)))

(check-equal?
  (option-map
    (expressions-rhs-resolve-expression
      (expressions
        syntax-a
        (structure
          (field `point 
            (stack
              (field `b (stack (racket))) 
              (field `c (stack (racket))) 
              (field `d (stack (racket)))))))
        (expression syntax-b (field `get (structure (field! `b)))))
    expressions-sexp-structure)
  (pair 
    `(unsafe-vector-ref a 0)
    (structure (field `b (stack (racket))))))

(check-equal?
  (expressions-rhs-resolve-expression
    (expressions
      syntax-a
      (structure
        (field `point 
          (stack
            (field `b (stack (racket))) 
            (field `c (stack (racket))) 
            (field `d (stack (racket)))))))
    (expression syntax-b (field `get (structure (field! `e)))))
  #f)

; -----------------------------------------------------------------------

(define (expressions-resolve-make
  ($lhs-expressions : Expressions)
  ($rhs-expression : Expression))
  : (Option Expressions)
  (or
    (option-bind (expression-symbol-content $rhs-expression `make) $make-content
      (option-bind (expressions-expression-option $make-content) $make-expression
        (option-bind (expression-symbol-option $make-expression) $make-symbol
          (expressions-resolve-fn $lhs-expressions 
            (lambda (($tuple : Tuple))
              (expression-expressions
                (field-expression $make-symbol $tuple)))))))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-make 
      (expressions complex-syntax-a structure-ab)
      (field-expression `make 
        (tuple (field-expression `foo null)))))
  `(expressions
    (let-values (((tmp-a tmp-b) (complex-a))) (cons tmp-a tmp-b))
    (structure (foo (a racket) (b racket)))))
