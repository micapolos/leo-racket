#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/stack
  leo/typed/option
  leo/typed/base
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/typed/srcloc
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/type-match)

; ----------------------------------------------------------------------------------------

(define (tuple-resolve-selector
  ($tuple : Tuple)
  ($selector : Expression))
  : (Option Expression)
  (and 
    (not (null? $tuple))
    (or
      (expression-resolve-selector (top $tuple) $selector)
      (tuple-resolve-selector (pop $tuple) $selector))))

(define (expression-resolve-selector
  ($expression : Expression)
  ($selector : Expression))
  : (Option Expression)
  (and
    (type-matches-selector? 
      (expression-type $expression) 
      (expression-type $selector))
    $expression))

; -----------------------------------------------------------------------------------------

(define (expression-resolve-symbol
  ($expression : Expression)
  ($symbol : Symbol))
  : (Option Expression)
  (define $expression-type (expression-type $expression))
  (and
    (type-matches-symbol? $expression-type $symbol)
    (expression (expression-syntax $expression) $expression-type)))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-symbol
      (expression syntax-b (field `a (stack type-b))) `a))
  (pair `(dynamic-b) (field `a (stack type-b))))

(check-equal?
  (expression-resolve-symbol
    (expression syntax-b (field `a (stack type-b))) `b)
  #f)

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
  (pair `(dynamic-a) type-a))

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

(define (expression-resolve-symbol-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (define $type (expression-type $rhs-expression))
  (and
    (field? $type)
    (null? (field-structure $type))
    (expression-resolve-symbol
      $lhs-expression
      (field-symbol $type))))

(check-equal?
  (option-app expression-sexp-type
    (expression-resolve-symbol-expression
      (expression syntax-b (field `a (stack type-b))) 
      (expression syntax-a (field `a null))))
  (pair `(dynamic-b) (field `a (stack type-b))))

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a type-a))
  #f)

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `not-a null)))
  #f)

(check-equal?
  (expression-resolve-symbol-expression
    (expression syntax-b (field `a (stack type-b))) 
    (expression syntax-a (field `a (stack type-a))))
  #f)

; -----------------------------------------------------------------------

(define (expression-resolve-expression
  ($lhs-expression : Expression)
  ($rhs-expression : Expression))
  : (Option Expression)
  (or
    (expression-resolve-symbol-expression $lhs-expression $rhs-expression)))

; -----------------------------------------------------------------------

(define (expression-rhs-get ($expression : Expression) ($selector : Expression)) : (Option Expression)
  (option-app tuple-resolve-selector 
    (expression-rhs-tuple-option $expression) 
    $selector))

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
  (pair `(dynamic-b) (field `a (stack type-b))))

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
  (pair `(#%app (dynamic-d) (dynamic-a) (dynamic-b)) (structure type-c type-d)))

(check-equal?
  (arrow-expression-resolve-tuple
    (expression syntax-d (arrow (stack type-a type-b) (stack type-c)))
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (choice-expression-resolve-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple)) : (Option Expressions)
  (let (($expression-type (expression-type $lhs-expression)))
    (and (choice? $expression-type)
      (let* (($choice-type-stack (choice-type-stack $expression-type))
             ($choice-structure-stack (map structure $choice-type-stack))
             ($case-type-stack (tuple-structure $rhs-tuple))
             ($apply-structure-option-stack (map type-apply-structure $case-type-stack $choice-structure-stack)))
        (and
          (andmap (ann identity (-> (Option Structure) (Option Structure))) $apply-structure-option-stack)
          #f)))))

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
      $rhs-tuple)
    (choice-expression-resolve-tuple 
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
  (or 
    (option-app expressions-resolve-expression
      (option-app expression-expressions (single $lhs-tuple))
      $rhs-expression)))

; -----------------------------------------------------------------------

(define (tuple-resolve ($tuple : Tuple)) : (Option Expressions)
  (and
    (>= (length $tuple) 2))
    (tuple-expression-resolve (cdr $tuple) (car $tuple)))

; -----------------------------------------------------------------------

(define (tuple-do ($tuple : Tuple) ($fn : (-> Tuple Expressions))) : Expressions
  (define $binder-stack (tuple-binder-stack $tuple))
  (define $let-syntax-list (reverse (filter-false (map binder-syntax-option $binder-stack))))
  (define $arg-tuple (map binder-expression $binder-stack))
  (define $fn-expressions (#%app $fn $arg-tuple))
  (define $fn-syntax (expressions-syntax $fn-expressions))
  (define $fn-structure (expressions-structure $fn-expressions))
  (make-expressions
    (make-syntax 
      (case (length $let-syntax-list)
        ((0) $fn-syntax)
        (else `(let ,$let-syntax-list ,$fn-syntax))))
    $fn-structure))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple static-expression-a)
      (lambda (($tuple : Tuple))
        (expression-expressions (field-expression `done $tuple)))))
  `(expressions #f (structure (done a))))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple dynamic-expression-a static-expression-b)
      (lambda (($tuple : Tuple))
        (expression-expressions (field-expression `done $tuple)))))
  `(expressions
    (let ((tmp-a (dynamic-a))) tmp-a)
    (structure (done (a racket) b))))

(check-equal?
  (expressions-sexp
    (tuple-do
      (tuple dynamic-expression-a static-expression-b dynamic-expression-c)
      (lambda (($tuple : Tuple))
        (expression-expressions (field-expression `done $tuple)))))
  `(expressions
    (let ((tmp-a (dynamic-a)) (tmp-c (dynamic-c))) (cons tmp-a tmp-c))
    (structure (done (a racket) b (c racket)))))

(define (expressions-resolve-expression
  ($expressions : Expressions)
  ($expression : Expression))
  : (Option Expressions)
  (expressions-rhs-resolve-expression $expressions $expression))

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
    expressions-sexp)
  `(expressions 
    (unsafe-vector-ref (dynamic-a) 0) 
    (structure (b racket))))

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
