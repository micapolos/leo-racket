#lang leo/typed

(require 
  leo/compiler/binding
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions-sexp
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-match)

(define (scope-resolve-first-fn ($scope : Scope) ($fn : (-> Binding (Option Expressions)))) : (Option Expressions)
  (and
    (not (null? $scope))
    (or
      ($fn (car $scope))
      (scope-resolve-first-fn (pop $scope) $fn))))

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

(define (arrow-binding-resolve-tuple
  ($lhs-binding : Binding)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (define $binding-syntax-option (binding-identifier-option $lhs-binding))
  (define $binding-type (binding-type $lhs-binding))
  (define $structure (tuple-structure $rhs-tuple))
  (define $dynamic-syntax-stack (tuple-syntax-stack $rhs-tuple))
  (and 
    (arrow? $binding-type)
    (let ()
      (define $arrow $binding-type)
      (define $arrow-from-structure (arrow-from-structure $arrow))
      (define $arrow-to-structure (arrow-to-structure $arrow))
      (and 
        (structure-matches? $structure $arrow-from-structure)
        (expressions
          (and $binding-syntax-option
            (make-syntax
              `(
                ,$binding-syntax-option
                ,@(reverse $dynamic-syntax-stack))))
          $arrow-to-structure)))))

(check-equal?
  (option-app expressions-sexp-structure
    (arrow-binding-resolve-tuple
      (binding identifier-d
        (arrow 
          (stack type-a type-b) 
          (stack type-c type-d)))
      (stack expression-a expression-b)))
  (pair `(d a b) (structure type-c type-d)))

(check-equal?
  (arrow-binding-resolve-tuple
    (binding identifier-d (arrow (stack type-a type-b) (stack type-c)))
    (stack expression-b expression-a))
  #f)

; ------------------------------------------------------------------------

(define (binding-resolve-tuple
  ($lhs-binding : Binding)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (define $single-rhs-expression (single $rhs-tuple))
  (or
    (and
      $single-rhs-expression
      (option-app expression-expressions
        (expression-resolve-get
          (binding-expression $lhs-binding)
          $single-rhs-expression)))
    (arrow-binding-resolve-tuple
      $lhs-binding
      $rhs-tuple)))

; -----------------------------------------------------------------------

(define (scope-resolve-tuple
  ($lhs-scope : Scope)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (and 
    (not (null? $lhs-scope))
    (or
      (binding-resolve-tuple
        (top $lhs-scope)
        $rhs-tuple)
      (scope-resolve-tuple
        (pop $lhs-scope)
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
          (usage-ingredients-resolve-fn 'direct (ingredients $lhs-expressions)
            (lambda (($tuple : Tuple))
              (expression-expressions
                (field-expression $make-symbol $tuple)))))))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-make 
      (expressions syntax-a structure-ab)
      (field-expression `make 
        (tuple (field-expression `foo null)))))
  `(expressions
    (let-values (((tmp-a tmp-b) a)) (cons tmp-a tmp-b))
    (structure (foo (a racket) (b racket)))))
