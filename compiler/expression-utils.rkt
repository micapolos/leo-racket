#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/literal
  leo/compiler/expressions
  leo/compiler/expression
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/type-sexp
  leo/compiler/type-utils
  leo/compiler/type-syntax)

(define dynamic-expression-a (expression syntax-a dynamic-type-a))
(define dynamic-expression-b (expression syntax-b dynamic-type-b))
(define dynamic-expression-c (expression syntax-c dynamic-type-c))
(define dynamic-expression-d (expression syntax-d dynamic-type-d))

(define static-expression-a (expression #f static-type-a))
(define static-expression-b (expression #f static-type-b))
(define static-expression-c (expression #f static-type-c))
(define static-expression-d (expression #f static-type-d))

(define expression-a dynamic-expression-a)
(define expression-b dynamic-expression-b)
(define expression-c dynamic-expression-c)
(define expression-d dynamic-expression-d)

(define static-tuple-a (tuple static-expression-a))
(define static-tuple-b (tuple static-expression-b))
(define static-tuple-c (tuple static-expression-c))
(define static-tuple-d (tuple static-expression-d))

(define dynamic-tuple-a (tuple dynamic-expression-a))
(define dynamic-tuple-b (tuple dynamic-expression-b))
(define dynamic-tuple-c (tuple dynamic-expression-c))
(define dynamic-tuple-d (tuple dynamic-expression-d))

(define tuple-a dynamic-tuple-a)
(define tuple-b dynamic-tuple-b)
(define tuple-c dynamic-tuple-c)
(define tuple-d dynamic-tuple-d)

(define tuple-ab (tuple expression-a expression-b))

(define (expression-sexp ($expression : Expression)) : Sexp
  `(expression
    ,(option-app syntax->datum (expression-syntax-option $expression))
    ,(type-sexp (expression-type $expression))))

(check-equal?
  (expression-sexp (expression syntax-a static-type-b))
  `(expression a b))

(define (tuple-sexp ($tuple : Tuple)) : Sexp
  `(tuple ,@(reverse (map expression-sexp $tuple))))

(check-equal?
  (tuple-sexp (tuple expression-a expression-b))
  `(tuple (expression a (a racket)) (expression b (b racket))))

(define (racket-expression ($sexp : Sexp)) 
  (expression (make-syntax (sexp-datum $sexp)) (racket)))

(define (boolean-expression ($boolean : Boolean)) 
  (expression (make-syntax $boolean) boolean-type))

(define (number-expression ($number : Number)) 
  (expression (make-syntax $number) number-type))

(define (int-expression ($fixnum : Fixnum)) 
  (expression (make-syntax $fixnum) int-type))

(define (float-expression ($flonum : Flonum)) 
  (expression (make-syntax $flonum) float-type))

(define (text-expression ($string : String)) 
  (expression (make-syntax $string) text-type))

(define (word-expression ($symbol : Symbol))
  (expression (make-syntax `(quote ,$symbol)) word-type))

(define (literal-expression ($literal : Literal))
  (cond
    ((string? $literal) (text-expression $literal))
    ((number? $literal) (number-expression $literal))))

(define (type-expression ($type : Type)) : Expression
  (expression (type-syntax $type) (type-universe $type)))

(define (reified-expression ($structure : Structure))
  (expression #f (reified $structure)))

(define (expression-dynamic? ($expression : Expression)) : Boolean
  (type-dynamic? (expression-type $expression)))

(define (expression-sexp-type ($expression : Expression)) : (Pairof Sexp Type)
  (pair
    (option-app syntax->datum (expression-syntax-option $expression))
    (expression-type $expression)))

(define (tuple-structure 
  ($tuple : Tuple))
  : Structure
  (map expression-type $tuple))

(define (tuple-syntax-stack
  ($tuple : Tuple))
  : (Stackof Syntax)
  (filter-false (map expression-syntax-option $tuple)))

(check-equal?
  (tuple-structure (stack expression-a expression-b))
  (stack type-a type-b))

(check-equal?
  (tuple-syntax-stack (stack expression-a expression-b))
  (stack syntax-a syntax-b))

; ---------------------------------------------------------

(define (expression-symbol-content ($expression : Expression) ($symbol : Symbol)) : (Option Expressions)
  (define $type (expression-type $expression))
  (and 
    (field? $type)
    (equal? (field-symbol $type) $symbol)
    (expressions
      (expression-syntax-option $expression)
      (field-structure $type))))

(check-equal?
  (expression-symbol-content (expression syntax-a (field `foo structure-ab)) `foo)
  (expressions syntax-a structure-ab))

(check-equal?
  (expression-symbol-content (expression syntax-a (field `foo structure-ab)) `bar)
  #f)

(check-equal?
  (expression-symbol-content (expression syntax-a (racket)) `bar)
  #f)

; ----------------------------------------------------------------------------

(define (expression-symbol-option ($expression : Expression)) : (Option Symbol)
  (define $type (expression-type $expression))
  (and 
    (field? $type)
    (null? (field-structure $type))
    (field-symbol $type)))

; ----------------------------------------------------------------------------

(define (syntax-option-structure-ref
  ($syntax-option : (Option Syntax))
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer))
  : Expression
  (define $structure-dynamic-size (structure-dynamic-size $structure))
  (define $dynamic-index (structure-dynamic-ref $structure $index))
  (define $type (list-ref $structure $index))
  (expression
    (and $syntax-option
      (make-syntax
        (and
          $dynamic-index
          (case $structure-dynamic-size
            ((0) #f)
            ((1) $syntax-option)
            ((2)
              `(
                ,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr)
                ,$syntax-option))
            (else
              `(unsafe-vector-ref
                ,$syntax-option
                ,(- $structure-dynamic-size $dynamic-index 1)))))))
    $type))

(define (tuple-push-syntax-stack-structure
  ($tuple : Tuple)
  ($syntax-stack : (Stackof Syntax))
  ($structure : Structure))
  : Tuple
  (cond
    ((null? $structure) $tuple)
    (else
      (define $top-type (top $structure))
      (define $pop-structure (pop $structure))
      (cond
        ((type-dynamic? $top-type)
          (tuple-push-syntax-stack-structure
            (push $tuple (expression (top $syntax-stack) $top-type))
            (pop $syntax-stack)
            $pop-structure))
        (else
          (tuple-push-syntax-stack-structure
            (push $tuple (expression #f $top-type))
            $syntax-stack
            $pop-structure))))))

(define (syntax-stack-structure-tuple ($syntax-stack : (Stackof Syntax)) ($structure : Structure)) : Tuple
  (reverse (tuple-push-syntax-stack-structure null-tuple $syntax-stack $structure)))

(check-equal?
  (tuple-sexp
    (syntax-stack-structure-tuple
      (stack syntax-a syntax-c)
      (structure dynamic-type-a static-type-b dynamic-type-c)))
  (tuple-sexp
    (tuple
      (expression syntax-a dynamic-type-a)
      (expression #f static-type-b)
      (expression syntax-c dynamic-type-c))))

; ------------------------------------------------------------

(define (syntax-option-structure-tuple ($syntax-option : (Option Syntax)) ($structure : Structure)) : Tuple
  (map 
    (curry (curry syntax-option-structure-ref $syntax-option) $structure)
    (range (length $structure))))

(check-equal?
  (map
    expression-sexp-type
    (syntax-option-structure-tuple
      syntax-a
      (structure dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)))
  (stack
    (pair `(unsafe-vector-ref a 0) dynamic-type-a)
    (pair `(unsafe-vector-ref a 1) dynamic-type-b)
    (pair `#f static-type-c)
    (pair `(unsafe-vector-ref a 2) dynamic-type-d)))

; ------------------------------------------------------------

(define (expression-rhs-tuple-option ($expression : Expression)) : (Option Tuple)
  (define $type (expression-type $expression))
  (and (field? $type)
    (syntax-option-structure-tuple
      (expression-syntax-option $expression)
      (field-structure $type))))

(check-equal?
  (option-app map expression-sexp-type
    (expression-rhs-tuple-option
      (expression syntax-a 
        (field `foo
          (structure type-b type-c)))))
  (stack
    (cons `(unsafe-car a) type-b)
    (cons `(unsafe-cdr a) type-c)))

(check-equal?
  (expression-rhs-tuple-option (expression syntax-a (racket)))
  #f)

; ---------------------------------------------------------

(define (expression-apply-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Expressions)
  (define $syntax-option (expression-syntax-option $lhs-expression))
  (define $type-option
    (type-apply-structure
      (expression-type $lhs-expression)
      (tuple-structure $rhs-tuple)))
  (and $type-option
    (expressions
      (and $syntax-option
        (make-syntax
          `(,$syntax-option
            ,@(reverse
              (tuple-syntax-stack $rhs-tuple)))))
      $type-option)))

(check-equal?
  (option-app expressions-sexp-structure
    (expression-apply-tuple
      (expression #`fn
        (arrow 
          (structure 
            dynamic-type-a 
            static-type-b 
            dynamic-type-c)
          (structure 
            dynamic-type-c 
            static-type-d)))
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c)))
  (expressions-sexp-structure
    (expressions
      #`(fn a c)
      (structure
        dynamic-type-c
        static-type-d))))

(check-equal?
  (option-app expressions-sexp-structure
    (expression-apply-tuple
      (expression #f (arrow dynamic-structure-a static-structure-b))
      dynamic-tuple-a))
  (expressions-sexp-structure
    (expressions #f static-structure-b)))

(check-equal?
  (expression-apply-tuple
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (stack expression-c))
  #f)

; ------------------------------------------------------------------------------

(define (tuple-syntax-option
  ($tuple : Tuple))
  : (Option Syntax)
  (define $syntax-stack
    (filter-false (map expression-syntax-option $tuple)))
  (define $length
    (length $syntax-stack))
  (case $length
    ((0) #f)
    ((1) (top $syntax-stack))
    ((2) 
      (make-syntax
        `(cons 
          ,(pop-top $syntax-stack)
          ,(top $syntax-stack))))
    (else 
      (make-syntax
        `(vector 
          ,@(reverse $syntax-stack))))))

(check-equal?
  (option-app syntax->datum (tuple-syntax-option null))
  #f)

(check-equal?
  (option-app syntax->datum (tuple-syntax-option (stack static-expression-a)))
  #f)

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack dynamic-expression-a)))
  `a)

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack 
        dynamic-expression-a 
        static-expression-a)))
  `a)

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack 
        dynamic-expression-a 
        dynamic-expression-b)))
  `(cons a b))

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        static-expression-c)))
  `(cons a b))

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c)))
  `(vector a b c))

(check-equal?
  (option-app syntax->datum
    (tuple-syntax-option
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c 
        static-expression-d)))
  `(vector a b c))

; -----------------------------------------------------------------

(define (tuple-values-syntax-option ($tuple : Tuple)) : (Option Syntax)
  (define $syntax-stack (filter-false (map expression-syntax-option $tuple)))
  (case (length $syntax-stack)
    ((0) #f)
    ((1) (top $syntax-stack))
    (else (make-syntax `(values ,@(reverse $syntax-stack))))))

(define (tuple-values-syntax ($tuple : Tuple)) : Syntax
  (or
    (tuple-values-syntax-option $tuple)
    (make-syntax `(values))))

(check-equal?
  (option-map
    (tuple-values-syntax-option null)
    syntax->datum)
  #f)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack dynamic-expression-a))
    syntax->datum)
  `a)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c))
    syntax->datum)
  `(values a c))

; ---------------------------------------------------------

(define (field-expression ($symbol : Symbol) ($tuple : Tuple null-tuple)) : Expression
  (expression
    (tuple-syntax-option $tuple)
    (field $symbol (tuple-structure $tuple))))

(check-equal?
  (expression-sexp-type (field-expression `foo tuple-ab))
  (pair `(cons a b) (field `foo structure-ab)))

(define-syntax (field-expression! $syntax)
  (syntax-case $syntax ()
    ((_ name expression ...)
      #`(field-expression (quote name) (tuple expression ...)))))

(check-equal?
  (expression-sexp (field-expression! foo (field-expression! bar)))
  `(expression #f (foo bar)))

; ---------------------------------------------------------

(define (expression-lift-type ($expression : Expression)) : (Option Type)
  (type-lift (expression-type $expression)))

(define (tuple-lift-structure ($tuple : Tuple)) : (Option Structure)
  (structure-lift (tuple-structure $tuple)))

; ---------------------------------------------------------

(define (binding-expression ($binding : Binding)) : Expression
  (expression
    (binding-identifier-option $binding)
    (binding-type $binding)))

(define (scope-tuple ($scope : Scope)) : Tuple
  (map binding-expression $scope))
