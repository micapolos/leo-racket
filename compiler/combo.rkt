#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/type-sexp
  leo/compiler/type-utils
  leo/compiler/any-sexp
  leo/compiler/constant)

(data context)

(data value
  (syntax : Syntax)
  (constant-option : (Option Constant)))

(data expression
  (value-option : (Option Value))
  (type : Type))

(data expressions
  (value-option : (Option Value))
  (structure : (Stackof Type)))

(data compiler
  (context : Context)
  (expression-stack : (Stackof Expression)))

; ----------------------------------------------------------------------------

(define (value-stack-value-option ($value-stack : (Stackof Value))) : (Option Value)
  (case (length $value-stack)
    ((0) #f)
    ((1)
      (top $value-stack))
    ((2)
      (define $lhs-value (pop-top $value-stack))
      (define $rhs-value (top $value-stack))
      (value
        (make-syntax
          `(cons
            ,(value-syntax $lhs-value)
            ,(value-syntax $rhs-value)))
                  (option-app constant
        (option-app cons
          (option-app constant-any (value-constant-option $lhs-value))
          (option-app constant-any (value-constant-option $rhs-value))))))
    (else
      (value
        (make-syntax
          `(vector
            ,@(reverse (map value-syntax $value-stack))))
        (option-bind (lift-option-list (map value-constant-option $value-stack)) $constant-stack
          (constant (apply vector (reverse (map constant-any $constant-stack)))))))))

; ----------------------------------------------------------------------------

(define (expression-stack-expressions ($expression-stack : (Stackof Expression))) : Expressions
  (expressions
    (value-stack-value-option (filter-false (map expression-value-option $expression-stack)))
    (map expression-type $expression-stack)))

; ----------------------------------------------------------------------------

(define (value-sexp ($value : Value)) : Sexp
  `(value
    ,(syntax-sexp (value-syntax $value))
    ,(option-app constant-sexp (value-constant-option $value))))

(define (expression-sexp ($expression : Expression)) : Sexp
  `(expression
    ,(option-app value-sexp (expression-value-option $expression))
    ,(type-sexp (expression-type $expression))))

; ----------------------------------------------------------------------------

(define (compiler-plus ($compiler : Compiler) ($syntax : Syntax)) : Compiler
  (define $context (compiler-context $compiler))
  (compiler $context
    (push
      (compiler-expression-stack $compiler)
      (compile-expression $context $syntax))))

(define (compile-expression-stack ($context : Context) ($syntax-list : (Listof Syntax))) : (Stackof Expression)
  (compiler-expression-stack
    (fold
      (compiler $context null)
      $syntax-list
      compiler-plus)))

(define (compile-expression ($context : Context) ($syntax : Syntax)) : Expression
  (define $e (syntax-e $syntax))
  (cond
    ((null? $e) (error "compile error"))
    ((symbol? $e) (expression #f (field! $e)))
    ((number? $e) (expression (value $syntax (constant $e)) number-type))
    ((string? $e) (expression (value $syntax (constant $e)) text-type))
    ((list? $e)
      (define $car (car $e))
      (define $car-e (syntax-e $car))
      (define $cdr (cdr $e))
      (cond
        ((symbol? $car-e)
          (define $expressions (compile-expressions $context $cdr))
          (expression
            (expressions-value-option $expressions)
            (field $car-e (expressions-structure $expressions))))
        (else (error "compile error"))))
    (else (error "compile error"))))

(define (compile-expressions ($context : Context) ($syntax-list : (Listof Syntax))) : Expressions
  (expression-stack-expressions
    (compile-expression-stack $context $syntax-list)))

(check-equal?
  (expression-sexp (compile-expression (context) #`foo))
  (expression-sexp (expression #f (field! `foo))))

(check-equal?
  (expression-sexp (compile-expression (context) #`"foo"))
  (expression-sexp (expression (value #`"foo" (constant "foo")) text-type)))

(check-equal?
  (expression-sexp (compile-expression (context) #`123))
  (expression-sexp (expression (value #`123 (constant 123)) number-type)))

(check-equal?
  (expression-sexp (compile-expression (context) #`(foo 123 gar "bar")))
  (expression-sexp
    (expression
      (value
        #`(cons 123 "bar")
        (constant (cons 123 "bar")))
      (field! `foo number-type (field! `gar) text-type))))

(check-equal?
  (expression-sexp (compile-expression (context) #`(foo 123 gar "bar" far "zar")))
  (expression-sexp
    (expression
      (value
        #`(vector 123 "bar" "zar")
        (constant (vector 123 "bar" "zar")))
      (field! `foo number-type (field! `gar) text-type (field! `far) text-type))))
