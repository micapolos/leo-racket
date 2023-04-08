#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/compiler
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/expressions-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/compile-recursively
  leo/compiler/syntax-type
  leo/compiler/sexp-expression
  leo/compiler/syntax-utils
  leo/compiler/module-resolver)

(define (compiler-debug ($compiler : Compiler)) : Compiler 
  (compiler-with-ingredients $compiler
    (ingredients
      (expression-expressions
        (sexp-expression (compiler-sexp $compiler))))))

(define (compiler-with-ingredients ($compiler : Compiler) ($ingredients : Ingredients)) : Compiler
  (struct-copy compiler $compiler (ingredients $ingredients)))

(define (compiler-apply-debug ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler 
  (compiler-debug 
    (compiler-with-ingredients $compiler
      (ingredients-plus
        (compiler-ingredients $compiler)
        (compile-ingredients-recursively
          (compiler-scope $compiler)
          $syntax-list)))))

(define (compiler-apply-racket ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler 
  (cond
    ((null? $syntax-list)
      (compiler-with-ingredients $compiler
        (ingredients-apply-racket
          (compiler-ingredients $compiler))))
    (else
      (compiler-with-ingredients $compiler
        (ingredients-plus
          (compiler-ingredients $compiler)
          (ingredients
            (expression-expressions
              (syntax-list-racket-expression $syntax-list))))))))

(define (compiler-apply-type ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler 
  (compiler-with-ingredients $compiler
    (ingredients-apply-type
      (ingredients-plus 
        (compiler-ingredients $compiler)
        (compile-ingredients-recursively
          (compiler-scope $compiler)
          $syntax-list)))))

(define (compiler-apply-package ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (define $expressions
    (option-or
      (syntax-resolve-module (make-syntax `(package ,@$syntax-list)))
      (error "can not resolve package")))
  (compiler-plus-expressions $compiler $expressions))

(define (compiler-apply-quote ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-plus-quoted-tuple $compiler
    (sexp-list-tuple
      (map syntax->datum $syntax-list))))

(define (compiler-apply-apply ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (scope-apply-ingredients
      (compiler-scope $compiler)
      (ingredients-plus 
        (compiler-ingredients $compiler)
        (compile-ingredients-recursively
          (compiler-scope $compiler)
          $syntax-list)))))

(define (compiler-apply-fn
  ($compiler : Compiler) 
  ($fn : (-> Tuple Expressions))) : Compiler
  (define $ingredients (compiler-ingredients $compiler))
  (compiler-with-ingredients $compiler
    (ingredients
      (cond
        ((null? (ingredients-structure $ingredients))
          ($fn (scope-tuple (compiler-scope $compiler))))
        (else
          ($fn 
            (option-or
              (expressions-rhs-option
                (ingredients-expressions $ingredients))
              (error "top: no rhs"))))))))

(define (compiler-thing ($compiler : Compiler) ($index : Exact-Positive-Integer)) : Compiler
  (compiler-apply-fn $compiler
    (lambda (($tuple : Tuple))
      (expression-expressions 
        (tuple-ref $tuple (sub1 $index))))))

(define (compiler-everything ($compiler : Compiler)) : Compiler
  (compiler-apply-fn $compiler
    (lambda (($tuple : Tuple))
      (tuple-expressions $tuple))))

(define (syntax-list-racket-expression ($syntax-list : (Listof Syntax))) : Expression
  (unless (= (length $syntax-list) 2)
    (error "native syntax error"))
  (define $syntax-1 (car $syntax-list))
  (define $syntax-2 (cadr $syntax-list))
  (or
    (syntax-symbol-match-args $syntax-2 `of $args
      (unless (= (length $args) 1)
        (error "native of single line error"))
      (define $car-e (syntax-e $syntax-1))
      (define $car-syntax
        (cond
          ((symbol? $car-e) $syntax-1)
          ((string? $car-e) (any-syntax (read-syntax #f (open-input-string $car-e))))
          (else (error (format "racket syntax error: ~s" $car-e)))))
      (expression
        $car-syntax
        (syntax-type (car $args))))
    (error "native of expected")))
