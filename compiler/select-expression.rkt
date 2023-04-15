#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/select-ingredients)

(define (select-expression ($line-stack : (Stackof Select-Expression-Line))) : Expression
  (select-ingredients-expression
    (fold
      null-select-ingredients
      (reverse $line-stack)
      (lambda (($select-ingredients : Select-Ingredients) ($line : Select-Expression-Line))
        (cond
          ((expression-the? $line)
            (select-ingredients-plus-the
              $select-ingredients
              (expression-the-expression $line)))
          ((type-not? $line)
            (select-ingredients-plus-not
              $select-ingredients
              (type-not-type $line))))))))

(check-equal?
  (expression-sexp
    (select-expression
      (stack
        (expression-the (number-expression 123))
        (type-not text-type))))
  (expression-sexp
    (expression
      #`(cons #t 123)
      (choice! number-type text-type))))

(define-syntax (select-expression! $syntax)
  (syntax-case $syntax ()
    ((_ $line-syntax ...)
      #`(select-expression
        (stack
          #,@(map
            (lambda ($line-syntax)
              (syntax-case $line-syntax (the not)
                ((the $expression-syntax)
                  #`(expression-the $expression-syntax))
                ((not $type-syntax)
                  #`(type-not $type-syntax))))
            (syntax-e #`($line-syntax ...))))))))

(check-equal?
  (expression-sexp
    (select-expression!
      (the (number-expression 123))
      (not text-type)))
  (expression-sexp
    (select-expression
      (stack
        (expression-the (number-expression 123))
        (type-not text-type)))))
