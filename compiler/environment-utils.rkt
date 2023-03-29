#lang leo/typed

(require
  leo/compiler/binder
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/ingredients
  leo/compiler/expressions
  leo/compiler/ingredients-utils
  leo/compiler/expressions-utils
  leo/compiler/module-syntax
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define
  (environment-plus-entry
    ($environment : Environment)
    ($entry : Entry))
  : Environment
  (environment-eval-define
    $environment
    (syntax->datum
      (binder-entry-define-syntax $entry))))

(check-equal?
  (bind $enviroment
    (environment-plus-entry
      base-environment
      (entry
        (stack #`a #`b #`c)
        (make-syntax `(values #f (+ 1 2) (string-append "foo" "bar")))))
    (map
      (curry environment-ref $enviroment)
      (stack `a `b `c `foo)))
  (stack
    (just #f)
    (just 3)
    (just "foobar")
    #f))
