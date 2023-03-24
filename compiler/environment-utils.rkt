#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/environment
  leo/typed/option
  leo/typed/base
  leo/typed/stack
  leo/typed/maybe
  leo/typed/testing
  leo/compiler/binder
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/ingredients
  leo/compiler/expressions
  leo/compiler/ingredients-utils
  leo/compiler/expressions-utils
  leo/compiler/module-syntax
  leo/compiler/type
  leo/compiler/type-utils)

(define
  (environment-plus-ingredients
    ($environment : Environment)
    ($ingredients : Ingredients))
  : Environment
  (fold
    $environment
    (reverse
      (filter-false
        (map binder-entry-option
          (usage-ingredients-binder-stack 'indirect $ingredients))))
    environment-plus-entry))

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
    (environment-plus-ingredients
      base-environment
      (ingredients
        (expressions #`#f (structure (field! `bool boolean-type)))
        (expressions #`128 (structure (field! `num number-type)))
        (expressions
          #`(values (+ 1 2) (string-append "foo" "bar"))
          (structure number-type text-type))))
    (map
      (curry environment-ref $enviroment)
      (map type-generate-temporary-symbol
        (stack
          (field! `bool boolean-type)
          (field! `num number-type)
          number-type
          text-type
          (field! `absent)))))
  (stack
    (just #f)
    (just 128)
    (just 3)
    (just "foobar")
    #f))
