#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/typed
  leo/typed/testing)

(define (typed-decompile ($typed : (Typed Any))) : Any
  (define $type (typed-type $typed))
  (define $value (typed-value $typed))
  (cond
    ((native-type? $type) 
      $value)
    ((symbol-type? $type)
      (symbol-type-symbol $type))
    ((arrow-type? $type) 
      (list `function)) ; TODO
    ((field-type? $type)
      (cons
        (field-type-symbol $type)
        (let (($type-body (field-type-body $type)))
          (cond
            ((struct-type-body? $type-body) `(TODO))
            ((choice-type-body? $type-body) `(TODO))))))
    ((type-type? $type) `(TODO))))
