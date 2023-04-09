#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/generate-temporary
  leo/compiler/binding)

(data value-scope
  (environment : Environment)
  (scope : Scope))

(data value-binder
  (binding : Binding)
  (any : Any))

(define base-value-scope
  (value-scope
    base-environment
    null-scope))

(define (value-generate-binder ($value : Value)) : Value-Binder
  (define $type (value-type $value))
  (value-binder
    (binding (type-generate-temporary-option $type) $type)
    (value-any $value)))

(define (value-scope-plus-binder ($value-scope : Value-Scope) ($value-binder : Value-Binder)) : Value-Scope
  (define $environment (value-scope-environment $value-scope))
  (define $scope (value-scope-scope $value-scope))
  (define $binding (value-binder-binding $value-binder))
  (define $identifier-option (binding-identifier-option $binding))
  (define $any (value-binder-any $value-binder))
  (define $plus-environment
    (cond
      ($identifier-option (environment-set $environment (syntax-e $identifier-option) $any))
      (else $environment)))
  (define $plus-scope (push $scope $binding))
  (value-scope $plus-environment $plus-scope))

(define (value-scope-plus-binder-stack ($value-scope : Value-Scope) ($binder-stack : (Stackof Value-Binder))) : Value-Scope
  (fold
    $value-scope
    (reverse $binder-stack)
    value-scope-plus-binder))

(define (value-binder-expression ($value-binder : Value-Binder)) : Expression
  (binding-expression (value-binder-binding $value-binder)))
