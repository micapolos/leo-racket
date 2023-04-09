#lang leo/typed

(require
  leo/compiler/type)

(data value-binding
  (symbol-option : (Option Symbol))
  (type : Type))

(define-type Value-Scope
  (Stackof Value-Binding))

(define value-scope : (-> Value-Binding * Value-Scope) stack)

(define null-value-scope : Value-Scope (value-scope))
