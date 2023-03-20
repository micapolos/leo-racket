#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack)

(define-type (Term V)
  (U
    (Native V)
    Index
    (Tuple (Term V))
    (Ref (Term V))
    (Switch (Term V))
    Variable
    (Abstraction (Term V))
    (Application (Term V))
    (Binder (Term V))))

(data (V) native
  (value : V))

(data index
  (value : Exact-Nonnegative-Integer)
  (size : Exact-Nonnegative-Integer))

(data (V) tuple
  (stack : (Stackof V)))

(data (V) terms
  (stack : (Stackof V)))

(data (V) ref
  (tuple : V)
  (index : Index))

(data (V) switch
  (selector : V)
  (case-stack : (Stackof V)))

(data variable
  (index : Exact-Nonnegative-Integer))

(data (V) abstraction
  (symbol-stack : (Stackof Symbol))
  (terms : (Terms V)))

(data (V) application
  (lhs : V)
  (rhs-stack : (Stackof V)))

(data (V) binding
  (symbol-stack : (Stackof Symbol))
  (body : V))

(data (V) binder
  (recursiveness : (U 'recursive 'non-recursive))
  (binding-stack : (Stackof (Binding V)))
  (terms : (Terms V)))
