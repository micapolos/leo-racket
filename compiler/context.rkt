#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/types
  leo/compiler/syntax-type-content-tuple)

(data context
  (scope : Scope)
  (types : Types))

(define null-context (context null-scope null-types))
