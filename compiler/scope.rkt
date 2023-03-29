#lang leo/typed

(require
  leo/compiler/expression)

(data scope
  (tuple : Tuple))

(data (V) scoped
  (scope : Scope)
  (value : V))
