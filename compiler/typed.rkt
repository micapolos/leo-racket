#lang leo/typed

(require
  leo/compiler/type)

(data (Value) typed
  (value : Value)
  (type : Type))
