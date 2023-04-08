#lang leo/typed

(require
  leo/compiler/type)

(data binding
  (identifier-option : (Option Identifier))
  (type : Type))
