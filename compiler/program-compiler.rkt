#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expression
  leo/compiler/program)

(data program-compiler
  (tuple : Tuple)
  (program : Program))
