#lang typed/racket/base

(require leo/compiler/leo)

(leo
  number squared
  (doing number (times number)))