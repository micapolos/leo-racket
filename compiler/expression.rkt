#lang leo/typed

(require
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(data expression (syntax-option : (Option Syntax)) (type : Type))

(define-type Selection (U Type Expression))

(define-type Tuple (Stackof Expression))

(define tuple : (-> Expression * Tuple) stack)

(define null-tuple null)

(define-type Select-Expression-Line (U Expression-The Type-Not))

(data expression-the (expression : Expression))
