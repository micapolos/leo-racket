#lang leo/typed

(require
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(data expression (syntax : Syntax) (type : Type))

(define-type Selection (U Type Expression))

(define-type Tuple (Stackof Expression))

(define tuple : (-> Expression * Tuple) stack)

(define null-tuple null)

(define (empty-expression ($type : Type)) : Expression 
  (expression null-syntax $type))
