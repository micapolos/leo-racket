#lang leo/typed

(require
  leo/compiler/expressions)

(define-type Ingredients (Stackof Expressions))

(define null-ingredients null)

(define ingredients : (-> Expressions * (Stackof Expressions)) stack)
