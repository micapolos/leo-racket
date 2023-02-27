#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/racket
  leo/typed/type
  leo/typed/syntax-match
  leo/typed/types)

(define (type-syntax ($type : Type)) : Syntax
  (cond
    ((thing? $type) 
      #`(thing))
    ((racket? $type) 
      (cast-syntax
        #`(racket (quote #,(racket-any $type)))))
    ((tuple? $type)
      (cast-syntax
        #`(tuple 
          (quote #,(tuple-symbol $type))
          (list #,@(map type-syntax (tuple-type-list $type))))))
    ((arrow? $type) 
      (cast-syntax
        #`(arrow 
          (list #,@(map type-syntax (arrow-lhs-types $type)))
          #,(type-syntax (arrow-rhs-type $type)))))
    ((any? $type) 
      (cast-syntax
        #`(any #,(type-syntax (any-type $type)))))))
