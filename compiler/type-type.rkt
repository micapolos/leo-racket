#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-type ($type : Type)) : Type
  (cond
    ((equal? $type (null-field `number)) number-type)
    ((equal? $type (null-field `text)) text-type)
    ((equal? $type (null-field `int)) int-type)
    ((equal? $type (null-field `float)) float-type)
    ((equal? $type (null-field `boolean)) boolean-type)
    ((equal? $type (null-field `racket)) (racket))
    ((racket? $type) (a (null-field `racket)))
    ((field? $type)
      (field 
        (field-symbol $type) 
        (structure-structure (field-structure $type))))
    ((choice? $type)
      (a (field `choice (structure-structure (choice-structure $type)))))
    ((arrow? $type) 
      (a 
        (field `function 
          (push 
            (arrow-lhs-structure $type)
            (field `giving (arrow-rhs-structure $type))))))
    ((a? $type) (a $type))))

(define (structure-structure ($structure : Structure)) : Structure
  (map type-type $structure))
