#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/type)

(define (type-syntax ($type : Type)) : Syntax
  (make-syntax
    (cond
      ((racket? $type) `(racket))
      ((field? $type)
        `(field!
          (quote ,(field-symbol $type))
          ,@(structure-syntax-list (field-structure $type))))
      ((choice? $type)
        `(choice!
          ,@(structure-syntax-list (choice-type-stack $type))))
      ((arrow? $type)
        `(recipe!
          ,@(structure-syntax-list(arrow-from-structure $type))
          (does ,@(structure-syntax-list (arrow-to-structure $type)))))
      ((generic? $type)
        `(generic
          ,(type-syntax (generic-type $type))))
      ((specific? $type)
        `(specific
          ,(type-syntax (specific-type $type))
          ,(type-syntax (specific-argument-type $type))))
      ((recursive? $type)
        `(recursive ,(type-syntax (recursive-type $type))))
      ((variable? $type)
        `(variable ,(variable-index $type)))
      ((universe? $type)
        `(universe ,(universe-index $type)))
      ((value? $type)
        `(value
          ,(sexp-datum (any-sexp (value-any $type)))
          ,(type-syntax (value-type $type)))))))

(define (structure-syntax ($structure : Structure)) : Syntax
  (make-syntax
    `(structure ,@(structure-syntax-list $structure))))

(define (structure-syntax-list ($structure : Structure)) : (Listof Syntax)
  (reverse (map type-syntax $structure)))
