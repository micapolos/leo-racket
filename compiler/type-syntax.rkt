#lang leo/typed

(require
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/type)

(define (type-syntax ($type : Type)) : Syntax
  (make-syntax
    (cond
      ((racket? $type)
        `(racket))
      ((field? $type)
        `(field
          (quote ,(field-symbol $type))
          ,(structure-syntax (field-structure $type))))
      ((choice? $type)
        `(choice
          ,(structure-syntax (choice-type-stack $type))))
      ((arrow? $type)
        `(arrow
          ,(structure-syntax (arrow-from-structure $type))
          ,(structure-syntax (arrow-to-structure $type))))
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
      ((reified? $type)
        `(reified
          ,(structure-syntax (reified-structure $type)))))))

(define (structure-syntax ($structure : Structure)) : Syntax
  (make-syntax
    `(structure ,@(structure-syntax-list $structure))))

(define (structure-syntax-list ($structure : Structure)) : (Listof Syntax)
  (reverse (map type-syntax $structure)))
