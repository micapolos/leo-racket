#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-set ($type : Type) ($variable : Variable) ($set-type : Type)) : Type
  (cond
    ((racket? $type) $type)
    ((field? $type)
      (field
        (field-symbol $type)
        (structure-set (field-structure $type) $variable $set-type)))
    ((choice? $type)
      (choice
        (structure-set (choice-type-stack $type) $variable $set-type)))
    ((arrow? $type)
      (arrow
        (structure-set (arrow-from-structure $type) $variable $set-type)
        (structure-set (arrow-to-structure $type) $variable $set-type)))
    ((generic? $type)
      (generic
        (type-set
          (generic-type $type)
          (variable-next $variable)
          $set-type)))
    ((specific? $type)
      (specific
        (type-set (specific-type $type) $variable $set-type)
        (type-set (specific-argument-type $type) $variable $set-type)))
    ((recursive? $type)
      (recursive
        (type-set (recursive-type $type) (variable-next $variable) $set-type)))
    ((variable? $type)
      (cond
        ((= (variable-index $type) (variable-index $variable)) $set-type)
        (else $type)))
    ((universe? $type) $type)
    ((reified? $type) $type)))

(define (structure-set ($structure : Structure) ($variable : Variable) ($set-type : Type)) : Structure
  (map
    (lambda (($type : Type))
      (type-set $type $variable $set-type))
    $structure))

(check-equal?
  (type-set (variable 2) (variable 2) (field! `ok))
  (field! `ok))

(check-equal?
  (type-set (variable 3) (variable 2) (field! `ok))
  (variable 3))

(check-equal?
  (type-set (field! `foo (variable 2) (variable 3)) (variable 2) (field! `ok))
  (field! `foo (field! `ok) (variable 3)))

(check-equal?
  (type-set (choice! (variable 2) (variable 3)) (variable 2) (field! `ok))
  (choice! (field! `ok) (variable 3)))

(check-equal?
  (type-set (recipe! (variable 2) (variable 3) (doing (variable 2) (variable 3))) (variable 2) (field! `ok))
  (recipe! (field! `ok) (variable 3) (doing (field! `ok) (variable 3))))

(check-equal?
  (type-set (generic (variable 2)) (variable 2) (field! `ok))
  (generic (variable 2)))

(check-equal?
  (type-set (generic (variable 3)) (variable 2) (field! `ok))
  (generic (field! `ok)))

(check-equal?
  (type-set (specific (variable 2) (variable 2)) (variable 2) (field! `ok))
  (specific (field! `ok) (field! `ok)))

(check-equal?
  (type-set (specific (variable 2) (variable 3)) (variable 2) (field! `ok))
  (specific (field! `ok) (variable 3)))

(check-equal?
  (type-set (specific (variable 3) (variable 2)) (variable 2) (field! `ok))
  (specific (variable 3) (field! `ok)))

(check-equal?
  (type-set (recursive (variable 2)) (variable 2) (field! `ok))
  (recursive (variable 2)))

(check-equal?
  (type-set (recursive (variable 3)) (variable 2) (field! `ok))
  (recursive (field! `ok)))

