#lang leo/typed

(require
  leo/compiler/type)

(define (type-resolve ($type : Type)) : Type
  (scope-type-resolve null $type))

(define (scope-type-resolve ($scope : (Stackof Type)) ($type : Type)) : Type
  (cond
    ((racket? $type) $type)
    ((value? $type) $type)
    ((variable? $type) (stack-ref $scope (variable-index $type)))
    ((field? $type)
      (field
        (field-symbol $type)
        (scope-structure-resolve $scope (field-structure $type))))
    ((choice? $type)
      (choice
        (scope-structure-resolve $scope (choice-type-stack $type))))
    ((arrow? $type)
      (arrow
        (scope-structure-resolve $scope (arrow-from-structure $type))
        (scope-structure-resolve $scope (arrow-to-structure $type))))
    ((generic? $type) $type)
    ((specific? $type)
      (scope-type-resolve
        (push $scope (specific-argument-type $type))
        (specific-type $type)))
    ((recursive? $type) $type)
    ((universe? $type) $type)
    ((reified? $type) $type)))

(define (scope-structure-resolve ($scope : (Stackof Type)) ($structure : Structure)) : Structure
  (map (curry scope-type-resolve $scope) $structure))
