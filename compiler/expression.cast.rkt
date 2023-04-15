#lang leo/typed

(require
  leo/compiler/select-expression-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (expression-cast ($expression : Expression) ($type : Type)) : (Option Expression)
  (cond
    ((choice? $type)
      (expression-choice-cast $expression $type))
    ((field? $type)
      (expression-field-cast $expression $type))
    ((generic? $type)
      (option-bind (expression (expression-syntax-option $expression) (generic-type $type)) $cast-expression
        (expression
          (expression-syntax-option $cast-expression)
          (generic (expression-type $cast-expression)))))
    ((recursive? $type)
      (option-bind (expression (expression-syntax-option $expression) (recursive-type $type)) $cast-expression
        (expression
          (expression-syntax-option $cast-expression)
          (recursive (expression-type $cast-expression)))))
    (else #f)))

(define (tuple-cast ($tuple : Tuple) ($structure : Structure)) : (Option Tuple)
  (option-app stack
    (option-app expression-cast
      (single $tuple)
      (single $structure))))

; ------------------------------------------------------------------------------------

(define (expression-choice-cast ($expression : Expression) ($choice : Choice)) : (Option Expression)
  (define $structure (choice-type-stack $choice))
  (option-bind (structure-index-matching-type $structure (expression-type $expression)) $index
  (index-syntax-structure-select-expression-option
    $index
    (expression-syntax-option $expression)
    $structure)))

(bind $choice (choice! (field! `foo) (field! `bar) (field! `goo))
  (check-equal?
    (option-app expression-sexp
      (expression-choice-cast (field-expression! foo) $choice))
    (expression-sexp
      (expression #`2 $choice)))

  (check-equal?
    (option-app expression-sexp
      (expression-choice-cast (field-expression! zoo) $choice))
  #f))

; ------------------------------------------------------------------------------------

(define (expression-field-cast ($expression : Expression) ($field : Field)) : (Option Expression)
  (define $expression-type (expression-type $expression))
  (and
    (field? $expression-type)
    (equal? (field-symbol $expression-type) (field-symbol $field))
    (option-bind (single (field-structure $expression-type)) $expression-rhs-type
      (option-bind (single (field-structure $field)) $field-rhs-type
        (expression-cast
          (expression (expression-syntax-option $expression) $expression-rhs-type)
          $field-rhs-type)))))
