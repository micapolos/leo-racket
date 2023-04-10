#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/generate-temporary)

(define (symbol-expression
  ($symbol : Symbol)
  ($return-type : Type)
  ($identifier-option : (Option Identifier)))
  (expression
    $identifier-option
    (arrow
      (structure (field! $symbol))
      (structure $return-type))))

(define (unit-expression-2
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($return-type : Type)
  ($syntax : Syntax))
  (expression
    $syntax
    (arrow
      (structure 
        (field! $symbol-1
          (field! $symbol-2)))
      (structure $return-type))))

(define (unary-expression 
  ($lhs-type : Type)
  ($symbol : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field! $symbol))
      (structure $return-type))))

(define (unary-expression-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field! $symbol-1)
        (field! $symbol-2))
      (structure $return-type))))

(define (unary-nested-expression-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure
            (field! $symbol-2))))
      (structure $return-type))))

(define (unary-nested-expression-3
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($symbol-3 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2
              (structure
                (field! $symbol-3))))))
      (structure $return-type))))

(define (binary-expression 
  ($lhs-type : Type)
  ($symbol : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field $symbol (structure $rhs-type)))
      (structure $return-type))))

(define (binary-expression-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2 
              (structure $rhs-type)))))
      (structure $return-type))))

(define (binary-expression-3
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($symbol-3 : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2 
              (structure 
                (field $symbol-3
                  (structure $rhs-type)))))))
      (structure $return-type))))

(define (binary-expression-4
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($symbol-3 : Symbol)
  ($symbol-4 : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (expression
    $identifier
    (arrow
      (structure 
        $lhs-type
        (field! $symbol-1
          (field! $symbol-2 
            (field! $symbol-3
              (field! $symbol-4 $rhs-type)))))
      (structure $return-type))))
