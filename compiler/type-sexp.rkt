#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/list
  racket/unsafe/ops
  leo/typed/base
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-sexp
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-sexp ($type : Type)) : Sexp
  (cond
    ((racket? $type) `racket)
    ((equal? $type number-type) `number)
    ((equal? $type text-type) `text)
    ((equal? $type int-type) `int)
    ((equal? $type float-type) `float)
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (if (null? $structure)
        $symbol
        `(,$symbol ,@(structure-sexp-list $structure))))
    ((choice? $type)
      `(one (of ,@(structure-sexp-list (choice-type-stack $type)))))
    ((arrow? $type) 
      (define $lhs-structure (arrow-from-structure $type))
      (define $rhs-structure (arrow-to-structure $type))
      `(recipe 
        ,@(structure-sexp-list $lhs-structure)
        (doing ,@(structure-sexp-list $rhs-structure))))
    ((generic? $type) 
      `(generic ,(type-sexp (generic-type $type))))
    ((specific? $type) 
      `(specific 
        ,(type-sexp (specific-type $type))
        (of ,(type-sexp (specific-argument-type $type)))))
    ((recursive? $type) 
      `(recursive ,(type-sexp (recursive-type $type))))
    ((variable? $type) 
      `(variable ,(variable-index $type)))
    ((universe? $type) `(universe ,(universe-index $type)))
    ((value? $type) `(value ,(value-sexp $type)))))

(define (structure-sexp-list ($structure : Structure)) : (Listof Sexp)
  (reverse (map type-sexp $structure)))

(define (structure-sexp ($structure : Structure)) : Sexp
  `(structure ,@(structure-sexp-list $structure)))

(check-equal? (type-sexp (racket)) `racket)

(check-equal? (type-sexp number-type) `number)
(check-equal? (type-sexp text-type) `text)
(check-equal? (type-sexp float-type) `float)
(check-equal? (type-sexp int-type) `int)
(check-equal? (type-sexp boolean-type) `(boolean (one (of true false))))

(check-equal? (type-sexp (field `foo null)) `foo)
(check-equal? (type-sexp (field `foo (structure (racket)))) `(foo racket))

(check-equal? (type-sexp (choice!)) `(one (of)))
(check-equal? (type-sexp (choice! (field! `zero) (field! `one))) `(one (of zero one)))

(check-equal? (type-sexp (recursive (field! `foo))) `(recursive foo))

(check-equal? (type-sexp (generic (field! `foo))) `(generic foo))
(check-equal? (type-sexp (specific (field! `foo) (field! `bar))) `(specific foo (of bar)))

(check-equal? (type-sexp (variable 0)) `(variable 0))

(check-equal? 
  (type-sexp 
    (field `foo (structure (field! `bar) (field! `zoo))))
  `(foo bar zoo))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack number-type text-type)
      (stack text-type int-type)))
  `(recipe number text (doing text int)))

(check-equal? (type-sexp (universe 128)) `(universe 128))

; ----------------------------------------------------------------------------

(define (value-sexp ($value : Value)) : Sexp
  (type-stack-value-sexp null $value))

(define (any-structure-sexp-list ($any : Any) ($structure : Structure)) : (Listof Sexp)
  (type-stack-any-structure-sexp-list null $any $structure))

(define (type-stack-value-sexp 
  ($type-stack : (Stackof Type))
  ($value : Value)) : Sexp
  (define $type (value-type $value))
  (define $any (value-any $value))
  (cond
    ((racket? $type) (any-sexp $any))
    ((equal? $type int-type) `(int ,(cast $any Fixnum)))
    ((equal? $type float-type) `(float ,(cast $any Flonum)))
    ((equal? $type number-type) (cast $any Number))
    ((equal? $type boolean-type) `(boolean ,(if (cast $any Boolean) `true `false)))
    ((equal? $type text-type) (cast $any String))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (cond
        ((null? $structure) $symbol)
        (else 
          `(
            ,$symbol
            ,@(type-stack-any-structure-sexp-list $type-stack $any $structure)))))
    ((choice? $type)
      (type-stack-value-sexp $type-stack (any-choice-value $any $type)))
    ((arrow? $type) (type-sexp $type))
    ((generic? $type) 
      (type-stack-value-sexp
        $type-stack 
        (value $any (generic-type $type))))
    ((specific? $type) 
      (type-stack-value-sexp
        (push $type-stack (specific-argument-type $type))
        (value $any (specific-type $type))))
    ((recursive? $type) 
      (type-stack-value-sexp
        (push $type-stack (recursive-type $type))
        (value $any (recursive-type $type))))
    ((variable? $type) 
      (type-stack-value-sexp
        $type-stack 
        (value $any (list-ref $type-stack (variable-index $type)))))
    ((universe? $type)
      (if (= (universe-index $type) 0)
        (type-sexp (cast $any Type))
        (value-sexp (value $any (universe (sub1 (universe-index $type)))))))
    ((value? $type) 
      `(value ,(type-stack-value-sexp $type-stack $type)))))

(define (any-choice-value 
  ($any : Any) 
  ($choice : Choice)) : Value
  (define $structure (choice-type-stack $choice))
  (define $size (length $structure))
  (case $size
    ((0) (error "null choice"))
    ((1) (value $any (top $structure)))
    (else
      (define $dynamic? (structure-dynamic? $structure))
      (define-values ($selector $value) 
        (if $dynamic? 
          (bind $pair (cast $any (Pairof (U Exact-Nonnegative-Integer Boolean) Any))
            (values (car $pair) (cdr $pair)))
          (values $any #f)))
      (define $index 
        (if (= $size 2) 
          (if (cast $selector Boolean) 0 1)
          (cast $selector Exact-Nonnegative-Integer)))
      (value $value (list-ref (reverse $structure) $index)))))

(define (type-stack-any-structure-sexp-list 
  ($type-stack : (Stackof Type)) 
  ($any : Any) 
  ($structure : Structure)) : (Listof Sexp)
  (reverse
    (map
      (lambda (($index : Exact-Nonnegative-Integer))
        (type-stack-value-sexp 
          $type-stack 
          (any-structure-ref $any $structure $index)))
      (range (length $structure)))))

(define (any-structure-ref
  ($any : Any)
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer))
  : Value
  (define $structure-dynamic-size (structure-dynamic-size $structure))
  (define $dynamic-index (structure-dynamic-ref $structure $index))
  (value 
    (and
      $dynamic-index
      (case $structure-dynamic-size
        ((0) (error "impossible"))
        ((1) $any)
        ((2)
          ((if (= $dynamic-index 1) unsafe-car unsafe-cdr) (cast $any (Pairof Any Any))))
        (else
          (unsafe-vector-ref 
            (cast $any (Vectorof Any))
            (- $structure-dynamic-size $dynamic-index 1)))))
    (list-ref $structure $index)))

(check-equal?
  (value-sexp (value #t boolean-type))
  `(boolean true))

(check-equal?
  (value-sexp (value #f boolean-type))
  `(boolean false))

(check-equal?
  (value-sexp (value 3.14 number-type))
  3.14)

(check-equal?
  (value-sexp (value 1 int-type))
  `(int 1))

(check-equal?
  (value-sexp (value 3.14 float-type))
  `(float 3.14))

(check-equal?
  (value-sexp (value "foo" text-type))
  "foo")

(check-equal?
  (value-sexp (value `(quote 1 2 3) (racket)))
  `(quote 1 2 3))

(check-equal?
  (value-sexp 
    (value `foo 
      (arrow 
        (stack text-type) 
        (stack number-type))))
  `(recipe text (doing number)))

(check-equal?
  (value-sexp (value "foo" (field `foo (stack text-type))))
  `(foo "foo"))

(check-equal?
  (value-sexp 
    (value 
      (cons 128 "foo") 
      (field `foo (stack number-type (field! `bar) text-type))))
  `(foo 128 bar "foo"))

(check-equal?
  (value-sexp 
    (value 
      (vector 128 "foo" #t) 
      (field `foo (stack number-type (field! `bar) text-type boolean-type))))
  `(foo 128 bar "foo" (boolean true)))

(check-equal?
  (value-sexp 
    (value 
      #f
      (choice (structure (field! `foo)))))
  `foo)

(check-equal?
  (value-sexp 
    (value 
      "foo"
      (choice (structure text-type))))
  "foo")

(check-equal?
  (value-sexp 
    (value 
      #t
      (choice (structure (field! `foo) (field! `bar)))))
  `foo)

(check-equal?
  (value-sexp 
    (value 
      #f
      (choice (structure (field! `foo) (field! `bar)))))
  `bar)

(check-equal?
  (value-sexp 
    (value 
      (cons #t 123)
      (choice (structure number-type text-type))))
  123)

(check-equal?
  (value-sexp 
    (value 
      (cons #f "foo") 
      (choice (structure number-type text-type))))
  "foo")

(check-equal?
  (value-sexp 
    (value 
      0
      (choice (structure (field! `foo) (field! `bar) (field! `zoo)))))
  `foo)

(check-equal?
  (value-sexp 
    (value 
      1
      (choice (structure (field! `foo) (field! `bar) (field! `zoo)))))
  `bar)

(check-equal?
  (value-sexp 
    (value 
      2
      (choice (structure (field! `foo) (field! `bar) (field! `zoo)))))
  `zoo)

(check-equal?
  (value-sexp 
    (value 
      (cons 0 123) 
      (choice (structure number-type (field! `foo) text-type))))
  123)

(check-equal?
  (value-sexp 
    (value 
      (cons 1 #t) 
      (choice (structure number-type (field! `foo) text-type))))
  `foo)

(check-equal?
  (value-sexp 
    (value 
      (cons 2 "foo") 
      (choice (structure number-type (field! `foo) text-type))))
  "foo")

(check-equal?
  (value-sexp 
    (value 
      number-type
      type-type))
  `number)

(check-equal?
  (value-sexp 
    (value 
      (universe 0)
      (universe 1)))
  `(universe 0))
