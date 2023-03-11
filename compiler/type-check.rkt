#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  racket/list
  leo/typed/option
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type)

(define-type Match (Stackof (Option Type)))

(define (type-check? ($actual : Type) ($expected : Type)) : Boolean
  (and (type-match null $actual $expected) #t))

(define (structure-check? ($actual : Structure) ($expected : Structure)) : Boolean
  (and (structure-match null $actual $expected) #t))

(define (type-match ($match : Match) ($actual : Type) ($expected : Type)) : (Option Match)
  (cond
    ((racket? $expected)
      (and
        (racket? $actual)
        $match))
    ((value? $expected)
      (and
        (value? $actual)
        (type-check? (value-type $actual) (value-type $expected))
        (equal? (value-any $actual) (value-any $expected))
        $match))
    ((field? $expected)
      (and
        (field? $actual) 
        (equal? 
          (field-symbol $actual) 
          (field-symbol $expected))
        (structure-match
          $match
          (field-structure $actual) 
          (field-structure $expected))
        $match))
    ((choice? $expected)
      (and
        (choice? $actual)
        (type-stack-match
          $match
          (choice-type-stack $actual)
          (choice-type-stack $expected))))
    ((arrow? $expected)
      (and
        (arrow? $actual)
        (option-app structure-match
          (structure-match
            $match
            (arrow-from-structure $actual)
            (arrow-from-structure $expected))
          (arrow-to-structure $actual)
          (arrow-to-structure $expected))))
    ((generic? $expected)
      (cond
        ((generic? $actual) 
          (type-match
            (push $match #f)
            (generic-type $actual)
            (generic-type $expected)))
        (else
          (type-match
            (push $match #f)
            $actual
            (generic-type $expected)))))
    ((specific? $expected)
      (and
        (specific? $actual)
        (option-app type-match
          (type-match
            $match
            (specific-type $actual)
            (specific-type $expected))
          (specific-argument-type $actual)
          (specific-argument-type $expected))))
    ((recursive? $expected)
      (and
        (recursive? $actual)
        (type-match
          (push $match #f)
          (recursive-type $actual)
          (recursive-type $expected))))
    ((variable? $expected)
      (bind $index (variable-index $expected)
        (bind $type-option (list-ref $match $index)
          (cond
            ($type-option (type-match $match $actual $type-option))
            (else (list-set $match $index $actual))))))
    ((universe? $expected)
      (and
        (universe? $actual)
        (= 
          (universe-index $actual)
          (universe-index $expected))
        $match))
    (else #f)))

(define (type-check-symbol? ($type : Type) ($symbol : Symbol)) : Boolean
  (and
    (field? $type)
    (equal? (field-symbol $type) $symbol)))

(define (type-check-selector? ($type : Type) ($selector : Type)) : Boolean
  (or
    ;(and (a? $selector) (type-check? $type (a-type $selector)))
    (and
      (field? $selector) 
      (and (null? (field-structure $selector)))
      (type-check-symbol? $type (field-symbol $selector)))))

(define (type-stack-match
  ($match : Match)
  ($actual-type-stack : (Stackof Type))
  ($expected-type-stack : (Stackof Type)))
  : (Option Match)
  (and 
    (= (length $actual-type-stack) (length $expected-type-stack))
    (foldl
      (lambda (($actual-type : Type) ($expected-type : Type) ($match-option : (Option Match)))
        (and $match-option
          (type-match $match-option $actual-type $expected-type)))
      $match
      $actual-type-stack
      $expected-type-stack)))

(define structure-match type-stack-match)

; ---------------------------------------------------------------------------------

; racket
(check 
  (type-check? 
    (racket) 
    (racket)))

(check-not 
  (type-check? 
    (field! `non-racket)
    (racket)))

; field
(check
  (type-check?
    (field! `foo)
    (field! `foo)))

(check-not
  (type-check?
    (field! `foo)
    (field! `bar)))

(check
  (type-check?
    (field! `foo (field! `a) (field! `b))
    (field! `foo (field! `a) (field! `b))))

(check-not
  (type-check?
    (field! `foo (field! `a) (field! `a))
    (field! `foo (field! `a) (field! `b))))

(check-not
  (type-check?
    (field! `foo (field! `a) (field! `b))
    (field! `foo (field! `a))))

(check-not
  (type-check?
    (field! `foo (field! `a))
    (field! `foo (field! `a) (field! `b))))

; value
(check 
  (type-check? 
    (value 1 (field! `a)) 
    (value 1 (field! `a))))

(check-not 
  (type-check? 
    (value 1 (field! `a)) 
    (value 1 (field! `b))))

(check-not 
  (type-check? 
    (value 1 (field! `a)) 
    (value 2 (field! `a))))

; choice
(check
  (type-check?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a) (field! `b))))

(check-not
  (type-check?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a) (field! `c))))

(check-not
  (type-check?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a))))

; arrow
(check
  (type-check?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))))

(check-not
  (type-check?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `c)
      (does (field! `c) (field! `d)))))

(check-not
  (type-check?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `e)))))

; generic
(check
  (type-check?
    (generic (field! `a))
    (generic (field! `a))))

(check-not
  (type-check?
    (generic (field! `a))
    (generic (field! `b))))

(check
  (type-check?
    (field! `a)
    (generic (field! `a))))

(check
  (type-check?
    (field! `foo (field! `a) (field! `a))
    (generic (field! `foo (variable 0) (variable 0)))))

(check-not
  (type-check?
    (field! `foo (field! `a) (field! `b))
    (generic (field! `foo (variable 0) (variable 0)))))

(check
  (type-check?
    (field! `foo (field! `a) (field! `b))
    (generic (generic (field! `foo (variable 0) (variable 1))))))

; specific
(check
  (type-check?
    (specific (field! `stack) (field! `string))
    (specific (field! `stack) (field! `string))))

(check-not
  (type-check?
    (specific (field! `stack) (field! `string))
    (specific (field! `list) (field! `string))))

(check-not
  (type-check?
    (specific (field! `stack) (field! `string))
    (specific (field! `stack) (field! `number))))

(check-not
  (type-check?
    (field! `stack)
    (specific (field! `stack) (field! `string))))

; recursive
(check
  (type-check?
    (recursive (field! `a))
    (recursive (field! `a))))

(check
  (type-check?
    (recursive (variable 0))
    (recursive (variable 0))))

(check-not
  (type-check?
    (recursive (field! `a))
    (recursive (field! `b))))

(check-not
  (type-check?
    (field! `a)
    (recursive (field! `a))))

(check-not
  (type-check?
    (recursive (field! `a))
    (field! `a)))
    
(check
  (type-check?
    (recursive (recursive (variable 0)))
    (recursive (recursive (variable 1)))))

; universe
(check
  (type-check?
    (universe 0)
    (universe 0)))

(check-not
  (type-check?
    (universe 0)
    (universe 1)))
