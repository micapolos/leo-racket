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

(define (type-matches? ($actual : Type) ($expected : Type)) : Boolean
  (and (type-match null $actual $expected) #t))

(define (structure-matches? ($actual : Structure) ($expected : Structure)) : Boolean
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
        (type-matches? (value-type $actual) (value-type $expected))
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
      (or
        (and 
          (generic? $actual) 
          (type-match
            (push $match #f)
            (generic-type $actual)
            (generic-type $expected)))
        (type-match
          (push $match #f)
          $actual
          (generic-type $expected))))
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

(define (type-matches-symbol? ($type : Type) ($symbol : Symbol)) : Boolean
  (and
    (field? $type)
    (equal? (field-symbol $type) $symbol)))

(define (type-matches-selector? ($type : Type) ($selector : Type)) : Boolean
  (or
    ;(and (a? $selector) (type-matches? $type (a-type $selector)))
    (and
      (field? $selector) 
      (and (null? (field-structure $selector)))
      (type-matches-symbol? $type (field-symbol $selector)))))

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
  (type-matches? 
    (racket) 
    (racket)))

(check-not 
  (type-matches? 
    (field! `non-racket)
    (racket)))

; field
(check
  (type-matches?
    (field! `foo)
    (field! `foo)))

(check-not
  (type-matches?
    (field! `foo)
    (field! `bar)))

(check
  (type-matches?
    (field! `foo (field! `a) (field! `b))
    (field! `foo (field! `a) (field! `b))))

(check-not
  (type-matches?
    (field! `foo (field! `a) (field! `a))
    (field! `foo (field! `a) (field! `b))))

(check-not
  (type-matches?
    (field! `foo (field! `a) (field! `b))
    (field! `foo (field! `a))))

(check-not
  (type-matches?
    (field! `foo (field! `a))
    (field! `foo (field! `a) (field! `b))))

; value
(check 
  (type-matches? 
    (value 1 (field! `a)) 
    (value 1 (field! `a))))

(check-not 
  (type-matches? 
    (value 1 (field! `a)) 
    (value 1 (field! `b))))

(check-not 
  (type-matches? 
    (value 1 (field! `a)) 
    (value 2 (field! `a))))

; choice
(check
  (type-matches?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a) (field! `b))))

(check-not
  (type-matches?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a) (field! `c))))

(check-not
  (type-matches?
    (choice! (field! `a) (field! `b))
    (choice! (field! `a))))

; arrow
(check
  (type-matches?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))))

(check-not
  (type-matches?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `c)
      (does (field! `c) (field! `d)))))

(check-not
  (type-matches?
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (does (field! `c) (field! `e)))))

; generic
(check
  (type-matches?
    (generic (field! `a))
    (generic (field! `a))))

(check
  (type-matches?
    (generic (field! `a))
    (generic (generic (field! `a)))))

(check-not
  (type-matches?
    (generic (field! `a))
    (generic (field! `b))))

(check
  (type-matches?
    (field! `a)
    (generic (field! `a))))

(check
  (type-matches?
    (field! `foo (field! `a) (field! `a))
    (generic (field! `foo (variable 0) (variable 0)))))

(check-not
  (type-matches?
    (field! `foo (field! `a) (field! `b))
    (generic (field! `foo (variable 0) (variable 0)))))

(check
  (type-matches?
    (field! `foo (field! `a) (field! `b))
    (generic (generic (field! `foo (variable 0) (variable 1))))))

; specific
(check
  (type-matches?
    (specific (field! `stack) (field! `string))
    (specific (field! `stack) (field! `string))))

(check-not
  (type-matches?
    (specific (field! `stack) (field! `string))
    (specific (field! `list) (field! `string))))

(check-not
  (type-matches?
    (specific (field! `stack) (field! `string))
    (specific (field! `stack) (field! `number))))

(check-not
  (type-matches?
    (field! `stack)
    (specific (field! `stack) (field! `string))))

; recursive
(check
  (type-matches?
    (recursive (field! `a))
    (recursive (field! `a))))

(check
  (type-matches?
    (recursive (variable 0))
    (recursive (variable 0))))

(check-not
  (type-matches?
    (recursive (field! `a))
    (recursive (field! `b))))

(check-not
  (type-matches?
    (field! `a)
    (recursive (field! `a))))

(check-not
  (type-matches?
    (recursive (field! `a))
    (field! `a)))
    
(check
  (type-matches?
    (recursive (recursive (variable 0)))
    (recursive (recursive (variable 1)))))

; universe
(check
  (type-matches?
    (universe 0)
    (universe 0)))

(check-not
  (type-matches?
    (universe 0)
    (universe 1)))
