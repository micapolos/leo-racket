#lang leo/typed

(require
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
    ((field? $expected)
      (and
        (field? $actual) 
        (equal? 
          (field-symbol $actual) 
          (field-symbol $expected))
        (structure-match
          $match
          (field-structure $actual) 
          (field-structure $expected))))
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
    ((recursive? $expected)
      (or
        (and
          (recursive? $actual)
          (type-match
            (push $match #f)
            (recursive-type $actual)
            (recursive-type $expected)))
        (type-match
          (push $match #f)
          $actual
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
        (= (universe-index $actual) (universe-index $expected))
        $match))
    ((reified? $expected)
      (and
        (reified? $actual)
        (structure-match
          $match
          (reified-structure $actual)
          (reified-structure $expected))))
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
      (doing (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (doing (field! `c) (field! `d)))))

(check-not
  (type-matches?
    (recipe!
      (field! `a) (field! `b)
      (doing (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `c)
      (doing (field! `c) (field! `d)))))

(check-not
  (type-matches?
    (recipe!
      (field! `a) (field! `b)
      (doing (field! `c) (field! `d)))
    (recipe!
      (field! `a) (field! `b)
      (doing (field! `c) (field! `e)))))

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

; recursive
(check
  (type-matches?
    (field! `a)
    (recursive (field! `a))))

(check
  (type-matches?
    (recursive (field! `a))
    (recursive (field! `a))))

(check
  (type-matches?
    (recursive (field! `a (variable 0)))
    (recursive (field! `a (variable 0)))))

(check
  (type-matches?
    (recursive (field! `a (field! `a (variable 0))))
    (recursive (field! `a (variable 0)))))

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

(check-not
  (type-matches?
    (universe 0)
    (field! `not-universe)))

(check
  (type-matches?
    (reified! (racket))
    (generic (reified! (variable 0)))))
