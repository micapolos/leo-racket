#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/racket
  leo/typed/type
  leo/typed/types
  leo/typed/type-decompile
  leo/typed/type-utils
  leo/typed/typed
  leo/typed/testing)

(define (any-type-decompile ($any : Any) ($type : Type)) : Any
  (cond
    ((equal? $type boolean-type)
      (if (cast $any Boolean) `true `false))
    ((racket? $type) $any)
    ((arrow? $type) 
      (type-decompile $type))
    ((tuple? $type)
      (define $decompiled-list
        (anys-types-decompile
          (any-types-anys $any (tuple-type-list $type))
          (tuple-type-list $type)))
      (cond 
        ((null? $decompiled-list) (tuple-symbol $type))
        (else (cons (tuple-symbol $type) $decompiled-list))))
    ((any? $type) 
      (type-decompile $type))
    ((thing? $type)
      `(thing ,$any))))

(define 
  (any-types-anys ($any : Any) ($types : (Listof Type))) : (Listof Any)
    (case (length (filter type-is-dynamic? $types))
      ((0) null)
      ((1) (list $any))
      ((2) 
        (let (($pair (cast $any (Pairof Any Any))))
          (list (car $pair) (cdr $pair))))
      (else 
        (let (($vector (cast $any (Vectorof Any))))
          (vector->list $vector)))))

; TODO: make it tail-recursive
(define 
  (anys-types-decompile ($anys : (Listof Any)) ($types : (Listof Type))) : (Listof Any)
    (cond
      ((null? $types) null)
      ((type-is-static? (car $types))
        (cons 
          (any-type-decompile (void) (car $types))
          (anys-types-decompile $anys (cdr $types))))
      (else 
        (cons 
          (any-type-decompile (car $anys) (car $types))
          (anys-types-decompile (cdr $anys) (cdr $types))))))

(check-equal? (any-type-decompile "foo" string-type) "foo")
(check-equal? (any-type-decompile 3.14 number-type) 3.14)
(check-equal? (any-type-decompile 1 fixnum-type) 1)
(check-equal? (any-type-decompile 3.14 flonum-type) 3.14)
(check-equal? (any-type-decompile #t boolean-type) `true)
(check-equal? (any-type-decompile #f boolean-type) `false)

(check-equal? 
  (any-type-decompile `anything (racket `number)) 
  `anything)

(check-equal? 
  (any-type-decompile `anything (tuple `foo null))
  `foo)

(check-equal? 
  (any-type-decompile 
    (cons `a `b)
    (tuple `foo (list number-type (tuple `bar null) string-type)))
  `(foo a bar b))

(check-equal? 
  (any-type-decompile 
    `anything 
    (arrow (list number-type string-type) boolean-type))
  `(giving number string boolean))

(check-equal? 
  (any-type-decompile 
    `anything 
    (any number-type))
  `(any number))
