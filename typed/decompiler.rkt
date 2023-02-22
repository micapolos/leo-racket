#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/type-any
  leo/typed/type-utils
  leo/typed/typed
  leo/typed/testing)

(define (any-type-decompile ($any : Any) ($type : Type)) : Any
  (cond
    ((equal? $type boolean-type)
      (if (cast $any Boolean) `true `false))
    ((native-type? $type) $any)
    ((symbol-type? $type)
      (symbol-type-symbol $type))
    ((arrow-type? $type) 
      (type-any $type))
    ((field-type? $type)
      (cons
        (field-type-symbol $type)
        (let (($type-body (field-type-body $type)))
          (cond
            ((struct-type-body? $type-body) 
              (anys-types-decompile
                (any-types-anys $any (struct-type-body-type-list $type-body))
                (struct-type-body-type-list $type-body)))
            ((choice-type-body? $type-body) `(TODO))))))
    ((type-type? $type) 
      `(type ,(type-any (type-type-type $type))))))

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
  (any-type-decompile `anything (native-type `number)) 
  `anything)

(check-equal? 
  (any-type-decompile `anything (symbol-type `foo)) 
  `foo)

(check-equal? 
  (any-type-decompile 
    (cons `a `b)
    (field-type `foo 
      (struct-type-body 
        (list number-type (symbol-type `bar) string-type))))
  `(foo a bar b))

(check-equal? 
  (any-type-decompile 
    `anything 
    (arrow-type (list number-type string-type) (list boolean-type)))
  `(giving number string boolean))
