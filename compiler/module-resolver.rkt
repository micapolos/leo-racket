#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/string
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/syntax-match
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(require/typed racket/base
  (exn:missing-module? (-> Any Boolean)))

(define (module-symbol-tuple-option ($symbol : Symbol)) : (Option Tuple)
  (with-handlers ((exn:missing-module? (lambda (_) #f)))
    (and
      (module-declared? $symbol #t)
      (module-declared? `(submod ,$symbol structure) #t)
      (module-declared? `(submod ,$symbol syntax) #t)
      (module-declared? `(submod ,$symbol unsafe) #t)
      (let ()
        (define $structure
          (dynamic-require
            `(submod ,$symbol structure)
            `$structure
            (lambda () #f)))
        (define $syntax-stack
          (dynamic-require
            `(submod ,$symbol syntax)
            `$syntax-stack
            (lambda () #f)))
        (and $structure $syntax-stack
          (map
            expression
            (any-syntax-stack $syntax-stack)
            (any-structure $structure)))))))

(define (any-syntax-stack ($any : Any)) : (Stackof Syntax)
  (unless (list? $any) (error "not a list"))
  (map any-syntax $any))

(define (any-structure ($any : Any)) : Structure
  (unless (list? $any) (error "not a list"))
  (map (lambda (($item : Any)) (cast $item Type)) $any))

; -------------------------------------------------------------------

(define (module-symbol-tuple-expressions ($symbol : Symbol) ($tuple : Tuple)) : Expressions
  (expressions
    (make-syntax
      `(let ()
        (local-require (submod ,$symbol unsafe))
        ,(syntax-stack-values-syntax
          (filter identifier?
            (map expression-syntax
              (filter expression-dynamic? $tuple))))))
    (tuple-structure $tuple)))

(check-equal?
  (expressions-sexp
    (module-symbol-tuple-expressions
      `leo/module
      (tuple
        (expression
          #`point
          (field! `point
            (field! `x number-type)
            (field! `y number-type)))
        (expression
          null-syntax
          (field! `green (field! `apple)))
        (expression
          #`inc
          (recipe!
            number-type
            (field! `inc)
            (does number-type)))
        (expression
          #`"inline-text"
          text-type)
        (expression
          #`label
          (field! `label text-type)))))
  (expressions-sexp
    (expressions
      #`(let ()
        (local-require (submod leo/module unsafe))
        (values point inc label))
      (structure
        (field! `point (field! `x number-type) (field! `y number-type))
        (field! `green (field! `apple))
        (recipe! number-type (field! `inc) (doing number-type))
        text-type
        (field! `label text-type)))))

; ------------------------------------------------------------------

(define (resolve-module-symbol ($symbol : Symbol)) : (Option Expressions)
  (option-bind (module-symbol-tuple-option $symbol) $tuple
    (module-symbol-tuple-expressions $symbol $tuple)))

(define (type-module-component-symbol-option ($type : Type)) : (Option Symbol)
  (and
    (field? $type)
    (null? (field-structure $type))
    (field-symbol $type)))

(define (structure-module-symbol-option
  ($structure : Structure)) : (Option Symbol)
  (option-bind
    (and
      (not (null? $structure))
      (fold
        (ann null (Option (Stackof Symbol)))
        (map type-module-component-symbol-option $structure)
        (lambda (($symbol-stack-option : (Option (Stackof Symbol))) ($symbol-option : (Option Symbol)))
          (option-app push $symbol-stack-option $symbol-option))))
    $symbol-stack
    (string->symbol (string-join (map symbol->string $symbol-stack) "/"))))

(check-equal?
  (structure-module-symbol-option (structure (field! `leo) (field! `base)))
  `leo/base)

(check-not
  (structure-module-symbol-option (structure)))

(check-not
  (structure-module-symbol-option (structure (field! `leo (field! `base)))))

; -------------------------------------------------------------------------------

(define (structure-resolve-module ($structure : Structure)) : (Option Expressions)
  (option-app resolve-module-symbol (structure-module-symbol-option $structure)))
