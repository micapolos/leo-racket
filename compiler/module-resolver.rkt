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
  leo/compiler/type-utils
  leo/compiler/any-sexp)

(require/typed racket/base
  (exn:missing-module? (-> Any Boolean)))

(define (module-path-syntax ($module-path : Module-Path)) : Syntax
  (any-syntax
    (datum->syntax #f $module-path)))

(define (module-path-tuple-option ($module-path : Module-Path)) : (Option Tuple)
  (with-handlers ((exn:missing-module? (lambda (_) #f)))
    (and
      (module-declared? $module-path #t)
      (module-declared? `(submod ,$module-path structure) #t)
      (module-declared? `(submod ,$module-path syntax) #t)
      (module-declared? `(submod ,$module-path unsafe) #t)
      (let ()
        (define $structure
          (dynamic-require
            `(submod ,$module-path structure)
            `$structure
            (lambda () #f)))
        (define $syntax-stack
          (dynamic-require
            `(submod ,$module-path syntax)
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

(define (module-path-tuple-expressions ($module-path : Module-Path) ($tuple : Tuple)) : Expressions
  (expressions
    (make-syntax
      `(let ()
        (local-require (submod ,(module-path-syntax $module-path) unsafe))
        ,(syntax-stack-values-syntax
          (filter identifier?
            (map expression-syntax
              (filter expression-dynamic? $tuple))))))
    (tuple-structure $tuple)))

(check-equal?
  (expressions-sexp
    (module-path-tuple-expressions
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

(define (module-path-resolve ($module-path : Module-Path)) : (Option Expressions)
  (option-bind (module-path-tuple-option $module-path) $tuple
    (module-path-tuple-expressions $module-path $tuple)))

(define (type-module-component-symbol-option ($type : Type)) : (Option Symbol)
  (and
    (field? $type)
    (null? (field-structure $type))
    (field-symbol $type)))

(define (structure-module-path-option
  ($structure : Structure)) : (Option Module-Path)
  (option-bind
    (and
      (not (null? $structure))
      (fold
        null
        (map type-module-component-symbol-option $structure)
        (lambda (($symbol-stack-option : (Option (Stackof Symbol))) ($symbol-option : (Option Symbol)))
          (option-app push $symbol-stack-option $symbol-option))))
    $symbol-stack
    `(lib
      ,(string-append
        (string-join (map symbol->string (push $symbol-stack `leo)) "/")
        ".leo"))))

(check-equal?
  (structure-module-path-option (structure (field! `foo) (field! `bar)))
  `(lib "leo/foo/bar.leo"))

(check-not
  (structure-module-path-option (structure)))

(check-not
  (structure-module-path-option (structure (field! `leo (field! `base)))))

; -------------------------------------------------------------------------------

(define (structure-resolve-module ($structure : Structure)) : (Option Expressions)
  (option-app module-path-resolve (structure-module-path-option $structure)))

(check-equal?
  (option-app expressions-sexp
    (structure-resolve-module
      (structure (field! `compiler) (field! `tester))))
  (expressions-sexp
    (expressions
      #`(let () (local-require (submod (lib "leo/compiler/tester.leo") unsafe))
        (values tmp-text tmp-number))
      (structure
        text-type
        number-type
        (field! `color (field! `red))))))
