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

; --------------------------------------------------------------------------

(define (symbol-stack-plus-type-module-components
  ($symbol-stack : (Stackof Symbol))
  ($type : Type))
: (Option (Stackof Symbol))
  (and
    (field? $type)
    (symbol-stack-plus-structure-module-components
      (push $symbol-stack (field-symbol $type))
      (field-structure $type))))

(define (symbol-stack-plus-structure-module-components
  ($symbol-stack : (Stackof Symbol))
  ($structure : Structure)) : (Option (Stackof Symbol))
  (cond
    ((null? $structure) $symbol-stack)
    (else
      (option-bind (single $structure) $type
        (symbol-stack-plus-type-module-components $symbol-stack $type)))))

(define (type-module-components ($type : Type)) : (Option (Stackof Symbol))
  (symbol-stack-plus-type-module-components null $type))

(define (structure-module-components ($structure : Structure)) : (Option (Stackof Symbol))
  (symbol-stack-plus-structure-module-components null $structure))

(check-equal?
  (type-module-components (field! `foo))
  (stack `foo))

(check-equal?
  (type-module-components (field! `foo (field! `bar)))
  (stack `foo `bar))

(check-equal?
  (type-module-components (racket))
  #f)

(check-equal?
  (type-module-components (field! `foo (racket)))
  #f)

; --------------------------------------------------------------------------

(define (type-module-component-symbol-option ($type : Type)) : (Option Symbol)
  (and
    (field? $type)
    (null? (field-structure $type))
    (field-symbol $type)))

(define (structure-module-path-option
  ($structure : Structure)) : (Option Module-Path)
  (option-bind (single $structure) $type
    (and
      (field? $type)
      (equal? (field-symbol $type) `library)
      (option-bind
        (structure-module-components (field-structure $type))
        $symbol-stack
        `(lib
          ,(string-append
            (string-join
              (map symbol->string
                (cons `leo
                  (cons `library
                    (reverse $symbol-stack))))
              "/")
            ".leo"))))))

(check-not
  (structure-module-path-option (structure)))

(check-equal?
  (structure-module-path-option (structure (field! `library)))
  `(lib "leo/library.leo"))

(check-equal?
  (structure-module-path-option (structure (field! `library (field! `foo (field! `bar)))))
  `(lib "leo/library/foo/bar.leo"))

(check-not
  (structure-module-path-option (structure (field! `not-library))))

; -------------------------------------------------------------------------------

(define (structure-resolve-module ($structure : Structure)) : (Option Expressions)
  (option-app module-path-resolve (structure-module-path-option $structure)))

(check-equal?
  (option-app expressions-sexp
    (structure-resolve-module
      (structure (field! `library (field! `testing (field! `module))))))
  (expressions-sexp
    (expressions
      #`(let () (local-require (submod (lib "leo/library/testing/module.leo") unsafe))
        (values tmp-text tmp-number))
      (structure
        text-type
        number-type
        (field! `color (field! `red))))))
