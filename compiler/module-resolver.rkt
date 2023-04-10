#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/expressions-utils
  leo/compiler/ingredients
  leo/compiler/ingredients-sexp
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/any-sexp)

(require/typed racket/base
  (exn:missing-module? (-> Any Boolean)))

(define (module-path-syntax ($module-path : Module-Path)) : Syntax
  (any-syntax
    (datum->syntax #f $module-path)))

(define (module-path-scope-option ($module-path : Module-Path)) : (Option Scope)
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
        (define $identifier-stack
          (dynamic-require
            `(submod ,$module-path syntax)
            `$syntax-stack
            (lambda () #f)))
        (and $structure $identifier-stack
          (identifier-stack-structure-scope
            (any-identifier-stack $identifier-stack)
            (any-structure $structure)))))))

(define (any-syntax-stack ($any : Any)) : (Stackof Syntax)
  (unless (list? $any) (error "not a list"))
  (map any-syntax $any))

(define (any-structure ($any : Any)) : Structure
  (unless (list? $any) (error "not a list"))
  (map (lambda (($item : Any)) (cast $item Type)) $any))

(define (any-identifier-stack ($any : Any)) : (Stackof Identifier)
  (unless (list? $any) (error "not a list"))
  (map any-identifier $any))

; -------------------------------------------------------------------

(define (module-path-tuple-ingredients ($module-path : Module-Path) ($scope : Scope)) : Ingredients
  (map
    (lambda (($binding : Binding))
      (expression-expressions
        (expression
          (option-bind (binding-identifier-option $binding) $identifier
            (make-syntax
              `(local (submod ,(module-path-syntax $module-path) unsafe) ,$identifier)))
          (binding-type $binding))))
    $scope))

(check-equal?
  (ingredients-sexp
    (module-path-tuple-ingredients
      `leo/module
      (scope
        (binding
          #`point
          (field! `point
            (field! `x number-type)
            (field! `y number-type)))
        (binding
          #f
          (field! `green (field! `apple)))
        (binding
          #`inc
          (recipe!
            number-type
            (field! `inc)
            (does number-type)))
        (binding
          #`label
          (field! `label text-type)))))
  (ingredients-sexp
    (ingredients
      (expressions
        #`(local (submod leo/module unsafe) point)
        (structure (field! `point (field! `x number-type) (field! `y number-type))))
      (expressions
        #f
        (structure (field! `green (field! `apple))))
      (expressions
        #`(local (submod leo/module unsafe) inc)
        (structure (recipe! number-type (field! `inc) (doing number-type))))
      (expressions
        #`(local (submod leo/module unsafe) label)
        (structure (field! `label text-type))))))

; ------------------------------------------------------------------

(define (module-path-resolve ($module-path : Module-Path)) : (Option Ingredients)
  (option-bind (module-path-scope-option $module-path) $scope
    (module-path-tuple-ingredients $module-path $scope)))

; --------------------------------------------------------------------------

(define (symbol-stack-plus-syntax-module-components
  ($symbol-stack : (Stackof Symbol))
  ($syntax : Syntax))
: (Option (Stackof Symbol))
  (define $e (syntax-e $syntax))
  (cond
    ((symbol? $e) (push $symbol-stack $e))
    (else
      (syntax-match-symbol-args $syntax $symbol $args
        (symbol-stack-plus-syntax-list-module-components
          (push $symbol-stack $symbol)
          $args)))))

(define (symbol-stack-plus-syntax-list-module-components
  ($symbol-stack : (Stackof Symbol))
  ($syntax-list : (Listof Syntax))) : (Option (Stackof Symbol))
  (cond
    ((null? $syntax-list) $symbol-stack)
    (else
      (option-bind (single $syntax-list) $syntax
        (symbol-stack-plus-syntax-module-components $symbol-stack $syntax)))))

(define (syntax-module-components ($syntax : Syntax)) : (Option (Stackof Symbol))
  (symbol-stack-plus-syntax-module-components null $syntax))

(define (syntax-list-module-components ($syntax-list : (Listof Syntax))) : (Option (Stackof Symbol))
  (symbol-stack-plus-syntax-list-module-components null $syntax-list))

(check-equal?
  (syntax-module-components #`foo)
  (stack `foo))

(check-equal?
  (syntax-module-components #`(foo))
  (stack `foo))

(check-equal?
  (syntax-module-components #`(foo bar))
  (stack `foo `bar))

(check-equal?
  (syntax-module-components #`123)
  #f)

(check-equal?
  (syntax-module-components #`(foo 123))
  #f)

; --------------------------------------------------------------------------

(define (syntax-module-path-option
  ($syntax : Syntax)) : (Option Module-Path)
  (syntax-symbol-match-args $syntax `package $args
    (option-bind
      (syntax-list-module-components $args)
      $symbol-stack
      `(lib
        ,(string-append
          (string-join
            (map symbol->string
              (cons `leo
                (cons `package
                  (reverse $symbol-stack))))
            "/")
          ".leo")))))

(check-equal?
  (syntax-module-path-option
    #`(package (foo bar)))
  `(lib "leo/package/foo/bar.leo"))

(check-equal?
  (syntax-module-path-option
    #`(package 123))
  #f)

(check-equal?
  (syntax-module-path-option
    #`(not-package (foo bar)))
  #f)

; -------------------------------------------------------------------------------

(define (syntax-resolve-module ($syntax : Syntax)) : (Option Ingredients)
  (option-app module-path-resolve (syntax-module-path-option $syntax)))

(check-equal?
  (option-app ingredients-sexp
    (syntax-resolve-module
      #`(package (testing module))))
  (ingredients-sexp
    (ingredients
      (expressions
        #`(local (submod (lib "leo/package/testing/module.leo") unsafe) tmp-text)
        (structure text-type))
      (expressions
        #`(local (submod (lib "leo/package/testing/module.leo") unsafe) tmp-number)
        (structure number-type))
      (expressions
        #f
        (structure (field! `color (field! `red)))))))
