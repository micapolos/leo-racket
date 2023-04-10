#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-symbol
  leo/compiler/syntax-utils)

(define (type-tmp-symbol ($type : Type)) : Symbol
  (bind $type-symbol (type-symbol $type)
    (or
      (and (arrow? $type) (option-app type-symbol (top-option (arrow-from-structure $type))))
      $type-symbol)))

(define (symbol-temporary ($symbol : Symbol)) : Identifier
  (cond
    ((testing?)
      (datum->syntax #f 
        (string->symbol (string-append "tmp-" (symbol->string $symbol)))))
    (else (car (generate-temporaries (list $symbol))))))

(define (type-generate-temporary ($type : Type)) : Identifier
  (symbol-temporary (type-tmp-symbol $type)))

(define (type-generate-temporary-symbol ($type : Type)) : Symbol
  (syntax-e (type-generate-temporary $type)))

(check-equal?
  (string-prefix?
    (symbol->string (syntax-e (parameterize ((testing? #f)) (type-generate-temporary type-a))))
    (symbol->string (type-symbol type-a)))
  #t)

(check-equal?
  (syntax->datum (type-generate-temporary type-a))
  `tmp-a)

(check-equal?
  (syntax->datum (type-generate-temporary (recipe! type-a type-b (doing type-c type-d))))
  `tmp-b)

(define (tmp-syntax-a) (type-generate-temporary dynamic-type-a))
(define (tmp-syntax-b) (type-generate-temporary dynamic-type-b))
(define (tmp-syntax-c) (type-generate-temporary dynamic-type-c))
(define (tmp-syntax-d) (type-generate-temporary dynamic-type-d))

; -------------------------------------------------------------------------

(define (expression-generate-temporary-option ($expression : Expression)) : (Option Identifier)
  (and
    (type-dynamic? (expression-type $expression))
    (type-generate-temporary (expression-type $expression))))

(define (type-generate-temporary-option ($type : Type)) : (Option Identifier)
  (and 
    (type-dynamic? $type)
    (type-generate-temporary $type)))

(define (type-generate-binding ($type : Type)) : Binding
  (binding (type-generate-temporary-option $type) $type))

(define (structure-generate-scope ($structure : Structure)) : Scope
  (map type-generate-binding $structure))

(define (type-generate-expression ($type : Type)) : Expression
  (define $tmp-option (type-generate-temporary-option $type))
  (expression $tmp-option $type))

(define (structure-generate-tuple ($structure : Structure)) : Tuple
  (map type-generate-expression $structure))

; -------------------------------------------------------------------------

(define (structure-temporary-stack ($structure : Structure)) : (Stackof Identifier)
  (filter-false (map type-generate-temporary-option $structure)))
