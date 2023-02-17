#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/syntax-type
  leo/typed/type
  leo/typed/types
  leo/typed/type-select
  leo/typed/type-utils
  leo/typed/typed
  leo/typed/syntax-typed
  leo/testing)

(define (syntax-get ($syntax : Syntax) ($selector : Type)) : (Option Syntax)
  (define $type (syntax-type $syntax))
  (and
    (field-type? $type)
    (struct-type-body? (field-type-body $type))
    (let* (($struct-type-body (field-type-body $type))
           ($size (struct-type-body-size $struct-type-body))
           ($indexed (struct-type-body-select $struct-type-body $selector)))
      (and $indexed
        (let (($index (car $indexed))
              ($type (cdr $indexed)))
        (syntax-with-type
          (if (not $index)
            #`()
            (datum->syntax #f 
              (case $size
                ((0) (error "impossible"))
                ((1) $syntax)
                ((2) 
                  (list 
                    (if (= $index 0) `car `cdr)
                    $syntax))
                (else 
                  (list `vector-ref $syntax $index)))))
          $type))))))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type))))
      (symbol-type `number))
    syntax-typed-datum)
  (typed `foo number-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type string-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(cdr foo) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type string-type boolean-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(vector-ref foo 1) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type (symbol-type `foo) string-type))))
      (symbol-type `foo))
    syntax-typed-datum)
  (typed `() (symbol-type `foo)))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type (symbol-type `foo) string-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(cdr foo) string-type))
