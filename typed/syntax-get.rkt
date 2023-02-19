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
  leo/typed/syntax-match
  leo/testing)

(define (syntax-get ($syntax : Syntax) ($selector : Type)) : (Option Syntax)
  (define $type (syntax-type $syntax))
  (define $ctx $syntax)
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
                  (cond
                    ((syntax-symbol-arg-arg? $syntax `cons)
                      (if (= $index 0) 
                        (cadr (syntax-e $syntax))
                        (caddr (syntax-e $syntax))))
                    (else 
                      (list 
                        (if (= $index 0) #`unsafe-car #`unsafe-cdr)
                        $syntax))))
                (else 
                  (cond 
                    ((syntax-symbol-args? $syntax `vector)
                      (list-ref (syntax-e $syntax) (+ $index 1)))
                    (else (list #`unsafe-vector-ref $syntax $index)))))))
          $type))))))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list fixnum-type))))
      (symbol-type `fixnum))
    syntax-typed-datum)
  (typed `foo fixnum-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type string-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(unsafe-cdr foo) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(cons a b)
        (field-type `foo (struct-type-body (list number-type string-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `b string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type string-type boolean-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(unsafe-vector-ref foo 1) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(vector a b c)
        (field-type `foo (struct-type-body (list number-type string-type boolean-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `b string-type))

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
  (typed `(unsafe-cdr foo) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (field-type `point 
          (struct-type-body 
            (list 
              (field-type `x (struct-type-body (list number-type)))
              (field-type `y (struct-type-body (list number-type)))))))
      (symbol-type `y))
    syntax-typed-datum)
  (typed `(unsafe-cdr foo) (field-type `y (struct-type-body (list number-type)))))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(cons a b)
        (field-type `foo (struct-type-body (list number-type string-type))))
      (symbol-type `first))
    syntax-typed-datum)
  (typed `a (field-type `first (struct-type-body (list number-type)))))
