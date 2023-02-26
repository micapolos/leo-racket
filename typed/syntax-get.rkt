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
  leo/typed/testing)

(define (syntax-get ($syntax : Syntax) ($selector : Type)) : (Option Syntax)
  (define $type (syntax-type $syntax))
  (define $ctx $syntax)
  (and
    (tuple? $type)
    (let* (($type-list (tuple-type-list $type))
           ($size (type-list-size $type-list))
           ($indexed (type-list-select $type-list $selector)))
      (and $indexed
        (let (($index (car $indexed))
              ($type (cdr $indexed)))
        (syntax-with-type
          (if (not $index)
            #`#f
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
        (tuple `foo (list number-type)))
      (tuple `number null))
    syntax-typed-datum)
  (typed `foo number-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (tuple `foo (list number-type string-type)))
      (tuple `string null))
    syntax-typed-datum)
  (typed `(unsafe-cdr foo) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(cons a b)
        (tuple `foo (list number-type string-type)))
      (tuple `string null))
    syntax-typed-datum)
  (typed `b string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (tuple `foo (list number-type string-type boolean-type)))
      (tuple `string null))
    syntax-typed-datum)
  (typed `(unsafe-vector-ref foo 1) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(vector a b c)
        (tuple `foo (list number-type string-type boolean-type)))
      (tuple `string null))
    syntax-typed-datum)
  (typed `b string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (tuple `foo (list number-type (tuple `foo null) string-type)))
      (tuple `foo null))
    syntax-typed-datum)
  (typed #f (tuple `foo null)))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (tuple `foo (list number-type (tuple `foo null) string-type)))
      (tuple `string null))
    syntax-typed-datum)
  (typed `(unsafe-cdr foo) string-type))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`foo
        (tuple `point 
          (list
            (tuple `x (list number-type))
            (tuple `y (list number-type)))))
      (tuple `y null))
    syntax-typed-datum)
  (typed `(unsafe-cdr foo) (tuple `y (list number-type))))

(check-equal?
  (option-map
    (syntax-get
      (syntax-with-type 
        #`(cons a b)
        (tuple `foo (list number-type string-type)))
      (tuple `first null))
    syntax-typed-datum)
  (typed `a (tuple `first (list number-type))))
