#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/syntax-type
  leo/typed/type
  leo/typed/types
  leo/typed/type-get
  leo/typed/typed
  leo/typed/syntax-typed
  leo/testing)

(define (syntax-get-option ($syntax : Syntax) ($selector : Type)) : (Option Syntax)
  (define $syntax-type (syntax-type $syntax))
  (define $indexed (type-get-indexed $syntax-type $selector))
  (cond
    ((equal? $indexed #f) #f)
    (else 
      (define $index (car $indexed))
      (define $type (cdr $indexed))
      (cond
        ((equal? $index #f) (syntax-with-type $syntax $type))
        (else 
          (syntax-with-type 
            (datum->syntax #f (list `vector-ref $syntax $index))
            $type))))))

(check-equal?
  (option-map
    (syntax-get-option
      (syntax-with-type 
        #`foo
        (field-type `foo (struct-type-body (list number-type string-type))))
      (symbol-type `string))
    syntax-typed-datum)
  (typed `(vector-ref foo 1) string-type))
