#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/decompiler
  leo/typed/testing
  leo/typed/type
  leo/typed/typed-syntax
  leo/typed/type-parse
  leo/typed/types
  leo/typed/any-leo-string
  leo/typed/syntax-type
  leo/typed/type-any
  leo/typed/syntax-match
  leo/typed/compiled)

(define (syntax-top-level ($syntax : Syntax)) : Syntax
  (define $type (syntax-type-option $syntax))
  (cond
    ((and $type (not (equal? void-type $type)))
      (datum->syntax #f 
        `(display
          (any-leo-string 
            (any-type-decompile
              ,$syntax
              (any-parse-type 
                (quote 
                  ,(cast-syntax 
                    (datum->syntax #f 
                      (type-any $type))))))))))
    (else $syntax)))

(check-equal?
  (syntax->datum (syntax-top-level (number-typed-syntax 1)))
  '(display (any-leo-string (any-type-decompile 1 (any-parse-type 'number)))))
