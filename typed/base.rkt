#lang typed/racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    racket/string))

(define-syntax (data syntax)
  (syntax-case syntax ()
    ((_ name fields ...)
      #`(struct name (fields ...) 
        #:transparent 
        #:type-name 
        #,(datum->syntax syntax 
          (string->symbol
            (string-join 
              (map string-titlecase 
                (string-split (symbol->string (syntax->datum #`name)) "-"))
              "")))))))
