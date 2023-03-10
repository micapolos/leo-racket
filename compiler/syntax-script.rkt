#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/script
  leo/compiler/syntax-utils
  leo/compiler/srcloc)

(define (syntax-line ($syntax : Syntax)) : (Lineof srcloc)
  (define $syntax-e (syntax-e $syntax))
  (define $srcloc (syntax-srcloc $syntax))
  (or
    (and (number? $syntax-e) 
      (line $srcloc $syntax-e))
    (and (string? $syntax-e) 
      (line $srcloc $syntax-e))
    (and (symbol? $syntax-e) 
      (line $srcloc (sentence $syntax-e (script null))))
    (and 
      (not (null? $syntax-e))
      (list? $syntax-e) 
      (symbol? (syntax-e (car $syntax-e))) 
      (line $srcloc
        (sentence 
          (syntax-e (car $syntax-e))
          (syntax-list-script (cdr $syntax-e)))))
    (error 
      (format "syntax-line ~s" $syntax))))

(define (syntax-list-script ($syntax-list : (Listof Syntax))) : (Scriptof srcloc)
  (script (reverse (map syntax-line $syntax-list))))

(check-equal? 
  (line-strip (syntax-line #`1) )
  (line nil 1))

(check-equal? 
  (line-strip (syntax-line #`"foo"))
  (line nil "foo"))

(check-equal? 
  (line-strip (syntax-line #`foo))
  (line nil (sentence `foo null-script)))

(check-equal? 
  (line-strip (syntax-line #`(foo 1 "foo")))
  (line nil 
    (sentence `foo 
      (script 
        (stack 
          (line nil 1)
          (line nil "foo"))))))
