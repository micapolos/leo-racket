#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  (for-syntax racket/base))

(define-for-syntax (syntax-list->infix $syntax-list)
  (foldl
    (lambda ($syntax $infix)
      (let (($e (syntax-e $syntax)))
        (cond
          ((null? $e) (error "null syntax"))
          ((list? $e)
            (let* (($car (syntax-e (car $e)))
                   ($cdr (cdr $e)))
              (unless (symbol? $car) (error "syntax error not a symbol"))
              (let (($infix-cdr (map syntax->infix $cdr)))
                (if $infix
                  #`(#,$car #,$infix #,@$infix-cdr)
                  #`(#,$car #,@$infix-cdr)))))
          ((symbol? $e)
            (if $infix
              #`(#,$syntax #,$infix)
              $syntax))
          (else
            (when $infix (error "syntax error not a symbol"))
            $syntax))))
    #f
    $syntax-list))

(define-for-syntax (syntax->infix $syntax)
  (let (($e (syntax-e $syntax)))
    (cond
      ((null? $e) $syntax)
      ((list? $e) (syntax-list->infix $e))
      (else $syntax))))

(define-syntax (infix $syntax)
  (syntax-case $syntax ()
    ((_ $line ...)
      (syntax-list->infix (syntax-e #`($line ...))))))

(check-equal? (infix null) null)
(check-equal? (infix 123) 123)
(check-equal? (infix "foo") "foo")
(check-equal? (infix #\a) #\a)
(check-equal? (infix `()) `())
(check-equal? (infix "foo" string-length) 3)
(check-equal? (infix "foo" (string-length)) 3)
(check-equal? (infix "foo" (string-append "bar")) "foobar")
(check-equal? (infix "foo" (string-append "bar" "goo")) "foobargoo")
(check-equal? (infix "foo" (string-append ("bar" (string-append "goo")))) "foobargoo")
