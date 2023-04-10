#lang leo/typed

(define (any-sexp ($any : Any)) : Sexp
  (cond
    ((null? $any) $any)
    ((symbol? $any) $any)
    ((char? $any) $any)
    ((number? $any) $any)
    ((string? $any) $any)
    ((boolean? $any) $any)
    ((pair? $any) (cons (any-sexp (car $any)) (any-sexp (cdr $any))))
    (else `(any ,(format "~a" $any)))))

(check-equal? (any-sexp null) null)
(check-equal? (any-sexp `foo) `foo)
(check-equal? (any-sexp #\a) #\a)
(check-equal? (any-sexp 123) 123)
(check-equal? (any-sexp "foo") "foo")
(check-equal? (any-sexp #t) #t)
(check-equal? (any-sexp (cons `foo `bar)) (cons `foo `bar))
