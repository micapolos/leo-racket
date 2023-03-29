#lang leo/typed

(define (any-sexp ($any : Any)) : Sexp
  (cond
    ((null? $any) $any)
    ((symbol? $any) $any)
    ((number? $any) $any)
    ((string? $any) $any)
    ((boolean? $any) $any)
    ((pair? $any) (cons (any-sexp (car $any)) (any-sexp (cdr $any))))
    (else `(native ,(format "~a" $any)))))
