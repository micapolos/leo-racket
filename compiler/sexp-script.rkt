#lang leo/typed

(require
  leo/compiler/script)

(define (sexp-line ($sexp : Sexp)) : Line
  (or
    (and (number? $sexp) 
      (line nil $sexp))
    (and (string? $sexp) 
      (line nil $sexp))
    (and (symbol? $sexp) 
      (line nil 
        (sentence 
          (word nil $sexp) 
          (script null))))
    (and (pair? $sexp) (symbol? (car $sexp)) (list? (cdr $sexp))
      (line nil 
        (sentence 
          (word nil (car $sexp))
          (sexp-list-script (cdr $sexp)))))
    (error 
      (format "sexp-line ~s" $sexp))))

(define (sexp-list-script ($sexp-list : (Listof Sexp))) : Script
  (script (reverse (map sexp-line $sexp-list))))

(check-equal? (sexp-line 1) (line nil 1))

(check-equal? (sexp-line "foo") (line nil "foo"))

(check-equal? 
  (sexp-line `foo) 
  (line nil (sentence (word nil `foo) null-script)))

(check-equal? 
  (sexp-line `(foo 1 "foo")) 
  (line nil 
    (sentence (word nil `foo)
      (script 
        (stack 
          (line nil 1)
          (line nil "foo"))))))
