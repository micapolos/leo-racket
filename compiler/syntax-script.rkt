#lang leo/typed

(require
  leo/compiler/script
  leo/compiler/syntax-utils)

(define (syntax-line ($syntax : Syntax)) : (Lineof srcloc)
  (define $syntax-e (syntax-e $syntax))
  (define $srcloc (syntax-srcloc $syntax))
  (or
    (and (number? $syntax-e) 
      (line $srcloc $syntax-e))
    (and (string? $syntax-e) 
      (line $srcloc $syntax-e))
    (and (symbol? $syntax-e) 
      (line $srcloc (sentence (word $srcloc $syntax-e) (script null))))
    (and 
      (not (null? $syntax-e))
      (list? $syntax-e) 
      (symbol? (syntax-e (car $syntax-e))) 
      (line $srcloc
        (sentence 
          (word 
            (syntax-srcloc (car $syntax-e))
            (syntax-e (car $syntax-e)))
          (syntax-list-script (cdr $syntax-e)))))
    (error 
      (format "syntax-line ~s" $syntax))))

(define (syntax-list-script ($syntax-list : (Listof Syntax))) : (Scriptof srcloc)
  (script (reverse (map syntax-line $syntax-list))))

(check-equal? 
  (syntax-line (make-syntax 1 srcloc-a))
  (line srcloc-a 1))

(check-equal? 
  (syntax-line (make-syntax "foo" srcloc-a))
  (line srcloc-a "foo"))

(check-equal? 
  (syntax-line (make-syntax `foo srcloc-a))
  (line srcloc-a (sentence (word srcloc-a `foo) null-script)))

(check-equal? 
  (syntax-line 
    (make-syntax 
      `(,(make-syntax `foo srcloc-b)
        ,(make-syntax 1 srcloc-c) 
        ,(make-syntax "foo" srcloc-d))
      srcloc-a))
  (line srcloc-a
    (sentence (word srcloc-b `foo)
      (script 
        (stack 
          (line srcloc-c 1)
          (line srcloc-d "foo"))))))
