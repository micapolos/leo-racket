#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing)

(define empty-srcloc (srcloc #f #f #f #f #f))
(define test-srcloc (srcloc "test.leo" 1 2 3 4))
(define srcloc-a (srcloc "a.leo" 1 2 3 4))
(define srcloc-b (srcloc "b.leo" 1 2 3 4))
(define srcloc-c (srcloc "c.leo" 1 2 3 4))
(define srcloc-d (srcloc "d.leo" 1 2 3 4))

(define srcloc-weak-hash : (Weak-HashTable Any srcloc) (make-weak-hash))

(define #:forall (V) (with-srcloc ($srcloc : srcloc) ($fn : (-> V))) : V
  (define $value ($fn))
  (hash-set! srcloc-weak-hash $value $srcloc)
  $value)

(define (any-srcloc ($any : Any)) : (Option srcloc)
  (hash-ref srcloc-weak-hash $any (lambda () #f)))

(check-equal? (any-srcloc (gensym)) #f)
(check-equal? (any-srcloc (with-srcloc srcloc-a (lambda () (gensym)))) srcloc-a)
