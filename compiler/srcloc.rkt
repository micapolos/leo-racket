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

(define (set-srcloc! ($any : Any) ($srcloc : srcloc))
  (hash-set! srcloc-weak-hash $any $srcloc))

(define (any-srcloc ($any : Any)) : (Option srcloc)
  (hash-ref srcloc-weak-hash $any (lambda () #f)))

(bind $obj (gensym)
  (check-equal? (any-srcloc $obj) #f)
  (set-srcloc! $obj srcloc-a)
  (check-equal? (any-srcloc $obj) srcloc-a))