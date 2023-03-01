#lang typed/racket/base

(provide (all-defined-out))

(define empty-srcloc (srcloc #f #f #f #f #f))
(define test-srcloc (srcloc "test.leo" 1 2 3 4))
(define srcloc-a (srcloc "a.leo" 1 2 3 4))
(define srcloc-b (srcloc "b.leo" 1 2 3 4))
(define srcloc-c (srcloc "c.leo" 1 2 3 4))
(define srcloc-d (srcloc "d.leo" 1 2 3 4))

(define srcloc-weak-hash make-weak-hash)
