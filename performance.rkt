#lang racket

(struct foo (a b))

(define iterations 100000000)

(time (for ([i (in-range iterations)]) (foo 10 20)))
(time (for ([i (in-range iterations)]) (vector-immutable 10 20)))
(time (for ([i (in-range iterations)]) (vector 10 20)))
(time (for ([i (in-range iterations)]) (cons 10 20)))
(time (for ([i (in-range iterations)]) (mcons 10 20)))

(define my-foo (foo 10 20))
(define my-vector-immutable (vector-immutable 10 20))
(define my-vector (vector 10 20))
(define my-pair (cons 10 20))
(define my-mpair (mcons 10 20))

(time (for ([i (in-range iterations)]) (foo-b my-foo)))
(time (for ([i (in-range iterations)]) (vector-ref my-vector-immutable 1)))
(time (for ([i (in-range iterations)]) (vector-ref my-vector 1)))
(time (for ([i (in-range iterations)]) (cdr my-pair)))
(time (for ([i (in-range iterations)]) (mcdr my-mpair)))
