#lang racket/base

(require racket/unsafe/ops)

(define ns (make-base-namespace))
(parameterize
  ((current-namespace ns))
  
  (namespace-set-variable-value! 
    `plus 
    (dynamic-require `racket/unsafe/ops `unsafe-fx+)
    #f 
    (current-namespace) 
    #t)
  
  (namespace-set-variable-value! 
    `times 
    (dynamic-require `racket/unsafe/ops `unsafe-fx*)
    #f 
    (current-namespace) 
    #t)
  
  (namespace-set-variable-value! 
    `fib 
    (dynamic-require `leo/fibonacci `fibonacci)
    #f 
    (current-namespace) 
    #t)
  
  (eval `(time (fib (plus 12 (times 10 (plus 1 2)))))))
