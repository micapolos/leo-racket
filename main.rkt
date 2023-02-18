#lang racket/base
 
(provide 
  (except-out
    (all-from-out racket/base)
    #%module-begin)
  (rename-out 
    (module-begin #%module-begin)))

(require
  (for-syntax racket/base))
 
(define-syntax (module-begin $syntax)
  (syntax-case $syntax ()
    ((_ expr ...)
      #`(#%module-begin 
        (provide (all-defined-out))
        expr ...))))
