#lang racket/base
 
(provide 
  (except-out
    (all-from-out racket/base)
    #%module-begin)
  (rename-out 
    (module-begin #%module-begin)))

(require
  (for-syntax 
    racket/base
    racket/function
    syntax/strip-context
    leo/typed/compile))
 
(define-syntax (module-begin $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      (let* (($syntaxes (map (curry replace-context $syntax) (syntax-e #`(body ...)))))
        #`(#%module-begin 
          (provide (all-defined-out))
          #,@$syntaxes)))))
