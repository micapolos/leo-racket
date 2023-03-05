#lang typed/racket/base

(provide (all-defined-out))

(require
  (for-syntax racket/base))

(define-syntax (leo $syntax)
  (syntax-case $syntax ()
    ((_ body ...) 
      #`(begin body ...))))
