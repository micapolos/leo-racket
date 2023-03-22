#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/stack
  leo/typed/syntax-match
  leo/compiler/expression
  leo/compiler/type)

(define (module-symbol-tuple-option ($symbol : Symbol)) : (Option Tuple)
  (define $structure
    (dynamic-require
      `(submod ,$symbol structure)
      `$structure
      (lambda () #f)))
  (define $syntax-stack
    (dynamic-require
      `(submod ,$symbol syntax)
      `$syntax-stack
      (lambda () #f)))
  (and $structure $syntax-stack
    (map
      expression
      (any-syntax-stack $syntax-stack)
      (any-structure $structure))))

(define (any-syntax-stack ($any : Any)) : (Stackof Syntax)
  (unless (list? $any) (error "not a list"))
  (map any-syntax $any))

(define (any-structure ($any : Any)) : Structure
  (unless (list? $any) (error "not a list"))
  (map (lambda (($item : Any)) (cast $item Type)) $any))
