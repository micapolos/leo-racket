#lang racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  (define point (cons 10 20))
  (define (inc n) (+ n 1))
  (define label (string-append (number->string (inc (car point))) " apples!!!")))

(module structure typed/racket/base
  (provide (all-defined-out))
  (require leo/runtime/structure)
  (define $structure
    (structure
      (field! `point (field! `x number-type) (field! `y number-type))
      (field! `green (field! `apple))
      (recipe! number-type (doing number-type))
      text-type
      (field! `label text-type))))

(module syntax typed/racket/base
  (provide (all-defined-out))
  (require leo/runtime/syntax)
  (define $syntax-stack
    (stack
      #`point
      #`()
      #`inc
      #`"inline-text"
      #`label)))

(require leo/runtime/top-level 'unsafe 'structure)

(define $any-stack 
  (stack
    point
    `() 
    inc 
    "inline-text" 
    label))

(for-each
  value-displayln
  (reverse (map value $any-stack $structure)))
