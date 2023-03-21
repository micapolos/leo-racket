#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-syntax)

(define (unsafe-module-syntax ($tuple : Tuple)) : Syntax
  (make-syntax
    `(module unsafe racket/base
      (provide (all-defined-out))
      ,@(reverse
        (map expression-syntax 
          (filter-false
            (filter expression-dynamic? $tuple)))))))

(define (structure-module-syntax ($structure : Structure)) : Syntax
  (make-syntax
    `(module structure typed/racket/base
      (provide (all-defined-out))
      (require leo/runtime/structure)
      (define $structure
        ,(structure-syntax $structure)))))

(define (syntax-module-syntax ($syntax-stack : (Stackof Syntax))) : Syntax
  (make-syntax
    `(module syntax typed/racket/base
      (provide (all-defined-out))
      (require leo/runtime/syntax)
      (define $syntax-stack
        (stack ,@(reverse $syntax-stack))))))

(define (top-level-syntax-stack ($tuple : Tuple)) : (Stackof Syntax)
  (map make-syntax
    (stack
      (unsafe-module-syntax $tuple)
      (structure-module-syntax (tuple-structure $tuple))
      (syntax-module-syntax (map expression-syntax $tuple))
      `(require leo/runtime/top-level 'unsafe 'structure)
      `(define $any-stack (stack ,@(reverse (map expression-syntax $tuple))))
      `(for-each
        value-displayln
        (reverse (map value $any-stack $structure))))))

(check-equal?
  (reverse 
    (map syntax->datum
      (top-level-syntax-stack 
        (tuple
          (expression 
            #`(cons 10 20) 
            (field! `point 
              (field! `x number-type) 
              (field! `y number-type)))
          (expression
            null-syntax
            (field! `green (field! `apple)))
          (expression
            #`(lambda (n) (+ n 1))
            number-type)
          (expression
            #`"inline-text"
            text-type)
          (expression
            #`(string-append (number->string (inc (car point))) " apples!!!")
            (field! `label text-type))))))
  `((module unsafe racket/base
     (provide (all-defined-out))
     (cons 10 20)
     (lambda (n) (+ n 1))
     "inline-text"
     (string-append (number->string (inc (car point))) " apples!!!"))
   (module structure typed/racket/base
     (provide (all-defined-out))
     (require leo/runtime/structure)
     (define $structure
       (structure
        (field 'point
               (structure
                (field 'x (structure (field 'number (structure (racket)))))
                (field 'y (structure (field 'number (structure (racket)))))))
        (field 'green (structure (field 'apple (structure))))
        (field 'number (structure (racket)))
        (field 'text (structure (racket)))
        (field 'label (structure (field 'text (structure (racket))))))))
   (module syntax typed/racket/base
     (provide (all-defined-out))
     (require leo/runtime/syntax)
     (define $syntax-stack
       (stack
        (cons 10 20)
        #f
        (lambda (n) (+ n 1))
        "inline-text"
        (string-append (number->string (inc (car point))) " apples!!!"))))
   (require leo/runtime/top-level 'unsafe 'structure)
   (define $any-stack
     (stack
      (cons 10 20)
      #f
      (lambda (n) (+ n 1))
      "inline-text"
      (string-append (number->string (inc (car point))) " apples!!!")))
   (for-each value-displayln (reverse (map value $any-stack $structure)))))
