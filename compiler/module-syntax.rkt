#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/expressions
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-syntax)

(define (unsafe-module-syntax ($scope : Scope)) : Syntax
  (make-syntax
    `(module unsafe racket/base
      (provide (all-defined-out))
      ,@(reverse
        (map binding-define-syntax $scope)))))

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
        (stack 
          ,@(reverse 
            (map 
              (lambda (($syntax : Syntax))
                `(syntax ,$syntax))
              $syntax-stack)))))))

(define (top-level-syntax-stack ($tuple : Tuple)) : (Stackof Syntax)
  (define $structure (tuple-structure $tuple))
  (define $binding-option-stack (tuple-binding-option-stack $tuple))
  (define $scope (filter-false $binding-option-stack))
  (define $bound-tuple (binding-option-stack-tuple-bound-tuple $binding-option-stack $tuple))
  (map make-syntax
    (stack
      (unsafe-module-syntax $scope)
      (structure-module-syntax (tuple-structure $tuple))
      (syntax-module-syntax (map expression-syntax $bound-tuple))
      `(require leo/runtime/top-level 'unsafe 'structure)
      `(define $any-stack (stack ,@(reverse (map expression-syntax $bound-tuple))))
      `(for-each
        value-displayln
        (reverse (map value $any-stack $structure))))))

(define (binding-define-syntax ($binding : Binding)) : Syntax
  (make-syntax 
    `(define 
      ,(binding-identifier $binding)
      ,(binding-syntax $binding))))

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
     (define tmp-point (cons 10 20))
     (define tmp-number (lambda (n) (+ n 1)))
     (define tmp-label
       (string-append (number->string (inc (car point))) " apples!!!")))
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
       (stack #'tmp-point #'#f #'tmp-number #'"inline-text" #'tmp-label)))
   (require leo/runtime/top-level 'unsafe 'structure)
   (define $any-stack (stack tmp-point #f tmp-number "inline-text" tmp-label))
   (for-each value-displayln (reverse (map value $any-stack $structure)))))
