#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions-binder
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

(define (ingredients-syntax-stack ($ingredients : Ingredients)) : (Stackof Syntax)
  (define $binder-stack (map (curry single-use-expressions-binder #f) $ingredients))
  (map make-syntax
    (stack
      (binder-stack-unsafe-module-syntax $binder-stack)
      (structure-module-syntax (ingredients-structure $ingredients))
      (binder-stack-syntax-module-syntax $binder-stack)
      `(require leo/runtime/top-level 'unsafe 'structure)
      `(define $any-stack (stack ,@(reverse (binder-stack-syntax-stack $binder-stack))))
      `(for-each
        value-displayln
        (reverse (map value $any-stack $structure))))))

(define (binding-define-syntax ($binding : Binding)) : Syntax
  (make-syntax 
    `(define 
      ,(binding-identifier $binding)
      ,(binding-syntax $binding))))

(define (binder-stack-unsafe-module-syntax ($binder-stack : (Stackof Binder))) : Syntax
  (make-syntax
    `(module unsafe racket/base
      (provide (all-defined-out))
      ,@(reverse
        (binder-stack-define-syntax-stack $binder-stack)))))

(define (binder-stack-syntax-module-syntax ($binder-stack : (Stackof Binder))) : Syntax
  (syntax-module-syntax
    (binder-stack-syntax-stack $binder-stack)))

(define (binder-stack-define-syntax-stack ($binder-stack : (Stackof Binder))) : (Stackof Syntax)
  (filter-false (map binder-define-syntax-option $binder-stack)))

(define (binder-define-syntax-option ($binder : Binder)) : (Option Syntax)
  (option-app binder-entry-define-syntax (binder-entry-option $binder)))

(define (binder-entry-define-syntax ($entry : Entry)) : Syntax
  (make-syntax
    `(define-values
      ,(reverse (entry-identifier-stack $entry))
      ,(entry-syntax $entry))))

(check-equal?
  (reverse 
    (map syntax->datum
      (ingredients-syntax-stack
        (ingredients
          (expressions
            #`(cons 10 20)
            (structure
              (field! `point
                (field! `x number-type)
                (field! `y number-type))))
          (expressions
            null-syntax
            (structure (field! `green (field! `apple))))
          (expressions
            #`(lambda (n) (+ n 1))
            (structure
              (recipe!
                number-type
                (field! `inc)
                (does number-type))))
          (expressions
            #`(values
              "inline-text"
              (string-append (number->string (tmp-recipe (car tmp-point))) " apples!!!"))
            (structure
              text-type
              (field! `label text-type)))))))
  `((module unsafe racket/base
     (provide (all-defined-out))
     (define-values (tmp-point) (cons 10 20))
     (define-values (tmp-recipe) (lambda (n) (+ n 1)))
     (define-values
      (tmp-text tmp-label)
      (values
       "inline-text"
       (string-append
        (number->string (tmp-recipe (car tmp-point)))
        " apples!!!"))))
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
        (arrow
         (structure
          (field 'number (structure (racket)))
          (field 'inc (structure)))
         (structure (field 'number (structure (racket)))))
        (field 'text (structure (racket)))
        (field 'label (structure (field 'text (structure (racket))))))))
   (module syntax typed/racket/base
     (provide (all-defined-out))
     (require leo/runtime/syntax)
     (define $syntax-stack
       (stack #'tmp-point #'null #'tmp-recipe #'tmp-text #'tmp-label)))
   (require leo/runtime/top-level 'unsafe 'structure)
   (define $any-stack (stack tmp-point null tmp-recipe tmp-text tmp-label))
   (for-each value-displayln (reverse (map value $any-stack $structure)))))
