#lang leo/typed

(require
  leo/compiler/program
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/syntax-utils
  leo/compiler/any-sexp
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/binder
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-syntax)

(define leo-writer? : (Parameter Boolean) (make-parameter #f))

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

(define (ingredients-top-level-syntax-stack ($ingredients : Ingredients)) : (Stackof Syntax)
  (define $scoper-stack (ingredients-scoper-stack $ingredients))
  (map make-syntax
    (stack
      (scoper-stack-unsafe-module-syntax $scoper-stack)
      (structure-module-syntax (ingredients-structure $ingredients))
      (scoper-stack-syntax-module-syntax $scoper-stack)
      `(require leo/runtime/top-level 'unsafe 'structure)
      `(define $any-stack (stack ,@(reverse (scoper-stack-identifier-stack $scoper-stack))))
      `(any-stack-structure-displayln ,(if (leo-writer?) ''leo ''racket)
          $any-stack $structure))))

(define (scoper-stack-unsafe-module-syntax ($scoper-stack : (Stackof Scoper))) : Syntax
  (make-syntax
    `(module unsafe racket/base
      (provide (all-defined-out))
      (require leo/runtime/unsafe)
      ,@(reverse
        (scoper-stack-define-syntax-stack $scoper-stack)))))

(define (scoper-stack-syntax-module-syntax ($scoper-stack : (Stackof Scoper))) : Syntax
  (syntax-module-syntax
    (scoper-stack-identifier-stack $scoper-stack)))

(define (scoper-stack-define-syntax-stack ($scoper-stack : (Stackof Scoper))) : (Stackof Syntax)
  (filter-false (map scoper-define-syntax-option $scoper-stack)))

(define (scoper-define-syntax-option ($scoper : Scoper)) : (Option Syntax)
  (option-app scoper-entry-define-syntax (scoper-entry-option $scoper)))

(define (scoper-entry-define-syntax ($entry : Entry)) : Syntax
  (make-syntax
    `(define-values
      ,(reverse (entry-identifier-stack $entry))
      ,(entry-syntax $entry))))

(check-equal?
  (reverse 
    (map syntax->datum
      (ingredients-top-level-syntax-stack
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
     (require leo/runtime/unsafe)
     (define-values (tmp-point) (cons 10 20))
     (define-values (tmp-inc) (lambda (n) (+ n 1)))
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
        (field!
         'point
         (field! 'x (field! 'number (racket)))
         (field! 'y (field! 'number (racket))))
        (field! 'green (field! 'apple))
        (recipe!
         (field! 'number (racket))
         (field! 'inc)
         (does (field! 'number (racket))))
        (field! 'text (racket))
        (field! 'label (field! 'text (racket))))))
   (module syntax typed/racket/base
     (provide (all-defined-out))
     (require leo/runtime/syntax)
     (define $syntax-stack
       (stack #'tmp-point #'tmp-inc #'tmp-text #'tmp-label)))
   (require leo/runtime/top-level 'unsafe 'structure)
   (define $any-stack (stack tmp-point tmp-inc tmp-text tmp-label))
   (any-stack-structure-displayln 'racket $any-stack $structure)))
