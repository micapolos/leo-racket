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

(define (program-top-level-syntax-stack ($program : Program)) : (Stackof Syntax)
  (define $entry-stack (program-entry-stack $program))
  (define $ingredients (program-resolved-ingredients $program))
  (define $binder-stack (usage-ingredients-binder-stack 'indirect $ingredients))
  (map make-syntax
    (stack
      (binder-stack-unsafe-module-syntax $entry-stack $binder-stack)
      (structure-module-syntax (ingredients-structure $ingredients))
      (binder-stack-syntax-module-syntax $binder-stack)
      `(require leo/runtime/top-level 'unsafe 'structure)
      `(define $any-stack (stack ,@(reverse (binder-stack-syntax-stack $binder-stack))))
      `(for-each
        (curry value-displayln ,(if (leo-writer?) ''leo ''racket))
        (reverse (map value $any-stack $structure))))))

(define (binder-stack-unsafe-module-syntax ($entry-stack : (Stackof Entry)) ($binder-stack : (Stackof Binder))) : Syntax
  (make-syntax
    `(module unsafe racket/base
      ,(binder-stack-provide-syntax $binder-stack)
      (require leo/runtime/unsafe)
      ,@(reverse
        (binder-stack-define-syntax-stack $binder-stack)))))

(define (binder-stack-provide-syntax ($binder-stack : (Stackof Binder))) : Syntax
  (make-syntax
    `(provide
      ,@(reverse
        (apply append
          (map entry-identifier-stack
            (filter-false
              (map binder-entry-option $binder-stack))))))))

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
      (program-top-level-syntax-stack
        (program
          null ; entry-stack
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
                (field! `label text-type))))))))
  `((module unsafe racket/base
     (provide tmp-point tmp-inc tmp-text tmp-label)
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
       (stack #'tmp-point #'null #'tmp-inc #'tmp-text #'tmp-label)))
   (require leo/runtime/top-level 'unsafe 'structure)
   (define $any-stack (stack tmp-point null tmp-inc tmp-text tmp-label))
   (for-each
    (curry value-displayln 'racket)
    (reverse (map value $any-stack $structure)))))
