#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/type
  leo/compiler/type-utils)

(define (stack-of-type ($type : Type))
  (recursive
    (field `stack
      (structure
        (choice
          (structure
            (null-field `empty)
            (field `link 
              (structure
                (field `top 
                  (structure $type))
                (field `tail
                  (structure 
                    (variable 0)))))))))))

(define stack-type 
  (generic (stack-of-type (variable 1))))

(define racket-type
  (null-field `racket))

(define field-type 
  (field `field (structure text-type (stack-of-type (variable 1)))))

(define choice-type
  (field `choice (structure (stack-of-type (variable 1)))))

(define arrow-type
  (field `arrow
    (structure
      (field `from (structure (variable 0)))
      (field `to (structure (variable 0))))))

(define a-type
  (field `a (structure (variable 0))))

(define generic-type
  (field `generic (structure (variable 0))))

(define recursive-type
  (field `recursive (structure (variable 0))))

(define variable-type
  (field `variable (structure number-type)))

(define type-type 
  (recursive
    (field `type
      (structure
        (choice
          (structure
            racket-type
            field-type
            arrow-type
            a-type
            generic-type
            recursive-type
            variable-type))))))
