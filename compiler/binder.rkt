#lang leo/typed

(require
  leo/compiler/generate-temporary
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/ingredients
  leo/compiler/sexp-utils)

(define-type Usage (U 'direct 'indirect))

(data entry
  (identifier-stack : (Stackof Identifier))
  (syntax : Syntax))

(data binder
  (entry-option : (Option Entry))
  (tuple : Tuple))

(define (entry-sexp ($entry : Entry)) : Sexp
  `(entry
    (identifiers ,@(reverse (map syntax->datum (entry-identifier-stack $entry))))
    (syntax ,(syntax->datum (entry-syntax $entry)))))

(define (binder-sexp ($binder : Binder)) : Sexp
  `(binder
    ,(option-app entry-sexp (binder-entry-option $binder))
    ,(tuple-sexp (binder-tuple $binder))))

(define (usage-ingredients-binder-stack
  ($usage : Usage)
  ($ingredients : Ingredients))
: (Stackof Binder)
  (map (curry usage-expressions-binder $usage) $ingredients))

(define (usage-expressions-binder
  ($usage : Usage)
  ($expressions : Expressions)) : Binder
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $type-option (single $structure))
  (cond
    ((not (structure-dynamic? $structure))
      (binder #f
        (map (curry expression null-syntax) $structure)))
    ((and (equal? $usage `direct) $type-option)
      (binder #f 
        (tuple (expression $syntax $type-option))))
    (else
      (define $tmp-option-stack (map type-generate-temporary-option $structure))
      (define $identifier-stack (filter-false $tmp-option-stack))
      (define $entry (entry $identifier-stack $syntax))
      (define $tuple
        (map 
          (lambda (($tmp-option : (Option Identifier)) ($type : Type)) : Expression
            (expression 
              (or $tmp-option null-syntax)
              $type))
          $tmp-option-stack
          $structure))
      (binder $entry $tuple))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `direct
      (expressions null-syntax 
        (structure static-type-a static-type-b))))
  (binder-sexp
    (binder #f 
      (tuple 
        static-expression-a 
        static-expression-b))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `indirect
      (expressions null-syntax
        (structure static-type-a static-type-b))))
  (binder-sexp
    (binder #f
      (tuple
        static-expression-a
        static-expression-b))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `direct
      (expressions syntax-a
        (structure dynamic-type-a))))
  (binder-sexp
    (binder
      #f
      (tuple (expression syntax-a dynamic-type-a)))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `indirect
      (expressions syntax-a
        (structure dynamic-type-a))))
  (binder-sexp
    (binder
      (entry (stack (tmp-syntax-a)) syntax-a)
      (tuple (expression (tmp-syntax-a) dynamic-type-a)))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `direct
      (expressions syntax-a
        (structure dynamic-type-a dynamic-type-b))))
  (binder-sexp
    (binder
      (entry (stack (tmp-syntax-a) (tmp-syntax-b)) syntax-a)
      (tuple
        (expression (tmp-syntax-a) dynamic-type-a)
        (expression (tmp-syntax-b) dynamic-type-b)))))

(check-equal?
  (binder-sexp
    (usage-expressions-binder `indirect
      (expressions syntax-a
        (structure dynamic-type-a dynamic-type-b))))
  (binder-sexp
    (binder
      (entry (stack (tmp-syntax-a) (tmp-syntax-b)) syntax-a)
      (tuple
        (expression (tmp-syntax-a) dynamic-type-a)
        (expression (tmp-syntax-b) dynamic-type-b)))))

(define (entry-let-syntax ($entry : Entry)) : Syntax
  (make-syntax
    `(
      ,(reverse (entry-identifier-stack $entry))
      ,(entry-syntax $entry))))

(define (binder-stack-syntax-stack ($binder-stack : (Stackof Binder))) : (Stackof Syntax)
  (map expression-syntax
    (apply append
      (map binder-tuple $binder-stack))))

; ---------------------------------------------------------------------

(define (entry-stack-do-syntax
  ($entry-stack : (Stackof Entry))
  ($syntax : Syntax))
: Syntax
  (define $single-entry (single $entry-stack))
  (cond
    ((null? $entry-stack) $syntax)
    ((and
      $single-entry
      (equal? $syntax (single (entry-identifier-stack $single-entry))))
      (entry-syntax $single-entry))
    (else
      (define $entry-let-syntax-stack (map entry-let-syntax $entry-stack))
        (make-syntax
          `(let-values
            ,(reverse $entry-let-syntax-stack)
            ,$syntax)))))

(check-equal?
  (syntax->datum
    (entry-stack-do-syntax null syntax-a))
  (syntax->datum syntax-a))

(check-equal?
  (bind $tmp-syntax-a (tmp-syntax-a)
    (syntax->datum
      (entry-stack-do-syntax
        (stack (entry (stack $tmp-syntax-a) syntax-b))
        $tmp-syntax-a)))
  (syntax->datum syntax-b))

(check-equal?
  (syntax->datum
    (entry-stack-do-syntax
      (stack (entry (stack (tmp-syntax-a)) syntax-b))
      syntax-c))
  (syntax->datum
    #`(let-values
      (((#,(tmp-syntax-a)) #,syntax-b))
      #,syntax-c)))

(check-equal?
  (syntax->datum
    (entry-stack-do-syntax
      (stack (entry (stack (tmp-syntax-a) (tmp-syntax-b)) syntax-c))
      syntax-d))
  (syntax->datum
    #`(let-values
      (((#,(tmp-syntax-a) #,(tmp-syntax-b)) #,syntax-c))
      #,syntax-d)))

(check-equal?
  (syntax->datum
    (entry-stack-do-syntax
      (stack
        (entry (stack (tmp-syntax-a)) syntax-b)
        (entry (stack (tmp-syntax-c)) syntax-d))
      syntax-a))
  (syntax->datum
    #`(let-values
      (((#,(tmp-syntax-a)) #,syntax-b)
       ((#,(tmp-syntax-c)) #,syntax-d))
      #,syntax-a)))
