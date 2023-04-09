#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/ingredients-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/value-utils
  leo/compiler/type-sexp
  leo/compiler/type-utils
  leo/compiler/binding
  leo/compiler/syntax-utils
  leo/evaluator/scope
  leo/evaluator/environment
)

(data evaluator
  (value-scope : Value-Scope)
  (packet : Packet))

(define empty-evaluator
  (evaluator
    base-value-scope
    null-packet))

(define-type Evaluate-Fn (-> Value-Scope (Listof Syntax) Packet))

(define (evaluator-sexp ($evaluator : Evaluator))
  `(evaluator
    ,(value-scope-sexp (evaluator-value-scope $evaluator))
    ,(packet-sexp (evaluator-packet $evaluator))))

(define (evaluator-do-value-scope ($evaluator : Evaluator)) : Value-Scope
  (value-scope-plus-binder-stack
    (evaluator-value-scope $evaluator)
    (map value-generate-binder (evaluator-packet $evaluator))))

(define (evaluate-do ($evaluator : Evaluator) ($syntax-list : (Listof Syntax)) ($evaluate-fn : Evaluate-Fn)) : Evaluator
  (struct-copy evaluator $evaluator
    (packet
      ($evaluate-fn
        (evaluator-do-value-scope $evaluator)
        $syntax-list))))

(check-equal?
  (evaluator-sexp
    (evaluate-do
      (evaluator
        base-value-scope
        (packet (value "foo" (racket))))
      (list syntax-a syntax-b)
      (lambda (($value-scope : Value-Scope) ($syntax-list : (Listof Syntax)))
        (packet
          (value (value-scope-sexp $value-scope) (racket))
          (value (map syntax->datum $syntax-list) (racket))))))
  (evaluator-sexp
    (evaluator
      base-value-scope
      (packet
        (value
          (value-scope-sexp
            (value-scope-plus-binder
              base-value-scope
              (value-binder (binding #`tmp-racket (racket)) "foo")))
          (racket))
        (value (map syntax->datum (list syntax-a syntax-b)) (racket))))))

; (define (evaluator-plus-default ($evaluator : Evaluator) ($syntax : Syntax)) ($evaluate-fn : Evaluate-Fn)) : Evaluator
;   (define $e (syntax-e $syntax))
;   (cond
;     (($string $e) )
;   (struct-copy evaluator $evaluator
;     (evaluated-packet
;       ($evaluate-fn
;         (evaluator-value-scope $evaluator)
;         $syntax-list))))

; (define (evaluator-plus-symbol ($evaluator : Evaluator) ($symbol : Symbol) ($syntax-list : (Listof Syntax)) ($evaluate-fn : Evaluate-Fn)) : Evaluator
;   (struct-copy evaluator $evaluator
;     (evaluated-packet
;       ($evaluate-fn
;         (evaluator-value-scope $evaluator)
;         $syntax-list))))

(define (evaluator-resolve ($evaluator : Evaluator)) : Evaluator
  (define $packet (evaluator-packet $evaluator))
  (define $binder-stack (map value-generate-binder $packet))
  (define $value-scope (evaluator-value-scope $evaluator))
  (define $new-value-scope (value-scope-plus-binder-stack $value-scope $binder-stack))
  (define $new-environment (value-scope-environment $new-value-scope))
  (define $scope (value-scope-scope $new-value-scope))
  (define $ingredients (map expression-expressions (map value-binder-expression $binder-stack)))
  (define $resolved-ingredients (scope-apply-ingredients $scope $ingredients))
  (define $resolved-expressions (ingredients-expressions $resolved-ingredients))
  (define $resolved-structure (expressions-structure $resolved-expressions))
  (define $resolved-any-list (environment-eval-list $new-environment (syntax->datum (expressions-syntax $resolved-expressions))))
  (define $resolved-packet (any-stack-structure-packet (reverse $resolved-any-list) $resolved-structure))
  (evaluator $value-scope $resolved-packet))

(bind
  $value-scope (value-scope
    base-environment
    (scope
      (binding
        #`string-append
        (recipe! text-type (field! `plus text-type) (doing text-type)))))

  (check-equal?
    (evaluator-sexp
      (evaluator-resolve
        (evaluator
          $value-scope
          (packet
            (value "foo" text-type)
            (value "bar" (field! `plus text-type))))))
    (evaluator-sexp
      (evaluator-resolve
        (evaluator
          $value-scope
          (packet
            (value "foobar" text-type))))))
)
