#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/ingredients-utils
  leo/compiler/compiler-plus-expressions
  leo/evaluator/scope
  leo/evaluator/environment)

(data evaluator
  (value-scope : Value-Scope)
  (packet : Packet))

(define empty-evaluator
  (evaluator
    base-value-scope
    null-packet))

(define-type Evaluate-Fn (-> Value-Scope (Listof Syntax) Packet))

(define (evaluator-do-value-scope ($evaluator : Evaluator)) : Value-Scope
  (value-scope-plus-binder-stack
    (evaluator-value-scope $evaluator)
    (map value-generate-binder (evaluator-packet $evaluator))))

(define (evaluator-plus-do ($evaluator : Evaluator) ($syntax-list : (Listof Syntax)) ($evaluate-fn : Evaluate-Fn)) : Evaluator
  (struct-copy evaluator $evaluator
    (packet
      ($evaluate-fn
        (evaluator-do-value-scope $evaluator)
        $syntax-list))))

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
  (define $any-list (environment-eval-list $new-environment (syntax->datum (expressions-syntax $resolved-expressions))))
  TODO)