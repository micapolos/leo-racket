#lang leo/typed

(require
  leo/compiler/action
  leo/compiler/ingredients
  leo/compiler/expressions)

(define-type Ingredients-Action
  (U
    (Set-Action Ingredients)
    (Append-Action Ingredients)))

(define (ingredients-apply-action ($ingredients : Ingredients) ($action : Ingredients-Action)) : Ingredients
  (cond
    ((set-action? $action)
      (set-action-value $action))
    ((append-action? $action)
      (push-stack $ingredients (append-action-value $action)))))
