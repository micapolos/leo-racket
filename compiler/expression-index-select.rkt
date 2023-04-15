#lang leo/typed

(define (resolve-expression-choice-cast ($expression : Expression) ($choice : Choice)) : (Option Expression)
  (structure-dynamic-index (choice-structure)
