#lang leo/typed

(require 
  leo/compiler/type)

(data evaluator
  (input-value-stack : (Stackof Value))
  (output-value-stack : (Stackof Value)))
