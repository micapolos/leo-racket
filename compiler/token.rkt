#lang leo/typed

(require
  leo/compiler/literal)

(define-type Token (U Begin End))

(data begin
  (symbol : Symbol))

(data end)
