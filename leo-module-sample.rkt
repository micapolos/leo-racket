#lang racket/base

(module library racket/base
  (provide (all-defined-out))
  (define foo 128)
  (define (inc n) (+ n 1))
  (define label (string-append (number->string (inc foo)) " apples!!!")))

(module types typed/racket/base
  (provide (all-defined-out))
  (require leo/runtime/types)
  (define foo-type number-type)
  (define inc-type (recipe! number-type (doing number-type)))
  (define label-type (field! `label text-type)))

(require leo/runtime/top-level 'library 'types)

(displayln-any-list-type-list
  (list foo inc label)
  (list foo-type inc-type label-type))
