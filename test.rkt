#lang racket/base

(require leo/racket/runtime)

(let-values (((tmp-number tmp-text) (values 2 "foo")))
  (writeln
    (value-sexp 
      (value tmp-number 
        (field `cyferka (structure (field 'number (structure (racket))))))))
  (writeln
    (value-sexp 
      (value tmp-text 
        (field `napisik (structure (field 'text (structure (racket)))))))))
