#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack)

(data (script of V)
  (line-stack : (Stackof (Lineof V))))

(data (line of V)
  (value-fn : (-> V)) 
  (body : (U (Sentenceof V) Number String)))

(data (sentence of V)
  (symbol : Symbol) 
  (script : (Scriptof V)))

(define-type Script (Scriptof Nothing))
(define-type Line (Lineof Nothing))
(define-type Sentence (Sentenceof Nothing))

(: script-strip (All (V) (-> (Scriptof V) Script)))
(define #:forall (V) (script-strip ($script : (Scriptof V))) : Script
  (script 
    (map 
      (ann line-strip (-> (Lineof V) Line))
      (script-line-stack $script))))

(: line-strip (All (V) (-> (Lineof V) Line)))
(define #:forall (V) (line-strip ($line : (Lineof V))) : Line
  (bind $body (line-body $line)
    (cond
      ((sentence? $body)
        (line
          nothing
          (sentence 
            (sentence-symbol $body)
            (script-strip (sentence-script $body)))))
      ((number? $body) (line nothing $body))
      ((string? $body) (line nothing $body)))))

(define null-script : Script (script null))

(define (null-line ($symbol : Symbol)) : Line
  (line nothing (sentence $symbol null-script)))
