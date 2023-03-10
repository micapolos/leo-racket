#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack)

(define-type Literal (U Number String))

(data (script of V)
  (line-stack : (Stackof (Lineof V))))

(data (line of V)
  (value : V)
  (body : (U (Sentenceof V) Literal)))

(data (sentence of V)
  (symbol : Symbol) 
  (script : (Scriptof V)))

(define-type Script (Scriptof Void))
(define-type Line (Lineof Void))
(define-type Sentence (Sentenceof Void))

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
        (line nil
          (sentence 
            (sentence-symbol $body)
            (script-strip (sentence-script $body)))))
      (else (line nil $body)))))

(define null-script : Script (script null))

(define (null-line ($symbol : Symbol)) : Line
  (line nil (sentence $symbol null-script)))
