#lang leo/typed

(define-type Literal (U Number String))

(data (script of V)
  (line-stack : (Stackof (Lineof V))))

(data (word of V)
  (value : V)
  (symbol : Symbol))

(data (line of V)
  (value : V)
  (body : (U (Sentenceof V) Literal)))

(data (sentence of V)
  (word : (Wordof V))
  (script : (Scriptof V)))

(define-type Word (Wordof Void))
(define-type Script (Scriptof Void))
(define-type Line (Lineof Void))
(define-type Sentence (Sentenceof Void))

(: word-strip (All (V) (-> (Wordof V) Word)))
(define #:forall (V) (word-strip ($word : (Wordof V))) : Word
  (word nil (word-symbol $word)))

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
            (word-strip (sentence-word $body))
            (script-strip (sentence-script $body)))))
      (else (line nil $body)))))

(define null-script : Script (script null))

(define (null-line ($symbol : Symbol)) : Line
  (line nil (sentence (word nil $symbol) null-script)))
