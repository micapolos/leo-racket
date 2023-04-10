#lang leo/typed

(require
  leo/compiler/token)

(define-type Node
  (U
    Sexp-Node))

(data environment)

(data visitor
  (environment : Environment)
  (node : Node))

(data context)

(data sexp-node-symbol
  (sexp-node : Sexp-Node)
  (symbol : Symbol))

(define-type Sexp-Node-Parent
  (U
    Sexp-Node-Symbol))

(data sexp-node
  (parent-option : (Option Sexp-Node-Parent))
  (sexp-stack : (Stackof Sexp)))

(define (visitor-plus ($visitor : Visitor) ($token : Token)) : Visitor
  (cond
    ((begin? $token) (visitor-begin $visitor (begin-symbol $token)))
    ((end? $token) (visitor-end $visitor))))

(define (visitor-begin ($visitor : Visitor) ($symbol : Symbol)) : Visitor
  (define $environment (visitor-environment $visitor))
  (define $node (visitor-node $visitor))
  (visitor $environment
    (cond
      ((sexp-node? $node)
        (sexp-node-begin $node $symbol)))))

(define #:forall (V) (parent ($parent-option : (Option V))) : V
  (or $parent-option (error "no parent")))

(define (visitor-end ($visitor : Visitor)) : Visitor
  (define $environment (visitor-environment $visitor))
  (define $node (visitor-node $visitor))
  (visitor $environment
    (cond
      ((sexp-node? $node)
        (sexp-node-end $node)))))

(define (sexp-node-begin ($sexp-node : Sexp-Node) ($symbol : Symbol)) : Node
  (sexp-node
    (sexp-node-symbol $sexp-node $symbol)
    null))

(define (sexp-node-end ($sexp-node : Sexp-Node)) : Node
  (sexp-node-parent-end
    (parent (sexp-node-parent-option $sexp-node))
    (sexp-node-sexp-stack $sexp-node)))

(define (sexp-node-parent-end ($sexp-node-parent : Sexp-Node-Parent) ($sexp-stack : (Stackof Sexp))) : Node
  (cond
    ((sexp-node-symbol? $sexp-node-parent)
      (sexp-node-symbol-end $sexp-node-parent $sexp-stack))))

(define (sexp-node-symbol-end ($sexp-node-symbol : Sexp-Node-Symbol) ($sexp-stack : (Stackof Sexp))) : Sexp-Node
  (define $sexp-node (sexp-node-symbol-sexp-node $sexp-node-symbol))
  (define $symbol (sexp-node-symbol-symbol $sexp-node-symbol))
  (struct-copy sexp-node $sexp-node
    (sexp-stack
      (push
        (sexp-node-sexp-stack $sexp-node)
        (cond
          ((null? $sexp-stack) $symbol)
          (else `(,$symbol ,(reverse $sexp-stack))))))))

(fold
  (visitor (environment) (sexp-node #f null))
  (list (begin `foo) (begin `bar) (end) (begin `goo) (end) (end))
  visitor-plus)
