#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/bindings
  racket/function
  leo/typed/syntax-match
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/typed/syntax-type)

(struct compiled 
  (
    (bindings : Bindings) 
    (syntaxes : (Listof Syntax)))
  #:transparent
  #:type-name Compiled)

(define null-compiled (compiled null-bindings null))
(define base-compiled (compiled base-bindings null))

(define 
  (compiled-with-syntaxes
    ($compiled : Compiled)
    ($syntaxes : (Listof Syntax))) : Compiled
  (struct-copy compiled $compiled (syntaxes $syntaxes)))

(define 
  (compiled-with-bindings
    ($compiled : Compiled)
    ($bindings : Bindings)) : Compiled
  (struct-copy compiled $compiled (bindings $bindings)))

(define 
  (compiled-plus-binding
    ($compiled : Compiled)
    ($binding : Binding)) : Compiled
  (compiled-with-bindings $compiled 
    (bindings-plus (compiled-bindings $compiled) $binding)))

(define 
  (compiled-plus-compiled-syntax 
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (compiled-with-syntaxes 
    $compiled
    (cons $syntax (compiled-syntaxes $compiled))))

(define
  (compiled-plus-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (let* (($bindings (compiled-bindings $compiled))
         ($plus-compiled (compiled-plus-define-syntax $compiled $syntax)))
    (cond
      ((compiled? $plus-compiled) $plus-compiled)
      (else 
        (compiled-plus-compiled-syntax $compiled 
          (bindings-syntax $bindings $syntax))))))

(define 
  (compiled-plus-syntaxes
    ($compiled : Compiled)
    ($syntaxes : (Listof Syntax))) : Compiled
  (foldl
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-plus-syntax $compiled $syntax))
    $compiled
    $syntaxes))


; -------------------------------------------------------------------

(define
  (compiled-plus-define-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : (Option Compiled)
  (define $bindings (compiled-bindings $compiled))
  (cond
    ((syntax-symbol-arg? $syntax `define)
      (define $define-syntax (cadr (syntax-e $syntax)))
      (cond
        ((syntax-symbol-arg-arg? $define-syntax `is) 
          (define $is-lhs (cadr (syntax-e $define-syntax)))
          (define $is-rhs (caddr (syntax-e $define-syntax)))
          (cond 
            ((syntax-symbol-arg-arg? $is-lhs `giving)
              (define $native-rhs 
                (and 
                  (syntax-symbol-arg? $is-rhs `native)
                  (cadr (syntax-e $is-rhs))))
              (define $giving-lhs (cadr (syntax-e $is-lhs)))
              (define $giving-rhs (caddr (syntax-e $is-lhs)))
              (define $lhs-type (syntax-parse-type $giving-lhs))
              (define $rhs-type (syntax-parse-type $giving-rhs))
              (define $arrow-type (arrow-type (list $lhs-type) (list $rhs-type)))
              (define $function? (not (identifier? $giving-lhs)))
              (define $binding-type
                (if $function? $lhs-type (symbol-type (syntax-e $giving-lhs))))
              (define $expected-body-type
                (if $function? $arrow-type $rhs-type))
              (define $body 
                (or 
                  (and $native-rhs (syntax-with-type $native-rhs $expected-body-type))
                  (bindings-syntax $bindings $is-rhs)))
              (define $actual-type (syntax-type $body))
              (if (equal? $actual-type $expected-body-type)
                $actual-type
                (error 
                  (format 
                    "type error, expected: ~a, actual: ~a"
                    $expected-body-type
                    $actual-type)))
              ; TODO: Check that
              ; - $is-rhs has no type, assuming it's native
              ; - $is-rhs has type, and it matches
              (define $binding 
                (binding 
                  $binding-type
                  $body
                  $function?))
              (compiled-plus-binding $compiled $binding))
            (else (error (format "Illegal is lhs ~a" $is-lhs)))))
        ((syntax-symbol-arg-arg? $define-syntax `does) 
          (define $lhs (cadr (syntax-e $define-syntax)))
          (define $rhs (caddr (syntax-e $define-syntax)))
          (define $lhs-type (syntax-parse-type $lhs))
          $compiled)
        (else (error (format "Illegal define ~a" $define-syntax)))))
    (else #f)))
