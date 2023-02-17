#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/option
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/binding
  leo/typed/base-binding-list
  leo/typed/syntax-match
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/typed/syntax-type
  leo/testing)

(struct compiled 
  (
    (binding-list : (Listof Binding))
    (syntax-list : (Listof Syntax)))
  #:transparent
  #:type-name Compiled)

(define null-compiled (compiled null null))
(define base-compiled (compiled base-binding-list null))

(define 
  (compiled-with-syntax-list
    ($compiled : Compiled)
    ($syntax-list : (Listof Syntax))) : Compiled
  (struct-copy compiled $compiled (syntax-list $syntax-list)))

(define 
  (compiled-with-binding-list
    ($compiled : Compiled)
    ($binding-list : (Listof Binding))) : Compiled
  (struct-copy compiled $compiled (binding-list $binding-list)))

(define 
  (compiled-plus-binding
    ($compiled : Compiled)
    ($binding : Binding)) : Compiled
  (compiled-with-binding-list $compiled 
    (cons $binding (compiled-binding-list $compiled))))

(define 
  (compiled-plus-typed-syntax 
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (compiled-with-syntax-list
    $compiled
    (cons $syntax (compiled-syntax-list $compiled))))

(define
  (compiled-plus-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (let* (($binding-list (compiled-binding-list $compiled))
         ($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) 
        (error "null syntax???"))
      ((boolean? $syntax-e)
        (compiled-plus-typed-syntax 
          $compiled
          (syntax-with-type $syntax boolean-type)))
      ((number? $syntax-e)
        (compiled-plus-typed-syntax
          $compiled
          (syntax-with-type $syntax number-type)))
      ((string? $syntax-e)
        (compiled-plus-typed-syntax
          $compiled
          (syntax-with-type $syntax string-type)))
      ((symbol? $syntax-e)
        (compiled-plus-typed-syntax
          $compiled
          (binding-list-apply-symbol $binding-list $syntax-e)))
      (else
        (or
          (let (($do-syntax (binding-list-parse-do $binding-list $syntax)))
            (and 
              $do-syntax
              (compiled-plus-typed-syntax $compiled $do-syntax)))
          (cond
            ((syntax-identifier-args? $syntax)
              (define $identifier (car $syntax-e))
              (define $symbol (syntax-e $identifier))
              (define $args (cdr $syntax-e))
              (cond 
                ((equal? $symbol `function)
                  (error "TODO: function"))
                (else
                  (compiled-plus-typed-syntax
                    $compiled
                    (binding-list-apply-symbol-args
                      $binding-list
                      $symbol
                      (map (curry binding-list-syntax $binding-list) $args))))))
            (else (error (format "Syntax error ~a" $syntax)))))))))

(define 
  (compiled-plus-syntax-list
    ($compiled : Compiled)
    ($syntax-list : (Listof Syntax))) : Compiled
  (foldl
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-plus-syntax $compiled $syntax))
    $compiled
    $syntax-list))

(define (compiled-syntax ($compiled : Compiled)) : Syntax
  (let (($syntax-list (compiled-syntax-list $compiled)))
  (cond
    ((null? $syntax-list) #`(void))
    ((null? (cdr $syntax-list)) (car $syntax-list))
    (else (error "TODO: Multi-syntax")))))

; -----------------------------------------------------------------------

(define
  (binding-list-syntax
    ($binding-list : (Listof Binding))
    ($syntax : Syntax)) : Syntax
  (compiled-syntax
    (compiled-plus-syntax
      (compiled $binding-list null)
      $syntax)))

; ---------------------------------------------------------------

(define 
  (binding-list-parse-do 
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (and
    (syntax-symbol-arg-arg? $syntax `do)
    (let* (($tmp-syntax (car (generate-temporaries `(tmp))))
           ($syntax-e (syntax-e $syntax))
           ($expr-syntax (cadr $syntax-e))
           ($body-syntax (caddr $syntax-e))
           ($expr-typed-syntax (binding-list-syntax $binding-list $expr-syntax))
           ($expr-type (syntax-type $expr-typed-syntax))
           ($body-binding-list (cons (argument-binding $expr-type $tmp-syntax) $binding-list))
           ($body-typed-syntax (binding-list-syntax $body-binding-list $body-syntax))
           ($body-type (syntax-type $body-typed-syntax)))
      (syntax-with-type
        (datum->syntax #f 
          `(let 
            ((,$tmp-syntax ,$expr-typed-syntax))
            ,$body-typed-syntax))
        $body-type))))

(check-equal?
  (option-map
    (binding-list-parse-do
      null
      #`(do 1 number))
    syntax-typed-datum)
  ; TODO: Fix this test, generated tmp1 is not guaranteed.
  (typed `(let ((tmp1 1)) tmp1) number-type))

; -------------------------------------------------------------------

; (define
;   (compiled-plus-define-syntax
;     ($compiled : Compiled)
;     ($syntax : Syntax)) : (Option Compiled)
;   (define $bindings (compiled-bindings $compiled))
;   (cond
;     ((syntax-symbol-arg? $syntax `define)
;       (define $define-syntax (cadr (syntax-e $syntax)))
;       (cond
;         ((syntax-symbol-arg-arg? $define-syntax `is) 
;           (define $is-lhs (cadr (syntax-e $define-syntax)))
;           (define $is-rhs (caddr (syntax-e $define-syntax)))
;           (cond 
;             ((syntax-symbol-arg-arg? $is-lhs `giving)
;               (define $native-rhs 
;                 (and 
;                   (syntax-symbol-arg? $is-rhs `native)
;                   (cadr (syntax-e $is-rhs))))
;               (define $giving-lhs (cadr (syntax-e $is-lhs)))
;               (define $giving-rhs (caddr (syntax-e $is-lhs)))
;               (define $lhs-type (syntax-parse-type $giving-lhs))
;               (define $rhs-type (syntax-parse-type $giving-rhs))
;               (define $arrow-type (arrow-type (list $lhs-type) (list $rhs-type)))
;               (define $function? (not (identifier? $giving-lhs)))
;               (define $binding-type
;                 (if $function? $lhs-type (symbol-type (syntax-e $giving-lhs))))
;               (define $expected-body-type
;                 (if $function? $arrow-type $rhs-type))
;               (define $body 
;                 (or 
;                   (and $native-rhs (syntax-with-type $native-rhs $expected-body-type))
;                   (bindings-syntax $bindings $is-rhs)))
;               (define $actual-type (syntax-type $body))
;               (if (equal? $actual-type $expected-body-type)
;                 $actual-type
;                 (error 
;                   (format 
;                     "type error, expected: ~a, actual: ~a"
;                     $expected-body-type
;                     $actual-type)))
;               ; TODO: Check that
;               ; - $is-rhs has no type, assuming it's native
;               ; - $is-rhs has type, and it matches
;               (define $binding 
;                 (binding 
;                   $binding-type
;                   $body
;                   $function?))
;               (compiled-plus-binding $compiled $binding))
;             (else (error (format "Illegal is lhs ~a" $is-lhs)))))
;         ((syntax-symbol-arg-arg? $define-syntax `does) 
;           (define $lhs (cadr (syntax-e $define-syntax)))
;           (define $rhs (caddr (syntax-e $define-syntax)))
;           (define $lhs-type (syntax-parse-type $lhs))
;           $compiled)
;         (else (error (format "Illegal define ~a" $define-syntax)))))
;     (else #f)))
