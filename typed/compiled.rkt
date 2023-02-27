#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  racket/string
  leo/typed/base
  leo/typed/option
  leo/typed/type-utils
  leo/typed/type
  leo/typed/typed
  leo/typed/typed-syntax
  leo/typed/types
  leo/typed/type-symbol
  leo/typed/type-generate-temporary
  leo/typed/binding
  leo/typed/binding-syntax
  leo/typed/syntaxes
  leo/typed/syntax-match
  leo/typed/syntax-typed
  leo/typed/syntax-top-level
  leo/typed/syntax-resolve
  leo/typed/type-parse
  leo/typed/syntax-parse
  leo/typed/syntax-type
  leo/typed/testing
  leo/typed/values)

(struct compiled 
  (
    (binding-list : (Listof Binding))
    (provided-binding-list : (Listof Binding))
    (syntax-list : (Listof Syntax)))
  #:transparent
  #:type-name Compiled)

(define null-compiled (compiled null null null))

(define (compiled-body-syntax-list ($compiled : Compiled)) : (Listof Syntax)
  (reverse
    (cons 
      (binding-list-meta-module-syntax (compiled-provided-binding-list $compiled))
      (map syntax-top-level
        (compiled-syntax-list $compiled)))))

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
  (compiled-with-provided-binding-list
    ($compiled : Compiled)
    ($binding-list : (Listof Binding))) : Compiled
  (struct-copy compiled $compiled (provided-binding-list $binding-list)))

(define 
  (compiled-plus-binding
    ($compiled : Compiled)
    ($binding : Binding)) : Compiled
  (compiled-with-binding-list $compiled 
    (cons $binding (compiled-binding-list $compiled))))

(define 
  (compiled-plus-provided-binding
    ($compiled : Compiled)
    ($binding : Binding)) : Compiled
  (compiled-with-provided-binding-list $compiled 
    (cons $binding (compiled-provided-binding-list $compiled))))

(define 
  (compiled-plus-exported-binding
    ($compiled : Compiled)
    ($binding : Binding)) : Compiled
  (compiled-plus-provided-binding 
    (compiled-plus-binding $compiled $binding)
    $binding))

(define 
  (compiled-plus-syntax 
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (compiled-with-syntax-list
    $compiled
    (cons $syntax (compiled-syntax-list $compiled))))

(define
  (compiled-parse-syntax
    ($compiled : Compiled)
    ($syntax : Syntax)) : Compiled
  (let* (($binding-list (compiled-binding-list $compiled))
         ($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) 
        (error "null syntax???"))
      ((equal? $syntax-e `true)
        (compiled-plus-syntax 
          $compiled
          (syntax-with-type (datum->syntax #f #t $syntax) boolean-type)))
      ((equal? $syntax-e `false)
        (compiled-plus-syntax 
          $compiled
          (syntax-with-type (datum->syntax #f #f $syntax) boolean-type)))
      ((number? $syntax-e)
        (compiled-plus-syntax
          $compiled
          (syntax-with-type $syntax number-type)))
      ((string? $syntax-e)
        (compiled-plus-syntax
          $compiled
          (syntax-with-type $syntax string-type)))
      ((symbol? $syntax-e)
        (compiled-plus-syntax
          $compiled
          (binding-list-apply-symbol $binding-list $syntax-e)))
      ((and (syntax-symbol-arg? $syntax `fixnum) (fixnum? (syntax-e (cadr $syntax-e))))
        (compiled-plus-syntax
          $compiled
          (syntax-with-type (cadr $syntax-e) fixnum-type)))
      ((and (syntax-symbol-arg? $syntax `flonum) (flonum? (syntax-e (cadr $syntax-e))))
        (compiled-plus-syntax
          $compiled
          (syntax-with-type (cadr $syntax-e) flonum-type)))
      ((syntax-symbol-arg? $syntax `any)
        (compiled-plus-syntax
          $compiled
          (type-typed-syntax (syntax-parse-type (cadr $syntax-e)))))
      (else
        (or
          (bind $racket-syntax (syntax-parse-racket $syntax)
            (and $racket-syntax (compiled-plus-syntax $compiled $racket-syntax)))
          (let (($do-syntax (binding-list-parse-do $binding-list $syntax)))
            (and 
              $do-syntax
              (compiled-plus-syntax $compiled $do-syntax)))
          (let (($then-else-syntax (binding-list-parse-then-else $binding-list $syntax)))
            (and 
              $then-else-syntax
              (compiled-plus-syntax $compiled $then-else-syntax)))
          (let (($doing-syntax (binding-list-parse-doing $binding-list $syntax)))
            (and 
              $doing-syntax
              (compiled-plus-syntax $compiled $doing-syntax)))
          (bind $as-syntax (binding-list-parse-as $binding-list $syntax)
            (and 
              $as-syntax 
              (compiled-plus-syntax $compiled $as-syntax)))
          (compiled-parse-make $compiled $syntax)
          (compiled-parse-require $compiled $syntax)
          (compiled-parse-does $compiled $syntax)
          (compiled-parse-is $compiled $syntax)
          (cond
            ((syntax-identifier-args? $syntax)
              (define $identifier (car $syntax-e))
              (define $symbol (syntax-e $identifier))
              (define $args (cdr $syntax-e))
              (compiled-plus-syntax
                $compiled
                (binding-list-apply-symbol-args
                  $binding-list
                  $symbol
                  (map (curry binding-list-syntax $binding-list) $args))))
            (else (error (format "Syntax error ~a" $syntax)))))))))

(define 
  (compiled-parse-syntax-list
    ($compiled : Compiled)
    ($syntax-list : (Listof Syntax))) : Compiled
  (foldl
    (lambda (($syntax : Syntax) ($compiled : Compiled))
      (compiled-parse-syntax $compiled $syntax))
    $compiled
    $syntax-list))

(define (compiled-syntax ($compiled : Compiled)) : Syntax
  (let (($syntax-list (compiled-syntax-list $compiled)))
  (cond
    ((null? $syntax-list) #`(void))
    ((null? (cdr $syntax-list)) (car $syntax-list))
    (else (error "TODO: Multi-syntax")))))

(define (compile-syntax ($syntax : Syntax)) : Syntax
  (compiled-syntax (compiled-parse-syntax null-compiled $syntax)))

(define (compile-typed ($syntax : Syntax))
  (syntax-typed-datum (compile-syntax $syntax)))

(define (compile-binding ($syntax : Syntax))
  (bind $binding-list (compiled-binding-list (compiled-parse-syntax null-compiled $syntax))
    (when (null? $binding-list) (error "No bindings"))
    (car $binding-list)))

(define
  (binding-list-syntax
    ($binding-list : (Listof Binding))
    ($syntax : Syntax)) : Syntax
  (compiled-syntax
    (compiled-parse-syntax
      (compiled $binding-list null null)
      $syntax)))

; ---------------------------------------------------------------

(define 
  (binding-list-parse-do 
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (and
    (syntax-symbol-arg-args? $syntax `do)
    (let* (($syntax-e (syntax-e $syntax))
           ($do-reverse-syntaxes (reverse (cdr $syntax-e)))
           ($expr-syntaxes (reverse (cdr $do-reverse-syntaxes)))
           ($body-syntax (car $do-reverse-syntaxes))
           ($expr-typed-syntaxes (map (curry binding-list-syntax $binding-list) $expr-syntaxes))
           ($expr-types (map syntax-type $expr-typed-syntaxes))
           ($tmp-syntaxes (map type-generate-temporary $expr-types))
           ($argument-bindings (map argument-binding $expr-types $tmp-syntaxes))
           ($body-binding-list (append $argument-bindings $binding-list))
           ($body-typed-syntax (binding-list-syntax $body-binding-list $body-syntax))
           ($body-type (syntax-type $body-typed-syntax)))
      (syntax-with-type
        (datum->syntax $syntax 
          (list `let 
            (map 
              (lambda (($tmp : Syntax) ($expr : Syntax)) (list $tmp $expr)) 
              $tmp-syntaxes 
              $expr-typed-syntaxes)
            $body-typed-syntax))
        $body-type))))

; TODO: Fix the test, generated number1 is not guaranteed.
; (check-equal?
;   (option-map
;     (binding-list-parse-do
;       null
;       #`(do 1 number))
;     syntax-typed-datum)
;   (typed `(let ((number1 1)) number1) number-type))

; ---------------------------------------------------------------


(define
  (compiled-parse-is
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (syntax-symbol-match-arg-arg $syntax `is $is-lhs $is-rhs
    (define $binding-list (compiled-binding-list $compiled))
    (define $is-lhs-value (binding-list-syntax $binding-list $is-lhs))
    (define $is-lhs-type (syntax-type $is-lhs-value))
    (unless (any? $is-lhs-type)
      (error "is lhs not a type"))
    (define $lhs-type (any-type $is-lhs-type))
    (define-values
      ($param-types $return-type)
      (cond
        ((arrow? $lhs-type)
          (values 
            (arrow-lhs-types $lhs-type)
            (arrow-rhs-type $lhs-type)))
        (else (values (list $lhs-type) #f))))
    (unless (= (length $param-types) 1)
      (error "expected single param type"))
    (define $param-type (car $param-types))
    (unless (and (tuple? $param-type) (null? (tuple-type-list $param-type)))
      (error "expected symbol-type"))
    (define $symbol (tuple-symbol $param-type))
    (define $body $is-rhs)
    (or
      (syntax-symbol-match-args $body `racket $native-args
        (unless (= (length $native-args) 1)
          (error "expected 1 native arg"))
        (unless $return-type (error "native requires type"))
        (define $native-body (car $native-args))
        (unless (identifier? $native-body)
          (error "native must be identifier"))
        (define $binding 
          (constant-binding $symbol $return-type (syntax-e $native-body)))
        (compiled-plus-exported-binding $compiled $binding))
      (let ()
        (define $typed-body (binding-list-syntax $binding-list $body))
        (define $body-return-type (syntax-type $typed-body))
        (when 
          (and
            $return-type
            (not (equal? $body-return-type $return-type)))
          (error 
            (format 
              "Expression: ~a, type: ~a, expected type: ~a"
              (syntax-e $body)
              $body-return-type 
              $return-type)))
        (define $fn (type-generate-temporary $param-type))
        (define $binding 
          (constant-binding $symbol $body-return-type (syntax-e $fn)))
        (define $compiled-syntax 
          (datum->syntax #f `(define ,$fn ,$typed-body)))
        (compiled-plus-syntax
          (compiled-plus-exported-binding $compiled $binding)
          $compiled-syntax)))))

; ----------------------------------------------------------------------

(define
  (compiled-parse-does
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (syntax-symbol-match-arg-arg $syntax `does $does-lhs $does-rhs
    (define $binding-list (compiled-binding-list $compiled))
    (define $does-lhs-value (binding-list-syntax $binding-list $does-lhs))
    (define $does-lhs-type (syntax-type $does-lhs-value))
    (unless (any? $does-lhs-type)
      (error "does lhs not a type"))
    (define $lhs-type (any-type $does-lhs-type))
    (define-values
      ($param-types $return-type)
      (cond
        ((arrow? $lhs-type)
          (values 
            (arrow-lhs-types $lhs-type)
            (arrow-rhs-type $lhs-type)))
        (else (values (list $lhs-type) #f))))
    (unless (= (length $param-types) 1)
      (error "expected single param type"))
    (define $param-type (car $param-types))
    (unless (tuple? $param-type)
      (error (format "expected tuple param type: ~a" $lhs-type)))
    (define $symbol (tuple-symbol $param-type))
    (define $field-param-types (tuple-type-list $param-type))
    (define $dynamic-param-types (filter type-is-dynamic? $field-param-types))
    (define $param-tmps
      (map type-generate-temporary $dynamic-param-types))
    (define $typed-param-tmps
      (map syntax-with-type $param-tmps $dynamic-param-types))
    (define $argument-bindings
      (map
        argument-binding
        $dynamic-param-types $typed-param-tmps))
    (or
      (syntax-symbol-match-args $does-rhs `racket $native-args
        (unless (= (length $native-args) 1)
          (error "expected 1 native arg"))
        (unless $return-type (error "native requires type"))
        (define $native-body (car $native-args))
        (unless (identifier? $native-body)
          (error "native must be identifier"))
        (define $binding 
          (function-binding $symbol $field-param-types $return-type (syntax-e $native-body)))
        (compiled-plus-exported-binding $compiled $binding))
      (let ()
        (define $fn (type-generate-temporary $param-type))
        (define $body
          (or
            (syntax-symbol-match-args $does-rhs `recursively $args
              (unless (= (length $args) 1) (error "recursive multi-body"))
              (car $args))
            $does-rhs))
        (define $final-bindings
          (or
            (syntax-symbol-match-args $does-rhs `recursively $args
              (unless $return-type (error "recursive must have return type"))
              (cons
                (function-binding $symbol $field-param-types $return-type (syntax-e $fn))
                $argument-bindings))
            $argument-bindings))
        (define $body-binding-list (append $final-bindings $binding-list))
        (define $typed-body (binding-list-syntax $body-binding-list $body))
        (define $body-return-type (syntax-type $typed-body))
        (when 
          (and
            $return-type
            (not (equal? $body-return-type $return-type)))
          (error 
            (format 
              "Expression: ~a, type: ~a, expected type: ~a"
              (syntax-e $body)
              $body-return-type 
              $return-type)))
        (define $binding
          (function-binding $symbol $field-param-types $body-return-type (syntax-e $fn)))
        (define $compiled-syntax 
          (datum->syntax #f `(define (,$fn ,@$param-tmps) ,$typed-body)))
        (compiled-plus-syntax
          (compiled-plus-exported-binding $compiled $binding)
          $compiled-syntax)))))

; ---------------------------------------------------------------------

(define 
  (compiled-parse-make
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (syntax-symbol-match-args $syntax `define $args
    (foldl
      (lambda (($arg : Syntax) ($compiled : Compiled))
        (define $binding-list (compiled-binding-list $compiled))
        (define $value (binding-list-syntax $binding-list $arg))
        (define $type (syntax-type $value))
        (define $tmp (type-generate-temporary $type))
        (cond
          ((arrow? $type)
            (define $param-types (arrow-lhs-types $type))
            (unless (= (length $param-types) 1)
              (error "multi-param not supported"))
            (define $param-type (car $param-types))
            (unless (list? $param-type)
              (error "list param type expected"))
            (unless (not (null? $param-type))
              (error "expected non-empty list"))
            (define $symbol (car $param-type))
            (unless (symbol? $symbol)
              (error "symbol expected"))
            (define $fn-param-types (cdr $param-type))
            (define $body-types (arrow-lhs-types $type))
            (unless (= (length $body-types) 1)
              (error "multi-retirn not supproted"))
            (define $return-type (car $body-types))
            (compiled-plus-syntax
              (compiled-plus-exported-binding
                $compiled
                (function-binding $symbol $fn-param-types $return-type (syntax-e $tmp)))
              (datum->syntax #f `(define ,$tmp ,$value))))
          (else 
            (define $symbol (type-symbol $type))
            (unless $symbol (error "not a symbol"))
            (compiled-plus-syntax
              (compiled-plus-exported-binding
                $compiled
                (constant-binding $symbol $type (syntax-e $tmp)))
              (datum->syntax #f `(define ,$tmp ,$value))))))
      $compiled
      $args)))

; --------------------------------------------------------------------

(define 
  (compiled-parse-require
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (syntax-symbol-match-args $syntax `require $args
    (foldl
      (lambda (($arg : Syntax) ($compiled : Compiled))
        (unless (identifier? $arg) (error "require must be symbol"))
        (compiled-plus-require $compiled (syntax-e $arg)))
      $compiled
      $args)))

(define-namespace-anchor namespace-anchor)

(define 
  (compiled-plus-require
    ($compiled : Compiled)
    ($module-symbol : Symbol))
  : Compiled
  (define $compiled-plus-require
    (compiled-plus-syntax
      $compiled
      (datum->syntax #f `(require ,$module-symbol))))
  (define $bindings
    (and
      (string-prefix? (symbol->string $module-symbol) "leo/")
      (parameterize 
        ((current-namespace (namespace-anchor->namespace namespace-anchor)))
        (dynamic-require 
          `(submod ,$module-symbol meta) 
          `bindings 
          (lambda () #f)))))
  (if $bindings
    (compiled-with-binding-list
      $compiled-plus-require
      (append 
        (reverse (cast $bindings (Listof Binding)))
        (compiled-binding-list $compiled-plus-require)))
    $compiled-plus-require))

(bind $compiled (compiled-plus-require null-compiled `leo/mini)
  (check-equal? 
    (length (compiled-binding-list $compiled))
    2))

; --------------------------------------------------------------------

(define
  (binding-list-parse-doing
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (cond
    ((or (syntax-symbol-arg-args? $syntax `doing) (syntax-symbol-arg-args? $syntax `lambda))
      (define $doing-reverse-args (reverse (cdr (syntax-e $syntax))))
      (define $doing-lhss (reverse (cdr $doing-reverse-args)))
      (define $doing-rhs (car $doing-reverse-args))
      (define $doing-lhs (and (= (length $doing-lhss) 1) (car $doing-lhss)))
      (define-values
        ($arg-types $return-type)
        (cond
          (
            (and 
              $doing-lhs 
              (or 
                (syntax-symbol-arg-args? $doing-lhs `giving) 
                (syntax-symbol-arg-args? $doing-lhs `->)))
            (define $giving-reverse-args (reverse (cdr (syntax-e $doing-lhs))))
            (define $giving-lhss (reverse (cdr $giving-reverse-args)))
            (define $giving-rhs (car $giving-reverse-args))
            (values 
              (map syntax-parse-type $giving-lhss) 
              (syntax-parse-type $giving-rhs)))
          (else 
            (values 
              (map syntax-parse-type $doing-lhss) 
              #f))))
      (define $dynamic-arg-types (filter type-is-dynamic? $arg-types))
      (define $arg-tmps
        (map type-generate-temporary $dynamic-arg-types))
      (define $typed-arg-tmps
        (map syntax-with-type $arg-tmps $dynamic-arg-types))
      (define $argument-bindings
        (map
          argument-binding
          $dynamic-arg-types $typed-arg-tmps))
      (cond 
        ((syntax-symbol-arg? $doing-rhs `racket)
          (unless $return-type (error "native requires type"))
          (define $native-args (cdr (syntax-e $doing-rhs)))
          (define $native-body (car $native-args))
          (unless (identifier? $native-body)
            (error "native must be identifier"))
          (define $racket $return-type) ; the is a lie
          (syntax-with-type
            $native-body
            (arrow $arg-types $return-type)))
        (else
          (or
            (syntax-symbol-match-args $doing-rhs `recursively $args
              (unless (= (length $args) 1) (error "recursive multi-body"))
              (error "TODO: recursive"))
            (let ()
              (define $body $doing-rhs)
              (define $body-binding-list (append $argument-bindings $binding-list))
              (define $typed-body (binding-list-syntax $body-binding-list $body))
              (define $body-return-type (syntax-type $typed-body))
              (when 
                (and
                  $return-type 
                  (not (equal? $body-return-type $return-type)))
                (error 
                  (format 
                    "Expression: ~a, type: ~a, expected type: ~a"
                    (syntax-e $body)
                    $body-return-type 
                    $return-type)))
              (syntax-with-type
                (datum->syntax #f `(lambda (,@$arg-tmps) ,$typed-body))
                (arrow $arg-types $body-return-type)))))))
    (else #f)))

; ----------------------------------------------------------------------

(define
  (binding-list-parse-then-else
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (syntax-symbol-match-arg-arg $syntax `else $else-lhs $else-rhs
    (syntax-symbol-match-arg-arg $else-lhs `then $then-lhs $then-rhs
      (define $condition (binding-list-syntax $binding-list $then-lhs))
      (unless (equal? (syntax-type $condition) boolean-type)
        (error "condition must be boolean"))
      (define $consequent (binding-list-syntax $binding-list $then-rhs))
      (define $alternate (binding-list-syntax $binding-list $else-rhs))
      (unless (equal? (syntax-type $consequent) (syntax-type $alternate))
        (error "then and else type mismatch"))
      (syntax-with-type
        (datum->syntax #f `(if ,$condition ,$consequent ,$alternate))
        (syntax-type $alternate)))))

; --------------------------------------------------------------------

(define
  (binding-list-parse-as 
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (syntax-symbol-match-arg-arg $syntax `as $lhs $rhs
    (define $value (binding-list-syntax $binding-list $lhs))
    (define $type-option (syntax-type-option $value))
    (when $type-option (error "as lhs should be untyped"))
    (unless (identifier? $value) (error "as lhs should be symbol"))
    (define $type (syntax-parse-type $rhs))
    (syntax-with-type $value $type)))

; ----------------------------------------------------------------------

(check-equal?
  (compile-typed #`true)
  (typed #t boolean-type))

(check-equal?
  (compile-typed #`false)
  (typed #f boolean-type))

(check-equal?
  (compile-typed #`1)
  (typed 1 number-type))

(check-equal?
  (compile-typed #`"foo")
  (typed "foo" string-type))

(check-equal?
  (compile-typed #`(fixnum 1))
  (typed 1 fixnum-type))

(check-equal?
  (compile-typed #`(flonum 1.0))
  (typed 1.0 flonum-type))

(check-equal?
  (compile-typed #`(any number))
  (typed null-value (any number-type)))

(bind
  $binding (compile-binding #`(does (any (increment number)) (done number)))
  (unless (function-binding? $binding) (error "not a function binding"))
  (check-equal? (function-binding-symbol $binding) `increment)
  (check-equal? (function-binding-param-types $binding) (list number-type))
  (check-equal? 
    (function-binding-return-type $binding) 
    (tuple `done (list number-type))))

(bind
  $binding (compile-binding #`(does (any (giving (plus number number) number)) (racket +)))
  (unless (function-binding? $binding) (error "not a function binding"))
  (check-equal? (function-binding-symbol $binding) `plus)
  (check-equal? (function-binding-param-types $binding) (list number-type number-type))
  (check-equal? (function-binding-return-type $binding) number-type))

(bind
  $binding (compile-binding #`(does (any (giving (stringify number) string)) "foo"))
  (unless (function-binding? $binding) (error "not a function binding"))
  (check-equal? (function-binding-symbol $binding) `stringify)
  (check-equal? (function-binding-param-types $binding) (list number-type))
  (check-equal? (function-binding-return-type $binding) string-type))

(bind
  $binding (compile-binding #`(is (any magic) 128))
  (unless (constant-binding? $binding) (error "not a constant binding"))
  (check-equal? (constant-binding-symbol $binding) `magic)
  (check-equal? (constant-binding-type $binding) number-type))

(bind
  $binding (compile-binding #`(is (any (giving magic number)) 128))
  (unless (constant-binding? $binding) (error "not a constant binding"))
  (check-equal? (constant-binding-symbol $binding) `magic)
  (check-equal? (constant-binding-type $binding) number-type))

(check-equal?
  (compile-typed #`(as (racket foo) number))
  (typed `foo number-type))

; TODO: Fix the test, generated number2 and string3 are not guaranteed.
; (check-equal?
;   (compile-typed #`(doing number string number))
;   (typed 
;     `(lambda (number2 string3) number2)
;     (giving
;       (list number-type string-type)
;       (list number-type))))
