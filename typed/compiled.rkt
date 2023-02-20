#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/option
  leo/typed/type-utils
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/type-generate-temporary
  leo/typed/binding
  leo/typed/syntax-match
  leo/typed/syntax-typed
  leo/typed/syntax-resolve
  leo/typed/type-parse
  leo/typed/syntax-type
  leo/typed/testing)

(struct compiled 
  (
    (binding-list : (Listof Binding))
    (syntax-list : (Listof Syntax)))
  #:transparent
  #:type-name Compiled)

(define null-compiled (compiled null null))

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
      ((boolean? $syntax-e)
        (compiled-plus-syntax 
          $compiled
          (syntax-with-type $syntax boolean-type)))
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
      (else
        (or
          (let (($do-syntax (binding-list-parse-do $binding-list $syntax)))
            (and 
              $do-syntax
              (compiled-plus-syntax $compiled $do-syntax)))
          (let (($doing-syntax (binding-list-parse-doing $binding-list $syntax)))
            (and 
              $doing-syntax
              (compiled-plus-syntax $compiled $doing-syntax)))
          (let (($of-syntax (syntax-parse-of $syntax)))
            (and 
              $of-syntax
              (compiled-plus-syntax $compiled $of-syntax)))
          (compiled-parse-define $compiled $syntax)
          (compiled-parse-bind $compiled $syntax)
          (compiled-parse-require $compiled $syntax)
          (cond
            ((syntax-identifier-args? $syntax)
              (define $identifier (car $syntax-e))
              (define $symbol (syntax-e $identifier))
              (define $args (cdr $syntax-e))
              (cond 
                ((equal? $symbol `function)
                  (error "TODO: function"))
                (else
                  (compiled-plus-syntax
                    $compiled
                    (binding-list-apply-symbol-args
                      $binding-list
                      $symbol
                      (map (curry binding-list-syntax $binding-list) $args))))))
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

(define
  (binding-list-syntax
    ($binding-list : (Listof Binding))
    ($syntax : Syntax)) : Syntax
  (compiled-syntax
    (compiled-parse-syntax
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
    (let* (($syntax-e (syntax-e $syntax))
           ($expr-syntax (cadr $syntax-e))
           ($body-syntax (caddr $syntax-e))
           ($expr-typed-syntax (binding-list-syntax $binding-list $expr-syntax))
           ($expr-type (syntax-type $expr-typed-syntax))
           ($tmp-syntax (type-generate-temporary $expr-type))
           ($body-binding-list (cons (argument-binding $expr-type $tmp-syntax) $binding-list))
           ($body-typed-syntax (binding-list-syntax $body-binding-list $body-syntax))
           ($body-type (syntax-type $body-typed-syntax)))
      (syntax-with-type
        (datum->syntax $syntax 
          (list `let 
            (list (list $tmp-syntax $expr-typed-syntax))
            $body-typed-syntax))
        $body-type))))

(check-equal?
  (option-map
    (binding-list-parse-do
      null
      #`(do 1 number))
    syntax-typed-datum)
  ; TODO: Fix the test, generated number1 is not guaranteed.
  (typed `(let ((number1 1)) number1) number-type))

; ---------------------------------------------------------------

(define (syntax-parse-of ($syntax : Syntax)) : (Option Syntax)
  (cond
    ((syntax-symbol-arg-arg? $syntax `of)
      (define $syntax-e (syntax-e $syntax))
      (define $native-syntax (cadr $syntax-e))
      (define $type-syntax (caddr $syntax-e))
      (define $type (syntax-parse-type $type-syntax))
      (syntax-with-type $native-syntax $type))
    (else #f)))

(check-equal?
  (option-map
    (syntax-parse-of #`(of pi number))
    syntax-typed-datum)
  (typed `pi number-type))

; ---------------------------------------------------------------

(define
  (compiled-parse-define
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (define $binding-list (compiled-binding-list $compiled))
  (cond
    ((syntax-symbol-arg? $syntax `define)
      (define $arg (cadr (syntax-e $syntax)))
      (cond
        ((or 
          (syntax-symbol-arg-arg? $arg `is) 
          (syntax-symbol-arg-arg? $arg `:=))
          (define $is-args (cdr (syntax-e $arg)))
          (define $is-lhs (car $is-args))
          (define $is-rhs (cadr $is-args))
          (define-values
            ($type $return-type)
            (cond
              ((or 
                (syntax-symbol-arg-arg? $is-lhs `being) 
                (syntax-symbol-arg-arg? $is-lhs `:))
                (define $being-args (cdr (syntax-e $is-lhs)))
                (define $being-lhs (car $being-args))
                (define $being-rhs (cadr $being-args))
                (values 
                  (syntax-parse-type $being-lhs) 
                  (syntax-parse-type $being-rhs)))
              (else 
                (values 
                  (syntax-parse-type $is-lhs) 
                  #f))))
          (define $body $is-rhs)
          (cond 
            ((symbol-type? $type)
              (define $symbol (symbol-type-symbol $type))
              (cond 
                ((syntax-symbol-arg? $body `native)
                  (unless $return-type (error "native requires type"))
                  (define $native-args (cdr (syntax-e $body)))
                  (define $native-body (car $native-args))
                  (unless (identifier? $native-body)
                    (error "native must be identifier"))
                  (define $native-type $return-type) ; the is a lie
                  (define $binding 
                    (constant-binding $symbol $return-type $native-body))
                  (compiled-plus-binding $compiled $binding))
                (else 
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
                  (define $tmp (type-generate-temporary $type))
                  (define $binding
                    (constant-binding $symbol $body-return-type $tmp))
                  (define $compiled-syntax 
                    (datum->syntax #f `(define ,$tmp ,$typed-body)))
                  (compiled-plus-syntax
                    (compiled-plus-binding $compiled $binding)
                    $compiled-syntax))))
            (else (error (format "not a symbol type: ~a" $type)))))
        ((or 
          (syntax-symbol-arg-arg? $arg `does) 
          (syntax-symbol-arg-arg? $arg `lambda))
          (define $does-args (cdr (syntax-e $arg)))
          (define $does-lhs (car $does-args))
          (define $does-rhs (cadr $does-args))
          (define-values
            ($type $return-type)
            (cond
              ((or 
                (syntax-symbol-arg-arg? $does-lhs `giving) 
                (syntax-symbol-arg-arg? $does-lhs `->))
                (define $giving-args (cdr (syntax-e $does-lhs)))
                (define $giving-lhs (car $giving-args))
                (define $giving-rhs (cadr $giving-args))
                (values 
                  (syntax-parse-type $giving-lhs) 
                  (syntax-parse-type $giving-rhs)))
              (else 
                (values 
                  (syntax-parse-type $does-lhs) 
                  #f))))
          (define $body $does-rhs)
          (cond 
            ((and (field-type? $type) (struct-type-body? (field-type-body $type)))
              (define $symbol (field-type-symbol $type))
              (define $arg-types (struct-type-body-type-list (field-type-body $type)))
              (define $dynamic-arg-types (filter type-is-dynamic? $arg-types))
              (define $arg-tmps
                (map type-generate-temporary $dynamic-arg-types))
              (define $argument-binding
                (argument-binding
                  $type
                  (symbol-args-make
                    $symbol 
                    (map syntax-with-type $arg-tmps $dynamic-arg-types))))
              (cond 
                ((syntax-symbol-arg? $body `native)
                  (unless $return-type (error "native requires type"))
                  (define $native-args (cdr (syntax-e $body)))
                  (define $native-body (car $native-args))
                  (unless (identifier? $native-body)
                    (error "native must be identifier"))
                  (define $native-type $return-type) ; the is a lie
                  (define $binding 
                    (function-binding $symbol $arg-types $return-type $native-body))
                  (compiled-plus-binding $compiled $binding))
                (else 
                  (define $body-binding-list (cons $argument-binding $binding-list))
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
                  (define $fn (type-generate-temporary $type))
                  (define $binding 
                    (function-binding $symbol $arg-types $body-return-type $fn))
                  (define $compiled-syntax 
                    (datum->syntax #f `(define (,$fn ,@$arg-tmps) ,$typed-body)))
                  (compiled-plus-syntax
                    (compiled-plus-binding $compiled $binding)
                    $compiled-syntax))))
            (else #f)))
        (else 
          (define $value (binding-list-syntax $binding-list $arg))
          (define $type (syntax-type $value))
          (define $tmp (type-generate-temporary $type))
          (compiled-plus-syntax
            (compiled-plus-binding
              $compiled
              (argument-binding $type $tmp))
            (datum->syntax #f `(define ,$tmp ,$value))))))
    (else #f)))

(define 
  (compiled-parse-bind
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (define $binding-list (compiled-binding-list $compiled))
  (cond
    ((syntax-symbol-arg? $syntax `bind)
      (define $arg (cadr (syntax-e $syntax)))
      (define $value (binding-list-syntax $binding-list $arg))
      (define $type (syntax-type $value))
      (define $tmp (type-generate-temporary $type))
      (compiled-plus-syntax
        (compiled-plus-binding
          $compiled
          (argument-binding $type $tmp))
        (datum->syntax #f `(define ,$tmp ,$value))))
    (else #f)))

; --------------------------------------------------------------------

(define 
  (compiled-parse-require
    ($compiled : Compiled)
    ($syntax : Syntax))
  : (Option Compiled)
  (cond
    ((syntax-symbol-args? $syntax `require)
      (compiled-plus-syntax $compiled $syntax))
    (else #f)))

; --------------------------------------------------------------------

(define
  (binding-list-parse-doing
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : (Option Syntax)
  (cond
    ((or (syntax-symbol-arg-arg? $syntax `doing) (syntax-symbol-arg-arg? $syntax `lambda))
      (define $doing-args (cdr (syntax-e $syntax)))
      (define $doing-lhs (car $doing-args))
      (define $doing-rhs (cadr $doing-args))
      (define-values
        ($type $return-type)
        (cond
          ((or 
            (syntax-symbol-arg-arg? $doing-lhs `giving) 
            (syntax-symbol-arg-arg? $doing-lhs `->))
            (define $giving-args (cdr (syntax-e $doing-lhs)))
            (define $giving-lhs (car $giving-args))
            (define $giving-rhs (cadr $giving-args))
            (values 
              (syntax-parse-type $giving-lhs) 
              (syntax-parse-type $giving-rhs)))
          (else 
            (values 
              (syntax-parse-type $doing-lhs) 
              #f))))
      (define $body $doing-rhs)
      (cond 
        ((and (field-type? $type) (struct-type-body? (field-type-body $type)))
          (define $symbol (field-type-symbol $type))
          (define $arg-types (struct-type-body-type-list (field-type-body $type)))
          (define $dynamic-arg-types (filter type-is-dynamic? $arg-types))
          (define $arg-tmps
            (map type-generate-temporary $dynamic-arg-types))
          (define $argument-binding
            (argument-binding
              $type
              (symbol-args-make
                $symbol 
                (map syntax-with-type $arg-tmps $dynamic-arg-types))))
          (cond 
            ((syntax-symbol-arg? $body `native)
              (unless $return-type (error "native requires type"))
              (define $native-args (cdr (syntax-e $body)))
              (define $native-body (car $native-args))
              (unless (identifier? $native-body)
                (error "native must be identifier"))
              (define $native-type $return-type) ; the is a lie
              (syntax-with-type
                $native-body
                (arrow-type 
                  (list (field-type $symbol (struct-type-body $arg-types)))
                  (list $return-type))))
            (else 
              (define $body-binding-list (cons $argument-binding $binding-list))
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
                (arrow-type 
                  (list (field-type $symbol (struct-type-body $arg-types)))
                  (list $body-return-type))))))
        (else #f)))
    (else #f)))

; ----------------------------------------------------------------------

(check-equal?
  (compile-typed #`#f)
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
  (compile-typed #`(doing (plus number string) (number plus)))
  (typed 
    `(lambda (number2 string3) number2)
    (arrow-type
      (list (field-type `plus (struct-type-body (list number-type string-type))))
      (list number-type))))
