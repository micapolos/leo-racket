#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-match
  leo/typed/testing)

(define (syntax-parse-type ($syntax : Syntax)) : Type
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((equal? $syntax-e `boolean) boolean-type)
      ((equal? $syntax-e `string) string-type)
      ((equal? $syntax-e `number) number-type)
      ((equal? $syntax-e `fixnum) fixnum-type)
      ((equal? $syntax-e `flonum) flonum-type)
      ((symbol? $syntax-e) (symbol-type $syntax-e))
      (else
        (or
          (syntax-parse-arrow-type $syntax)
          (cond
            ((list? $syntax-e) (syntaxes-parse-type $syntax-e))
            (else (error (format "type parse error ~v" $syntax)))))))))

(define (syntax-parse-arrow-type ($syntax : Syntax)) : (Option Type)
  (syntax-symbol-match-args-arg $syntax `giving args arg
    (arrow-type
      (map syntax-parse-type args)
      (list (syntax-parse-type arg)))))

(define (syntax-parse-types ($syntax : Syntax)) : (Listof Type)
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((list? $syntax-e) (syntaxes-parse-types $syntax-e))
      (else (list (syntax-parse-type $syntax))))))

(define (syntaxes-parse-types ($syntaxes : (Listof Syntax))) : (Listof Type)
  (map syntax-parse-type $syntaxes))

(define (syntaxes-parse-type ($syntaxes : (Listof Syntax))) : Type
  (cond
    ((null? $syntaxes)
      (error (format "invalid type ~v" $syntaxes)))
    ((identifier? (car $syntaxes))
      (identifier-syntaxes-parse-type (car $syntaxes) (cdr $syntaxes)))
    (else (error (format "invalid type ~a" $syntaxes)))))

; TODO: Fix!!!
(define (syntaxes-parse-type-body-option
  ($syntaxes : (Listof Syntax))) : (Option TypeBody)
  (cond
    ((null? $syntaxes) #f)
    ((identifier? (car $syntaxes))
      (identifier-syntaxes-parse-type-body-option (car $syntaxes) (cdr $syntaxes)))
    (else #f)))

(define (identifier-syntaxes-parse-type 
  ($identifier : Identifier)
  ($syntaxes : (Listof Syntax))) : Type
  (let (($symbol (syntax-e $identifier)))
    (cond
      ((equal? $symbol `identifier) 
        (error "TODO: escaping"))
      (else 
        (or
          (option-bind (syntaxes-parse-type-body-option $syntaxes) $type-body
            (field-type $symbol $type-body))
          (field-type $symbol (syntaxes-parse-struct-type-body $syntaxes)))))))

(define (identifier-syntaxes-parse-type-body-option
  ($identifier : Identifier)
  ($syntaxes : (Listof Syntax))) : (Option TypeBody)
  (let (($symbol (syntax-e $identifier)))
    (cond
      ((equal? $symbol `choice) 
        (syntaxes-parse-choice-type-body $syntaxes))
      (else #f))))

(define (syntaxes-parse-struct-type-body ($syntaxes : (Listof Syntax))) : TypeBody
  (struct-type-body (map syntax-parse-type $syntaxes)))

(define (syntaxes-parse-choice-type-body ($syntaxes : (Listof Syntax))) : TypeBody
  (choice-type-body (map syntax-parse-type $syntaxes)))

(check-equal? (syntax-parse-type #`boolean) boolean-type)
(check-equal? (syntax-parse-type #`number) number-type)
(check-equal? (syntax-parse-type #`fixnum) fixnum-type)
(check-equal? (syntax-parse-type #`flonum) flonum-type)
(check-equal? (syntax-parse-type #`string) string-type)
(check-equal? (syntax-parse-type #`foo) (symbol-type `foo))

(check-equal? 
  (syntax-parse-type #`(foo boolean number string)) 
  (field-type `foo 
    (struct-type-body (list boolean-type number-type string-type))))

(check-equal?
  (syntax-parse-type #`(id (number number) (string string)))
  (field-type `id 
    (struct-type-body
      (list 
        (field-type `number (struct-type-body (list number-type)))
        (field-type `string (struct-type-body (list string-type)))))))

(check-equal?
  (syntax-parse-type #`(giving number string boolean))
  (arrow-type (list number-type string-type) (list boolean-type)))

; (check-equal?
;   (syntax-parse-type #`(foo (choice boolean number string)))
;   (field-type `foo 
;     (choice-type-body (list boolean-type number-type string-type))))
