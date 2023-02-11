#lang racket/base

(provide parse)

(require 
  syntax/parse
  racket/function
  rackunit)

(define string-type `string)
(define number-type `number)
(define (id-type id) `(id ,id))
(define (fn-type name . args) `(fn ,name ,@args))

; -------------------------------------------------------------

(define empty-env null)

(define (env-set $env $key $value)
  (cons (cons $key $value) $env))

(define (env-ref $env $key)
  (define entry (assoc $key $env))
  (cond
    ((pair? entry) (cdr entry))
    (else (error (format "No such key: ~a" $key)))))

(check-equal? 
  (env-ref 
    (env-set 
      (env-set empty-env `k1 `v1) 
      `k2 `v2) 
    `k1) 
  `v1)

(check-equal? 
  (env-ref 
    (env-set 
      (env-set empty-env `k1 `v1) 
      `k2 `v2) 
    `k2) 
  `v2)

(check-equal? 
  (env-ref 
    (env-set 
      (env-set empty-env `k1 `v1) 
      `k1 `v2) 
    `k1) 
  `v2)

; -------------------------------------------------------------

(struct parsed (env rev-stxs) #:transparent)

(define empty-parsed (parsed null null))

(define (parsed-stxs $parsed) 
  (reverse (parsed-rev-stxs $parsed)))

(define (env-stx $env $stx)
  (syntax-parse $stx
    (a:string (stx-typed #`a `string))
    (a:number (stx-typed #`a `number))
    ((id:identifier args ...)
      (let* ((parsed-args (map parse-stx (syntax-e #`(args ...))))
             (type (cons (syntax-e #`id) (map stx-type parsed-args)))
             (entry (hash-ref env type))
             (ident (car entry))
             (ret-type (cdr entry)))
        (stx-typed #`(#,ident #,@parsed-args) ret-type)))
    (id:identifier 
      (let* ((symbol (syntax-e #`id))
             (type (hash-ref env symbol)))
        (stx-typed #`id type)))))

(define (parsed-plus $parsed $stx)
  (syntax-parse $stx
    (((~literal def) (id:identifier body:expr))
      (let* ((symbol (syntax-e #`id))
             (parsed-body (parse-stx #`body))
             (body-type (stx-type parsed-body)))
        (hash-set! env symbol body-type)
        #`(define #,(stx-typed #`id body-type) #,parsed-body)))
    (((~literal does) (id:identifier param:identifier ...) body:expr) 
      (let* ((type (cons (syntax-e #`id) (map syntax-e (syntax-e #`(param ...)))))
             (parsed-body (parse-stx #`body))
             (body-type (stx-type parsed-body)))
        (hash-set! env type (cons (syntax-e #`id) body-type))
        #`(define (id param ...) #,parsed-body)))))

; -------------------------------------------------------------

(define env 
  (hash-copy
    (hash 
      `(append string string) `(string-append . string)
      `(plus number number) `(+ . number)
      `(string number) `(number->string . string)
      `(length string) `(string-length . number))))

(define (parse-stx $stx) 
  (syntax-parse $stx
    (a:string (stx-typed #`a `string))
    (a:number (stx-typed #`a `number))
    (((~literal def) (id:identifier body:expr))
      (let* ((symbol (syntax-e #`id))
             (parsed-body (parse-stx #`body))
             (body-type (stx-type parsed-body)))
        (hash-set! env symbol body-type)
        #`(define #,(stx-typed #`id body-type) #,parsed-body)))
    (((~literal does) (id:identifier param:identifier ...) body:expr) 
      (let* ((type (cons (syntax-e #`id) (map syntax-e (syntax-e #`(param ...)))))
             (parsed-body (parse-stx #`body))
             (body-type (stx-type parsed-body)))
        (hash-set! env type (cons (syntax-e #`id) body-type))
        #`(define (id param ...) #,parsed-body)))
    ((id:identifier args ...)
      (let* ((parsed-args (map parse-stx (syntax-e #`(args ...))))
             (type (cons (syntax-e #`id) (map stx-type parsed-args)))
             (entry (hash-ref env type))
             (ident (car entry))
             (ret-type (cdr entry)))
        (stx-typed #`(#,ident #,@parsed-args) ret-type)))
    (id:identifier 
      (let* ((symbol (syntax-e #`id))
             (type (hash-ref env symbol)))
        (stx-typed #`id type)))))

(define (parse $stxs)
  (map parse-stx $stxs))

(define (stx-typed $stx $type)
  (syntax-property $stx `leo-type $type))

(define (stx-type $stx)
  (or
    (syntax-property $stx `leo-type)
    (error (format "Untyped ~a" $stx))))
