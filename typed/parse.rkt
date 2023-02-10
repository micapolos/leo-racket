#lang racket/base

(provide parse)

(require 
  syntax/parse
  racket/function)

(define number--plus-number--- +)

(define env 
  (hash 
    `(append string string) `(string-append . string)
    `(plus number number) `(+ . number)
    `(string number) `(number->string . string)
    `(length string) `(string-length . number)))

(define (parse-stx $stx) 
  (syntax-parse $stx
    (a:string (stx-typed #`a `string))
    (a:number (stx-typed #`a `number))
    ((id:identifier args ...)
      (let* ((parsed-args (map parse-stx (syntax-e #`(args ...))))
             (type (cons (syntax-e #`id) (map stx-type parsed-args)))
             (entry (hash-ref env type))
             (ident (car entry))
             (ret-type (cdr entry)))
        (stx-typed #`(#,ident #,@parsed-args) ret-type)))))

(define (parse $stxs)
  (map parse-stx $stxs))

(define (stx-typed $stx $type)
  (syntax-property $stx `leo-type $type))

(define (stx-type $stx)
  (syntax-property $stx `leo-type))
