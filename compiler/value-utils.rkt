#lang typed/racket/base

(provide (all-defined-out))

(require
  typed/racket/unsafe
  racket/list
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-match)

(unsafe-require/typed leo/compiler/any-utils
  (any-apply (-> Any (Listof Any) (Listof Any))))

(define (value-dynamic? ($value : Value)) : Boolean
  (type-dynamic? (value-type $value)))

(define (packet-apply 
  ($lhs : Packet)
  ($rhs : Packet)) : Packet
  (or (packet-resolve $lhs $rhs) $rhs))

(define (packet-resolve 
  ($lhs : Packet)
  ($rhs : Packet)) : (Option Packet)
  (cond
    ((null? $lhs) #f)
    (else 
      (or 
        (value-resolve (top $lhs) $rhs)
        (packet-resolve (pop $rhs) $rhs)))))

(define (value-resolve 
  ($lhs : Value)
  ($rhs : Packet)) : (Option Packet)
  (arrow-value-resolve $lhs $rhs))

(define (arrow-value-resolve 
  ($lhs : Value)
  ($rhs : Packet)) : (Option Packet)
  (define $lhs-type (value-type $lhs))
  (cond
    ((arrow? $lhs-type)
      (define $arrow $lhs-type)
      (define $lhs-any (value-any $lhs))
      (define $rhs-structure (map value-type $rhs))
      (define $arrow-from-structure (arrow-from-structure $arrow))
      (define $arrow-to-structure (arrow-to-structure $arrow))
      (and 
        (structure-matches? $rhs-structure $arrow-from-structure)
        (let ()
          (define $rhs-any-stack (map value-any (filter-false (filter value-dynamic? $rhs))))
          (define $result-stack (reverse (any-apply $lhs-any (reverse $rhs-any-stack))))
          (map
            (lambda (($index : (Option Exact-Nonnegative-Integer)) ($type : Type))
              (value
                (cond
                  ($index (stack-ref $result-stack $index))
                  (else `()))
                $type))
            (range (length $arrow-to-structure))
            $arrow-to-structure))))
    ((and (single $rhs) (type-matches-selector? $lhs-type (value-type (top $rhs))))
      (packet $lhs))
    (else #f)))

(check-equal?
  (value-resolve
    (value 128 (field! `foo number-type))
    (packet (value `() (field! `foo))))
  (packet
    (value 128 (field! `foo number-type))))

(check-equal?
  (value-resolve
    (value 
      (lambda () 128)
      (arrow 
        (structure (field! `foo))
        (structure number-type)))
    (packet
      (value `() (field! `foo))))
  (packet
    (value 128 number-type)))
