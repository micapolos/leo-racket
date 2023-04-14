#lang leo/typed

(require
  leo/compiler/type)

(data value
  (any : Any)
  (type : Type))

(define-type Packet (Stackof Value))

(define packet : (-> Value * Packet) stack)

(define null-packet (packet))
