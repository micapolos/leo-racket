#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/typed/position)

(data (V) positioned
  (value : V)
  (position : Position))

(define #:forall (V) (start-positioned ($value : V))
  (positioned $value start-position))

(define #:forall (I O)
  (positioned-map
    ($positioned : (Positioned I))
    ($char : Char)
    ($value-fn : (-> I Position O)))
  : (Positioned O)
  (bind $position (positioned-position $positioned)
    (positioned
      ($value-fn (positioned-value $positioned) $position)
      (position-plus-char $position $char))))

(check-equal?
  (positioned-map
    (positioned "foo" (position 3 8))
    #\space
    (lambda (($string : String) ($position : Position))
      (pair $string $position)))
  (positioned
    (pair "foo" (position 3 8))
    (position 3 9)))
