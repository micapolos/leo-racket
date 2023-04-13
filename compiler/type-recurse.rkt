#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/type-set)

(define (type-recurse ($type : Type)) : Type
  (cond
    ((recursive? $type)
      (type-set (recursive-type $type) (variable 0) $type))
    (else $type)))

(check-equal?
  (type-recurse (variable 0))
  (variable 0))

(check-equal?
  (type-recurse
    (recursive
      (field! `rec
        (choice!
          (field! `next (variable 0))
          (field! `end)))))
  (field! `rec
    (choice!
      (field! `next
        (recursive
          (field! `rec
            (choice!
              (field! `next (variable 0))
              (field! `end)))))
      (field! `end))))
