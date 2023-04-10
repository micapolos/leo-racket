#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/base
  (for-syntax racket/base))

(define-type (Maybe A) (Option (Just A)))

(struct (A) just ((value : A))
  #:transparent
  #:type-name Just)

(define-syntax (maybe-bind $syntax)
  (syntax-case $syntax ()
    ((_ arg expr body ...)
      (let (($maybe (car (generate-temporaries `(maybe)))))
        #`(let ((#,$maybe expr))
          (and
            (just? #,$maybe)
            (let ((arg (just-value #,$maybe))) body ...)))))))

 (define-syntax (maybe-map $syntax)
  (syntax-case $syntax ()
    ((_ arg expr body ...)
      (let (($maybe (car (generate-temporaries `(maybe)))))
        #`(let ((#,$maybe expr))
          (and
            (just? #,$maybe)
            (just (let ((arg (just-value #,$maybe))) body ...))))))))

(define-syntax (maybe-or $syntax)
  (syntax-case $syntax ()
    ((_ expr body ...)
      (let (($maybe (car (generate-temporaries `(maybe)))))
        #`(let ((#,$maybe expr))
          (cond
            ((just? #,$maybe) (just-value #,$maybe))
            (else body ...)))))))

(check-equal?
  (maybe-bind $number (just 123) (just (+ $number 1)))
  (just 124))

(check-equal?
  (maybe-bind $number #f (just (+ $number 1)))
  #f)

(check-equal?
  (maybe-map $number (just 123) (+ $number 1))
  (just 124))

(check-equal?
  (maybe-map $number #f (+ $number 1))
  #f)

(check-equal?
  (maybe-or (just 123) "foo")
  123)

(check-equal?
  (maybe-or #f "foo")
  "foo")

(define #:forall (V) (lift-maybe-list ($maybe-list : (Listof (Maybe V)))) : (Maybe (Listof V))
  (define $just-list (filter-false $maybe-list))
  (and
    (= (length $just-list) (length $maybe-list))
    (just (map (ann just-value (-> (Just V) V)) $just-list))))

(check-equal?
  (lift-maybe-list null)
  (just null))

(check-equal?
  (lift-maybe-list (list (just 1) (just 2) (just 3)))
  (just (list 1 2 3)))

(check-equal?
  (lift-maybe-list (list (just 1) #f (just 3)))
  #f)
