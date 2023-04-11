#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/failure
  leo/typed/testing
  (for-syntax racket/base))

(data (V) success (value : V))

(data (V E) result
  (value : (U (Success V) (Failure E))))

; ------------------------------------------------------------------------

(define-syntax (result-app $syntax)
  (syntax-case $syntax ()
    ((_ $fn $result)
      (let (($tmp (car (generate-temporaries `(result)))))
        #`(bind $value (result-value $result)
          (cond
            ((success? $value) (result (success ($fn (success-value $value)))))
            ((failure? $value) (result $value))))))))

(check-equal?
  (result-app string-length (result (success "foo")))
  (result (success 3)))

(check-equal?
  (result-app string-length (result (failure "dupa")))
  (result (failure "dupa")))
