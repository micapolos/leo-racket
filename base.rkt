#lang racket/base

(provide (all-defined-out))

(require
  leo/testing
  (for-syntax 
    racket/base
    racket/list))

; --------------------------------------------------------

(define-syntax (bind $syntax)
  (syntax-case $syntax ()
    ((_ expr name body ...)
      #`(let ((name expr)) body ...))))

(check-equal? 
  (bind (string-append "a" "b") x (string-append x x)) 
  "abab")

; ---------------------------------------------------------

(define-syntax (switch $syntax)
  (syntax-case $syntax ()
    ((_ $expr $body ...)
      (let* (($cases (syntax-e #`($body ...)))
             ($length (length $cases))
             ($last-index (sub1 $length)))
        #`(case $expr
          #,@(map
            (lambda ($index $case) 
              (list 
                (if (= $index $last-index) `else (list $index))
                $case))
            (range (length $cases))
            $cases))))))

(check-equal? (switch 0) (void))

(check-equal? (switch 0 "zero") "zero")
(check-equal? (switch 1 "zero") "zero")

(check-equal? (switch 0 "zero" "one") "zero")
(check-equal? (switch 1 "zero" "one") "one")
(check-equal? (switch 2 "zero" "one") "one")

(check-equal? (switch 0 "zero" "one" "two") "zero")
(check-equal? (switch 1 "zero" "one" "two") "one")
(check-equal? (switch 2 "zero" "one" "two") "two")
(check-equal? (switch 3 "zero" "one" "two") "two")
