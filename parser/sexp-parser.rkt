#lang leo/typed

(require
  leo/parser/literal-parser
  leo/parser/sentence-parser)

(define sexp-env : (Env (Stackof Sexp))
  (env
    (lambda (($sexp-stack : (Stackof Sexp)))
      (parser-map literal-parser
        (lambda (($literal : Literal))
          (push $sexp-stack (literal-sexp $literal)))))
    (lambda
      (
        ($sexp-stack : (Stackof Sexp))
        ($symbol : Symbol)
        ($fn : (->
          (Stackof Sexp)
          (-> (Stackof Sexp) (Stackof Sexp))
          (Parser (Stackof Sexp)))))
      : (Parser (Stackof Sexp))
      (#%app $fn null
        (lambda (($rhs : (Stackof Sexp))) : (Stackof Sexp)
          (push $sexp-stack
            (cond
              ((null? $rhs) $symbol)
              (else `(,$symbol ,@(reverse $rhs))))))))))

(define (parse-sexp-list ($string : String)) : (U (Stackof Sexp) (Failure Any))
  (parse
    (parser-map
      (env-script-parser sexp-env null)
      (lambda (($sexp-stack : (Stackof Sexp)))
        (reverse $sexp-stack)))
    $string))

(check-equal? (parse-sexp-list "") `())

(check-equal? (parse-sexp-list "foo\n") `(foo))
(check-equal? (parse-sexp-list "123\n") `(123))
(check-equal? (parse-sexp-list "\"foo\"\n") `("foo"))

(check-equal? (parse-sexp-list "foo 123\n") `((foo 123)))

(check-equal? (parse-sexp-list "foo\n  123\n") `((foo 123)))
(check-equal? (parse-sexp-list "foo\n  123\n  456\n") `((foo 123 456)))

(check-equal? (parse-sexp-list "foo: 123\n") `((foo 123)))
(check-equal? (parse-sexp-list "foo: 123, 456\n") `((foo 123 456)))

(check-equal? (parse-sexp-list "foo()\n") `(foo))
(check-equal? (parse-sexp-list "foo(123)\n") `((foo 123)))
(check-equal? (parse-sexp-list "foo(123, 456)\n") `((foo 123 456)))

(check-equal? (parse-sexp-list "foo")  (failure! parse-incomplete (at (position 1 4))))
(check-equal? (parse-sexp-list "foo:")  (failure! parse-incomplete (at (position 1 5))))
(check-equal? (parse-sexp-list "foo: ")  (failure! parse-incomplete (at (position 1 6))))
(check-equal? (parse-sexp-list "foo(")  (failure! parse-incomplete (at (position 1 5))))
(check-equal? (parse-sexp-list "foo.")  (failure! parse-incomplete (at (position 1 5))))
(check-equal? (parse-sexp-list "foo,") (failure! parse-incomplete (at (position 1 5))))
(check-equal? (parse-sexp-list "foo, ") (failure! parse-incomplete (at (position 1 6))))

(check-equal? (parse-sexp-list "Foo") (failure! parse-complete (at (position 1 2))))
(check-equal? (parse-sexp-list "fOo") (failure! parse-complete (at (position 1 3))))
(check-equal? (parse-sexp-list "foO") (failure! parse-incomplete (at (position 1 4))))
(check-equal? (parse-sexp-list "fo1") (failure! parse-incomplete (at (position 1 4))))

(check-equal? (parse-sexp-list "\n") null)

(check-equal? (parse-sexp-list "foo\n") `(foo))

(check-equal? (parse-sexp-list "foo\nbar\n") `(foo bar))
(check-equal? (parse-sexp-list "foo, bar\n") `(foo bar))

(check-equal? (parse-sexp-list "foo\nbar, goo\n") `(foo bar goo))
(check-equal? (parse-sexp-list "foo, bar\ngoo\n") `(foo bar goo))

(check-equal? (parse-sexp-list "\nfoo, bar\n\ngoo\n\n") `(foo bar goo))

(check-equal?
  (parse-sexp-list "foo\nbar\ngoo 123\nzar\n  123\n  456\n")
  `(foo bar (goo 123) (zar 123 456)))
