#lang leo/typed

(require
  leo/compiler/literal-parser
  leo/compiler/sentence-parser)

(define sexp-env : (Env (Stackof Sexp) (Stackof Sexp))
  (env
    (lambda
      (
        ($sexp-stack : (Stackof Sexp))
        ($symbol : Symbol)
        ($fn : (->
          (Stackof Sexp)
          (-> (Stackof Sexp) (Parser (Stackof Sexp)))
          (-> (Stackof Sexp) (Stackof Sexp))
          (Parser (Stackof Sexp)))))
      : (Parser (Stackof Sexp))
      (#%app $fn null
        (lambda (($inner-sexp-stack : (Stackof Sexp))) : (Parser (Stackof Sexp))
          (plus-sexp-line-parser $inner-sexp-stack))
        (lambda (($inner-sexp-stack : (Stackof Sexp))) : (Stackof Sexp)
          (push $sexp-stack
            (sexp-stack-sexp $symbol $inner-sexp-stack)))))))

(define (plus-sexp-line-parser ($sexp-stack : (Stackof Sexp))) : (Parser (Stackof Sexp))
  (parser-or
    (parser-map literal-parser
      (lambda (($literal : Literal))
        (push $sexp-stack (literal-sexp $literal))))
    (env-sentence-parser sexp-env $sexp-stack)))

(define sexp-stack-parser : (Parser (Stackof Sexp))
  (env-script-parser sexp-env null
    (lambda (($sexp-stack : (Stackof Sexp)))
      (plus-sexp-line-parser $sexp-stack))))

(define (sexp-stack-sexp ($symbol : Symbol) ($sexp-stack : (Stackof Sexp))) : Sexp
  (cond
    ((null? $sexp-stack) $symbol)
    (else `(,$symbol ,@(reverse $sexp-stack)))))

(define (parse-sexp-list ($string : String)) : (U (Listof Sexp) (Failure Any))
  (bind $result (parse sexp-stack-parser $string)
    (cond
      ((failure? $result) $result)
      (else (reverse $result)))))

(check-equal? (parse-sexp-list "") `())
(check-equal? (parse-sexp-list "foo\n") `(foo))
(check-equal? (parse-sexp-list "foo bar\n") `((foo bar)))
(check-equal? (parse-sexp-list "foo\nbar\n") `(foo bar))

(check-equal? (parse-sexp-list "0, -1, 2, \"foo\"\n") `(0 -1 2 "foo"))
