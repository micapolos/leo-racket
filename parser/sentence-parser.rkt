#lang leo/typed

(require
  leo/parser/literal-parser)

(data (V) env
  (plus-atom-parser-fn : (-> V (Parser V)))
  (begin-parser-fn : (-> V Symbol (Parser V)))
  (end-parser-fn : (-> V V (Parser V))))

(: env-plus-atom-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-plus-atom-parser $env $value)
  (#%app (env-plus-atom-parser-fn $env) $value))

(: env-begin-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-begin-parser $env $value $symbol)
  (#%app (env-begin-parser-fn $env) $value $symbol))

(: env-end-parser : (All (V) (-> (Env V) V V (Parser V))))
(define (env-end-parser $env $value $rhs)
  (#%app (env-end-parser-fn $env) $value $rhs))

; --------------------------------------------------------------------------------------

(define comma-separator-parser
  (exact-string-parser ", "))

(define newlines-separator-parser
  newlines-parser)

(define comma-or-newlines-separator-parser
  (first-parser
    comma-separator-parser
    newlines-separator-parser))

(: env-plus-script-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-plus-script-parser $env $value)
  (prefix-parser maybe-newlines-parser
    (value-or-parser $value
      (parser-suffix
        (repeat-separated-parser $value comma-or-newlines-separator-parser
          (lambda (($value : V))
            (env-plus-line-parser $env $value)))
        newlines-parser))))

(: env-plus-line-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-plus-line-parser $env $value)
  (first-parser
    (env-plus-atom-line-parser $env $value)
    (env-plus-sentence-parser $env $value)))

(: env-plus-atom-line-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-plus-atom-line-parser $env $value)
  (parser-bind
    (env-plus-atom-parser $env $value)
    (lambda (($atom : V))
      (value-or-parser $atom
        (prefix-parser (exact-char-parser #\.)
          (env-plus-line-parser $env $atom))))))

(: env-plus-sentence-parser : (All (V) (-> (Env V) V (Parser V))))
(define (env-plus-sentence-parser $env $value)
  (parser-bind word-parser
    (lambda (($word : Word))
      (first-parser
        (parser-bind (exact-char-parser #\.)
          (lambda ((_ : True))
            (parser-bind
              (env-begin-parser $env $value (word-symbol $word))
              (lambda (($rhs : V))
                (parser-bind
                  (env-end-parser $env $value $rhs)
                  (lambda (($dotted : V))
                    (env-plus-line-parser $env $dotted)))))))
        (parser-bind
          (env-plus-rhs-parser $env $value (word-symbol $word))
          (lambda (($rhs : V))
            (env-end-parser $env $value $rhs)))))))

(: env-plus-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-rhs-parser $env $value $symbol)
  (first-parser
    (env-plus-empty-rhs-parser $env $value $symbol)
    (env-plus-space-rhs-parser $env $value $symbol)
    (env-plus-colon-rhs-parser $env $value $symbol)
    (env-plus-parens-rhs-parser $env $value $symbol)
    (env-plus-newline-rhs-parser $env $value $symbol)))

(: env-plus-empty-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-empty-rhs-parser $env $value $symbol)
  (env-begin-parser $env $value $symbol))

(: env-plus-space-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-space-rhs-parser $env $value $symbol)
  (prefix-parser space-parser
    (parser-bind
      (env-begin-parser $env $value $symbol)
      (lambda (($rhs : V))
        (env-plus-line-parser $env $rhs)))))

(: env-plus-colon-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-colon-rhs-parser $env $value $symbol)
  (prefix-parser (exact-string-parser ": ")
    (parser-bind
      (env-begin-parser $env $value $symbol)
      (lambda (($rhs : V))
        (repeat-separated-parser $rhs comma-separator-parser
          (lambda (($rhs : V))
            (env-plus-line-parser $env $rhs)))))))

(: env-plus-parens-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-parens-rhs-parser $env $value $symbol)
  (prefix-parser-suffix
    (exact-string-parser "(")
    (parser-bind (env-begin-parser $env $value $symbol)
      (lambda (($rhs : V))
        (value-or-parser $rhs
          (repeat-separated-parser $rhs comma-separator-parser
            (lambda (($rhs : V))
              (env-plus-line-parser $env $rhs))))))
    (exact-string-parser ")")))

(: env-plus-newline-rhs-parser : (All (V) (-> (Env V) V Symbol (Parser V))))
(define (env-plus-newline-rhs-parser $env $value $symbol)
  (prefix-parser newlines-parser
    (indented-parser
      (parser-bind
        (env-begin-parser $env $value $symbol)
        (lambda (($rhs : V))
          (repeat-separated-parser $rhs comma-or-newlines-separator-parser
            (lambda (($rhs : V))
              (env-plus-line-parser $env $rhs))))))))
