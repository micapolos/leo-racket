#lang typed/racket/base

(provide read-leo-stxs)

(require
  typed/rackunit
  racket/string
  typed/syntax/readerr
  (for-syntax typed/racket/base))

(struct leo 
  ((reversed-statement-stxs : (Listof (Syntaxof Any)))
   (reversed-value-stxs : (Listof (Syntaxof Any)))
   (empty-line? : Boolean))
  #:transparent)

(define empty-leo (leo null null #f))

(define (leo-reversed-stxs ($leo : leo))
  (append
    (leo-reversed-value-stxs $leo)
    (leo-reversed-statement-stxs $leo)))

(define (leo-stxs ($leo : leo))
  (reverse (leo-reversed-stxs $leo)))

(define (leo-append ($lhs : leo) ($rhs : leo)) 
  (struct-copy leo $lhs
    (reversed-value-stxs
      (append
        (leo-reversed-stxs $rhs)
        (leo-reversed-value-stxs $lhs)))
    (empty-line? 
      (or 
        (leo-empty-line? $lhs) 
        (leo-empty-line? $rhs)))))

(define (leo-with-value-stx ($leo : leo) ($value-stx : (Syntaxof Any)))
  (struct-copy leo $leo
    (reversed-value-stxs (list $value-stx))))

(define (leo-append-value-stx ($leo : leo) ($value-stx : (Syntaxof Any)))
  (struct-copy leo $leo
    (reversed-value-stxs
      (cons $value-stx (leo-reversed-value-stxs $leo)))))

(define (leo-append-reversed-value-stxs 
    ($leo : leo) ($reversed-value-stxs : (Listof (Syntaxof Any))))
  (struct-copy leo $leo
    (reversed-value-stxs
      (append
        $reversed-value-stxs
        (leo-reversed-value-stxs $leo)))))

(define (leo-append-empty-line ($leo : leo)) 
  (struct-copy leo $leo
    (empty-line? #t)))

(define (leo-set-empty-line-from ($leo : leo) ($rhs : leo)) 
  (struct-copy leo $leo
    (empty-line? (leo-empty-line? $rhs))))

(define (leo-commit ($leo : leo))
  (leo
    (append
      (leo-reversed-value-stxs $leo)
      (leo-reversed-statement-stxs $leo))
    null
    #f))

(define (leo-commit-if-empty-line ($leo : leo)) 
  (cond
    ((leo-empty-line? $leo) (leo-commit $leo))
    (else $leo)))

(define (leo-gather ($leo : leo)) 
  (struct-copy leo $leo
    (reversed-value-stxs (list #`(#,@(leo-reversed-value-stxs $leo))))))

(define 
  (leo-append-identifier-stx-list?-rhs 
    ($leo : leo) 
    ($identifier : Symbol) 
    ($stx : (Syntaxof Any)) 
    ($list? : Boolean) 
    ($rhs : leo))
  (cond
    ((equal? $identifier `do) (leo-append-do-rhs $leo $rhs))
    ((equal? $identifier `the) (leo-append-the-rhs $leo $rhs))
    (else (leo-append-stx-list?-rhs $leo $stx $list? $rhs))))

(define (leo-append-do-rhs ($leo : leo) ($rhs : leo)) : leo
  (leo-append $leo $rhs))

(define (leo-append-the-rhs ($leo : leo) ($rhs : leo)) : leo
  (leo-append $leo (leo-gather $rhs)))

(define 
  (leo-append-stx-list?-rhs 
    ($leo : leo) 
    ($stx : (Syntaxof Any))
    ($list? : Boolean) 
    ($rhs : leo)) : leo
  (leo-set-empty-line-from
    (let 
      (($args
        (reverse
          (append
            (leo-reversed-stxs $rhs)
            (leo-reversed-value-stxs $leo)))))
      (cond
        ((and (null? $args) (not $list?)) (leo-with-value-stx $leo $stx))
          (else (leo-with-value-stx $leo #`(#,$stx #,@$args)))))
    $rhs))

; ---------------------------------------------------------------

(define (read-leo-stxs ($port : Input-Port) ($src : Any))
  (leo-stxs (read-leo $port $src 0 empty-leo)))

; ---------------------------------------------------------------

(define (err ($port : Input-Port) ($src : Any) ($message : String))
  (let-values 
    (((line col pos) (port-next-location $port)))
    (raise-read-error $message $src line col #f #f)))

(define (char-name ($char : Char))
  (substring (format "~v" $char) 2))

; ; -------------------------------------------------------

(define (symbol-colon-suffix? ($symbol : Symbol))
  (string-suffix? (symbol->string $symbol) ":"))

(define (symbol-drop-last-char ($symbol : Symbol))
  (string->symbol
    (let (($string (symbol->string $symbol)))
      (substring $string 0 (- (string-length $string) 1)))))

(define 
  (stx-symbol-drop-last-char 
    ($stx : (Syntaxof Any)) 
    ($symbol : Symbol)) : (Syntaxof Any)
  (datum->syntax $stx (symbol-drop-last-char $symbol)))

; ; -------------------------------------------------------

(define (skip-char-count ($port : Input-Port) ($count : Nonnegative-Integer))
  (read-string $count $port))

(define (skip-char ($port : Input-Port))
  (read-char $port))

; ; -------------------------------------------------------

(define (peek-eof ($port : Input-Port) ($skip : Nonnegative-Integer 0))
  (eof-object? (peek-char $port $skip)))

(check-equal? (peek-eof (open-input-string "")) #t)
(check-equal? (peek-eof (open-input-string "a")) #f)

; ; -------------------------------------------------------

(define 
  (peek-exact-string? 
    ($port : Input-Port) 
    ($string : String) 
    ($skip : Nonnegative-Integer 0)) : Boolean
  (equal? 
    (peek-string (string-length $string) $skip $port)
    $string))

(check-equal? (peek-exact-string? (open-input-string "") "") #t)
(check-equal? (peek-exact-string? (open-input-string "a") "") #t)
(check-equal? (peek-exact-string? (open-input-string "a") "a") #t)
(check-equal? (peek-exact-string? (open-input-string "a") "b") #f)
(check-equal? (peek-exact-string? (open-input-string "ab") "a") #t)
(check-equal? (peek-exact-string? (open-input-string "ab") "ab") #t)
(check-equal? (peek-exact-string? (open-input-string "ab") "ac") #f)
(check-equal? (peek-exact-string? (open-input-string "abc") "ab") #t)

; ; -------------------------------------------------------

(define indent-string "  ")
(define indent-length (string-length indent-string))

(define (peek-indent ($port : Input-Port) ($skip : Nonnegative-Integer 0)) 
  (peek-exact-string? $port indent-string $skip))

; ; -------------------------------------------------------

(define 
  peek-depth-max : (->* (Input-Port) (Integer Nonnegative-Integer Nonnegative-Integer) Integer)
  (lambda ($port ($max -1) ($depth 0) ($skip 0))
    (cond
      ((<= $max 0) $depth)
      ((peek-indent $port $skip) 
        (peek-depth-max 
          $port 
          (- $max 1) 
          (+ $depth 1) 
          (+ indent-length $skip)))
      (else $depth))))

(check-equal? (peek-depth-max (open-input-string "") 2) 0)
(check-equal? (peek-depth-max (open-input-string " ") 2) 0)
(check-equal? (peek-depth-max (open-input-string "  ") 2) 1)
(check-equal? (peek-depth-max (open-input-string "   ") 2) 1)
(check-equal? (peek-depth-max (open-input-string "    ") 2) 2)
(check-equal? (peek-depth-max (open-input-string "     ") 2) 2)
(check-equal? (peek-depth-max (open-input-string "      ") 2) 2)

; ; -------------------------------------------------------

(define (peek-exact-depth ($port : Input-Port) ($depth : Nonnegative-Integer))
  (= (peek-depth-max $port $depth) $depth))

(define (skip-depth ($port : Input-Port) ($depth : Nonnegative-Integer))
  (skip-char-count $port (* indent-length $depth)))

; ; -------------------------------------------------------

(define 
  read-depth : (->* (Input-Port) (Integer Nonnegative-Integer) Nonnegative-Integer)
  (lambda ($port ($max -1) ($depth 0))
    (cond
      ((= $max 0) $depth)
      ((peek-indent $port) 
        (skip-char-count $port indent-length)
        (read-depth $port (- $max 1) (+ $depth 1)))
      (else $depth))))

(check-equal? (read-depth (open-input-string "")) 0)
(check-equal? (read-depth (open-input-string " ")) 0)
(check-equal? (read-depth (open-input-string " a")) 0)
(check-equal? (read-depth (open-input-string "  ")) 1)
(check-equal? (read-depth (open-input-string "   ")) 1)
(check-equal? (read-depth (open-input-string "   a")) 1)
(check-equal? (read-depth (open-input-string "    ")) 2)
(check-equal? (read-depth (open-input-string "    a") 0) 0)
(check-equal? (read-depth (open-input-string "    a") 1) 1)
(check-equal? (read-depth (open-input-string "    a") 2) 2)
(check-equal? (read-depth (open-input-string "    a") 3) 2)

; -------------------------------------------------------

(define (read-atom ($port : Input-Port) ($src : Any)) : (U (Syntaxof Any) EOF)
  (read-syntax $src $port))

(define 
  (read-leo-atoms 
    ($port : Input-Port) 
    ($src : Any) 
    ($leo : leo)) : leo
  (let (($atom (read-atom $port $src)))
    (cond
      ((eof-object? $atom) $leo)
      (else 
        (let (($leo (leo-append-value-stx $leo $atom)))
          (cond
            ((equal? (peek-char $port) #\space)
              (skip-char $port)
              (read-leo-atoms $port $src $leo))
            ((equal? (peek-char $port) #\newline)
              (skip-char $port)
              $leo)
            (else (err $port $src "expected space or newline"))))))))

; ; -------------------------------------------------------

(define (read-leo-empty-lines ($port : Input-Port) ($leo : leo)) : leo
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\newline)
        (skip-char $port)
        (read-leo-empty-lines $port (leo-append-empty-line $leo)))
      (else $leo))))

(define 
  (read-leo 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer) 
    ($leo : leo)) : leo
  (let*
    (($leo-line 
      (read-leo-line-or-eof $port $src $depth 
        (leo-commit-if-empty-line $leo))))
    (cond
      ((eof-object? $leo-line) $leo)
      (else 
        (let (($leo-line (read-leo-empty-lines $port $leo-line)))
          (cond
            ((peek-exact-depth $port $depth)
              (skip-depth $port $depth)
              (read-leo $port $src $depth $leo-line))
            (else $leo-line)))))))

(define 
  (read-leo-list 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer) 
    ($leo : leo)) : leo
  (let*
    (($leo-line 
      (read-leo-line $port $src $depth empty-leo)))
    (cond
      ((eof-object? $leo-line) $leo)
      (else 
        (let (($leo-line (read-leo-empty-lines $port $leo-line)))
          (cond
            ((peek-exact-depth $port $depth)
              (skip-depth $port $depth)
              (read-leo-list $port $src $depth (leo-append $leo $leo-line)))
            (else 
              (leo-append $leo $leo-line))))))))

(define 
  (read-leo-line
    ($port : Input-Port)
    ($src : Any)
    ($depth : Nonnegative-Integer)
    ($leo : leo)) : leo
  (let (($leo-line-or-eof (read-leo-line-or-eof $port $src $depth $leo)))
    (cond
      ((eof-object? $leo-line-or-eof)
        (err $port $src "Unexpected eof"))
      (else $leo-line-or-eof))))

(define 
  (read-leo-line-or-eof 
    ($port : Input-Port)
    ($src : Any)
    ($depth : Nonnegative-Integer)
    ($leo : leo)) : (U leo EOF)
  (let (($peeked-char (peek-char $port)))
    (cond
      ((equal? $peeked-char #\newline)
        (skip-char $port)
        (read-leo-line $port $src $depth (leo-append-empty-line $leo)))
      ((eof-object? $peeked-char) $peeked-char)
      ((char-whitespace? $peeked-char)
        (let-values (((line col pos) (port-next-location $port)))
          (err $port $src (string-append "unexpected " (char-name $peeked-char)))))
      (else
        (let (($stx (read-atom $port $src)))
          (cond
            ((eof-object? $stx) 
              (err $port $src "Unexpected eof"))
            (else 
              (let (($datum (syntax-e $stx)))
                (cond
                  ((symbol? $datum)
                    (read-leo-symbol-stx-rhs $port $src $depth $leo $datum $stx))
                  (else
                    (read-leo-default-line $port $src $depth $leo $stx)))))))))))

(define 
  (read-leo-symbol-stx-rhs 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer) 
    ($leo : leo)
    ($symbol : Symbol)
    ($stx : (Syntaxof Any))) : leo
  (cond
    ((symbol-colon-suffix? $symbol)
      (let (($symbol (symbol-drop-last-char $symbol)))
        (read-leo-identifier-colon-stx-rhs $port $src $depth $leo
          $symbol (datum->syntax #f $symbol))))
    ((peek-exact-string? $port " =")
      (skip-char-count $port 2)
      (read-leo-identifier-stx-equal-rhs $port $src $depth $leo $symbol $stx))
    (else 
      (read-leo-identifier-stx-rhs $port $src $depth $leo $symbol $stx))))

(define 
  (read-leo-identifier-stx-equal-rhs 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer)
    ($leo : leo) 
    ($symbol : Symbol) 
    ($stx : (Syntaxof Any))) : leo
  (leo-append
    (leo-append-value-stx
      $leo
      (datum->syntax #f (string->keyword (symbol->string $symbol))))
    (read-leo-rhs-list $port $src $depth)))

(define 
  (read-leo-identifier-stx-rhs 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer) 
    ($leo : leo) 
    ($identifier : Symbol) 
    ($stx : (Syntaxof Any))) : leo
  (leo-append-identifier-stx-list?-rhs $leo $identifier $stx #f 
    (read-leo-rhs $port $src $depth)))

(define 
  (read-leo-identifier-colon-stx-rhs 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer) 
    ($leo : leo) 
    ($identifier : Symbol) 
    ($stx : (Syntaxof Any))) : leo
  (leo-append-identifier-stx-list?-rhs $leo $identifier $stx #t
    (read-leo-rhs-list $port $src $depth)))

(define 
  (read-leo-default-line
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer)
    ($leo : leo) 
    ($default : (Syntaxof Any))) : leo
  (cond 
    ((equal? (peek-char $port) #\newline)
      (skip-char $port)
      (leo-with-value-stx $leo $default))
    (else (err $port $src "expected newline"))))

(define 
  (read-leo-rhs 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer)) : leo
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-leo-line $port $src $depth empty-leo))
      ((equal? $char #\newline)
        (skip-char $port)
        (let (($rhs-depth (+ $depth 1)))
          (cond
            ((peek-exact-depth $port $rhs-depth)
              (skip-depth $port $rhs-depth)
              (read-leo $port $src $rhs-depth empty-leo))
            (else empty-leo))))
      (else
        (err $port $src "expected space or newline")))))

(define 
  (read-leo-rhs-atoms 
    ($port : Input-Port) 
    ($src : Any)) : leo
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-leo-atoms $port $src empty-leo))
      ((equal? $char #\newline)
        (skip-char $port)
        empty-leo)
      (else 
        (err $port $src "expected space or newline")))))

(define 
  (read-leo-rhs-list 
    ($port : Input-Port) 
    ($src : Any) 
    ($depth : Nonnegative-Integer)) : leo 
  (let (($char (peek-char $port)))
    (cond
      ((equal? $char #\space)
        (skip-char $port)
        (read-leo-atoms $port $src empty-leo))
      ((equal? $char #\newline)
        (skip-char $port)
        (let (($rhs-depth (+ $depth 1)))
          (cond
            ((peek-exact-depth $port $rhs-depth)
              (skip-depth $port $rhs-depth)
              (read-leo-list $port $src $rhs-depth empty-leo))
            (else empty-leo))))
      (else (err $port $src "expected space or newline")))))

(define (string->leo-syntaxes ($string : String))
  (leo-stxs (read-leo (open-input-string $string) "" 0 empty-leo)))

(define (string->leo-datums ($string : String))
  (map syntax->datum (string->leo-syntaxes $string)))

(check-equal? (string->leo-datums "") `())

(check-equal? (string->leo-datums "foo\n") `(foo))

(check-equal? (string->leo-datums "\nfoo\n") `(foo))
(check-equal? (string->leo-datums "\nfoo\n\n") `(foo))

(check-equal? (string->leo-datums "foo\nbar\n") `((bar foo)))
(check-equal? (string->leo-datums "foo\nbar\nzoo\n") `((zoo (bar foo))))

(check-equal? (string->leo-datums "foo bar\n") `((foo bar)))
(check-equal? (string->leo-datums "foo bar zoo\n") `((foo (bar zoo))))

(check-equal? (string->leo-datums "foo\n  bar\n") `((foo bar)))
(check-equal? (string->leo-datums "foo\n  bar\n  zoo\n") `((foo (zoo bar))))

(check-equal? (string->leo-datums "foo bar\n  zoo\n") `((foo (bar zoo))))
(check-equal? (string->leo-datums "foo\n  bar zoo\n") `((foo (bar zoo))))

(check-equal? (string->leo-datums "foo 123\n") `((foo 123)))
(check-equal? (string->leo-datums "1\nplus 2\n") `((plus 1 2)))
(check-equal? (string->leo-datums "\"foo\"\n") `("foo"))

(check-equal? (string->leo-datums "1\nplus 2\ntimes\n  3\n  minus 4\n") `((times (plus 1 2) (minus 3 4))))

(check-equal? (string->leo-datums "do\n") `())
(check-equal? (string->leo-datums "do 1\n") `(1))
(check-equal? (string->leo-datums "1\ndo\n") `(1))
(check-equal? (string->leo-datums "1\ndo 2\n") `(1 2))
(check-equal? (string->leo-datums "do 1\ndo 2\n") `(1 2))
(check-equal? (string->leo-datums "do 1\n") `(1))
(check-equal? (string->leo-datums "do\n  1\n  plus 2\ndo\n  3\n  plus 4\n") `((plus 1 2) (plus 3 4)))

(check-equal? (string->leo-datums "do:\n") `())
(check-equal? (string->leo-datums "do: 1\n") `(1))
(check-equal? (string->leo-datums "do: 1 2\n") `(1 2))

(check-equal? (string->leo-datums "foo:\n") `((foo)))
(check-equal? (string->leo-datums "foo: 1\n") `((foo 1)))

(check-equal? (string->leo-datums "foo: 1 2\n") `((foo 1 2)))

(check-equal? (string->leo-datums "foo:\n  x 1\n") `((foo (x 1))))

(check-equal? (string->leo-datums "foo:\n  x 1\n  y 2\n") `((foo (x 1) (y 2))))

(check-equal? (string->leo-datums "circle:\n  radius 10\n  center:\n    x 10\n    y 20\n") `((circle (radius 10) (center (x 10) (y 20)))))

(check-equal? (string->leo-datums "x =\nfoo\n") `((foo #:x)))
(check-equal? (string->leo-datums "x = 1\nfoo\n") `((foo #:x 1)))
(check-equal? (string->leo-datums "x = 1 2\nfoo\n") `((foo #:x 1 2)))
(check-equal? (string->leo-datums "x = 1\ny =\nfoo\n") `((foo #:x 1 #:y)))
(check-equal? (string->leo-datums "x = 1\ny = 2\nfoo\n") `((foo #:x 1 #:y 2)))
(check-equal? (string->leo-datums "x = 1\ny = 2 3\nfoo\n") `((foo #:x 1 #:y 2 3)))

(check-equal? (string->leo-datums "x =\nfoo\n") `((foo #:x)))
(check-equal? (string->leo-datums "x =\n  1\nfoo\n") `((foo #:x 1)))
(check-equal? (string->leo-datums "x =\n  1\n  2\nfoo\n") `((foo #:x 1 2)))
(check-equal? (string->leo-datums "x =\n  1\ny =\nfoo\n") `((foo #:x 1 #:y)))
(check-equal? (string->leo-datums "x =\n  1\ny =\n  2\nfoo\n") `((foo #:x 1 #:y 2)))
(check-equal? (string->leo-datums "x =\n  1\ny =\n  2\n  3\nfoo\n") `((foo #:x 1 #:y 2 3)))

(check-equal? (string->leo-datums "foo\n  x = 1\n  bar\n") `((foo (bar #:x 1))))
(check-equal? (string->leo-datums "foo:\n  x = 1\n  bar\n") `((foo #:x 1 bar)))

(check-equal? (string->leo-datums "foo\n\nbar\n") `(foo bar))
(check-equal? (string->leo-datums "foo\n  bar\n\n  zoo\n") `((foo bar zoo)))
(check-equal? (string->leo-datums "foo\n  bar\n\n  zoo\n\ngoo\n") `((foo bar zoo) goo))

(check-equal? (string->leo-datums "the\n") `(()))
(check-equal? (string->leo-datums "the foo\n") `((foo)))
