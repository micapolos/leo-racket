#lang leo/typed

(data (parser of I O)
  (parse-fn : (-> I (Parsed I O))))

(define-type (Parsed I O) 
  (U 
    (Parserof I O) 
    (Complete O)))

(data (O) complete 
  (value : O))

(define #:forall (I O) (parser-parse 
  ($parser : (Parserof I O)) 
  ($item : I)) : (Parsed I O)
  ((parser-parse-fn $parser) $item))

(define #:forall (I O) (parsed-complete ($parsed : (Parsed I O))) : (Complete O)
  (cond
    ((complete? $parsed) $parsed)
    (else (error "Not complete"))))

(define #:forall (I O) (parsed-parser ($parsed : (Parsed I O))) : (Parserof I O)
  (cond
    ((parser? $parsed) $parsed)
    (else (error "Already complete"))))

(define #:forall (I O) (parsed-fold
  ($parsed : (Parsed I O))
  ($list : (Listof I)))
  : (Parsed I O)
  (fold $parsed $list
    (lambda (($folded : (Parsed I O)) ($item : I)) : (Parsed I O)
      (parser-parse (parsed-parser $folded) $item))))

(define #:forall (I O) (parser-parse-list
  ($parse : (Parsed I O)) 
  ($list : (Listof I))) : O
  (complete-value (parsed-complete (parsed-fold $parse $list))))

(define (char-stack-length-parser-of-symbol
  ($char-stack : (Stackof Char)) 
  ($length : Positive-Integer)) 
  : (Parserof Char Symbol)
  (parser
    (lambda (($char : Char)) : (Parsed Char Symbol)
      (bind $new-char-stack (push $char-stack $char)
        (bind $new-length (sub1 $length)
          (case $new-length
            ((0) 
              (complete
                (string->symbol
                  (list->string
                    (reverse $new-char-stack)))))
            (else 
              (char-stack-length-parser-of-symbol
                $new-char-stack
                $new-length))))))))

(define (length-parser-of-symbol ($length : Positive-Integer)) : (Parserof Char Symbol)
  (char-stack-length-parser-of-symbol null $length))

(parser-parse-list (length-parser-of-symbol 2) (list #\a #\b))
