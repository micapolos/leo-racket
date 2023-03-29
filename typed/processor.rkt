#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  (for-syntax racket/base))

(data (I O) processor
  (fn : (-> I (Stackof O))))

(define-syntax (processor! $syntax)
  (syntax-case $syntax (:)
    ((_ (var : type) body ...)
      #`(processor
        (lambda ((var : type)) body ...)))))

(define #:forall (I O)
  (process
    ($processor : (Processor I O))
    ($input : I))
  : (Stackof O)
  (#%app (processor-fn $processor) $input))

(check-equal?
  (process
    (processor! ($string : String)
      (stack "processed" $string))
    "foo")
  (stack "processed" "foo"))

(define #:forall (I O)
  (push-processed
    ($processor : (Processor I O))
    ($output-stack : (Stackof O))
    ($input : I))
  : (Stackof O)
  (push-stack $output-stack (process $processor $input)))

(define #:forall (I O)
  (process-stack
    ($processor : (Processor I O))
    ($input-stack : (Stackof I)))
  : (Stackof O)
  (fold
    null
    (reverse $input-stack)
    (lambda (($output-stack : (Stackof O)) ($input : I))
      (push-processed $processor $output-stack $input))))

(define #:forall (I M O)
  (processor-compose
    ($lhs : (Processor M O))
    ($rhs : (Processor I M)))
  : (Processor I O)
  (processor! ($input : I)
    (process-stack $lhs (process $rhs $input))))

(check-equal?
  (list->string
    (reverse
      (process
        (processor-compose
          (processor! ($char : Char) (stack $char #\space))
          (processor! ($string : String) (reverse (string->list $string))))
        "foo")))
  "f o o ")
