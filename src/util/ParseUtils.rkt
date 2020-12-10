#lang racket
(provide (all-defined-out) (all-from-out parsack))
(require (only-in parsack $alphaNum $digit $eof $eol $err $letter $spaces between char choice endBy many1 oneOf
                          oneOfStrings parse-result parser-compose parser-cons parser-one parser-seq return sepBy
                          string try >> >>= <any>))

(define (try-or-else parser fallback)
  (<any> (try parser) (return fallback)))

(define (any-try lhs rhs)
  (<any> (try lhs) rhs))

(define chars->number
  (compose string->number list->string))

(define +integer
  (>>= (many1 $digit) (λ (digits) (return (chars->number digits)))))

(define -integer
  (>>= (parser-cons (char #\-) (many1 $digit))
       (λ (digits) (return (chars->number digits)))))

(define integer
  (<any> +integer -integer))

(define (trim-spaces-eof parser)
  (between $spaces (>> $spaces $eof) parser))

(define (end-or-sep-by parser separator)
  (any-try (endBy parser separator) (sepBy parser separator)))

(define (parser-count n parser)
  (let ([fold-parsers (λ (p acc) (parser-compose (x <- acc) (y <- p) (return (flatten (list x y)))))])
       (foldl fold-parsers parser (make-list (sub1 n) parser))))