#lang racket
(provide (all-defined-out) (all-from-out parsack))
(require (only-in threading λ~>>)
         (only-in parsack $alphaNum $digit $eof $eol $err $letter $space $spaces between char choice endBy lookAhead
                          many many1 many1Until oneOf oneOfStrings optional parse-result parser-compose parser-cons
                          parser-one parser-seq return sepBy sepBy1 skipMany1 string try >> >>= <any>))

(define (try-or-else parser fallback)
  (<any> (try parser) (return fallback)))

(define (any-try lhs rhs)
  (<any> (try lhs) rhs))

(define chars->number
  (λ~>> list->string string->number))

(define +integer
  (>>= (>> (optional (char #\+)) (many1 $digit))
       (λ~>> chars->number return)))

(define -integer
  (>>= (parser-cons (char #\-) (many1 $digit))
       (λ~>> chars->number return)))

(define integer
  (<any> +integer -integer))

(define (trim-spaces-eof parser)
  (between $spaces (>> $spaces $eof) parser))

(define (end-or-sep-by parser separator)
  (any-try (endBy parser separator) (sepBy parser separator)))

(define (parser-count n parser)
  (let ([fold-parsers (λ (p acc) (parser-compose (x <- acc) (y <- p) (return (flatten (list x y)))))])
       (foldl fold-parsers parser (make-list (sub1 n) parser))))