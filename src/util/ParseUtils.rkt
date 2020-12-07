#lang racket
(provide (all-defined-out) (all-from-out parsack))
(require (only-in parsack $anyChar $digit $eof $eol $letter $spaces between char endBy many1 many1Until parse-result
                          parser-compose parser-cons return sepBy string try >> >>= <any>))

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
  (between $spaces
           (>> $spaces $eof)
           parser))

(define (end-or-sep-by parser separator)
  (<any> (try (endBy parser separator))
         (sepBy parser separator)))