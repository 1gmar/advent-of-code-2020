#lang racket
(provide (all-defined-out) (all-from-out parsack))
(require (only-in parsack $digit $eof $eol $spaces between char endBy many1 parse-result parser-cons return sepBy
                          string try >> >>= <any>))

(define integer
  (let* ([positive (many1 $digit)]
         [negative (parser-cons (char #\-) positive)]
         [chars->number (compose string->number list->string)])
        (>>= (<any> positive negative)
             (λ (x) (return (chars->number x))))))

(define (trim-spaces-eof parser)
  (between $spaces
           (>> $spaces $eof)
           parser))