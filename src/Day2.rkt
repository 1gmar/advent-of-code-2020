#lang racket
(require (only-in threading λ~>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(struct password (range letter chars))

(define input-parser
  (let ([line-parser (parser-compose (pmin <- +integer)
                                     (char #\-)
                                     (pmax <- +integer)
                                     $spaces
                                     (letter <- $letter)
                                     (char #\:)
                                     $spaces
                                     (letters <- (many1 $letter))
                                     (return (password (cons pmin pmax) letter (apply vector-immutable letters))))])
       (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (solution-part1 input)

  (define/match (letter-within-range? pass)
    [((password (cons pmin pmax) letter chars)) (<= pmin (vector-count (curry eq? letter) chars) pmax)])

  (count letter-within-range? (parse-result input-parser input)))

(define (solution-part2 input)

  (define/match (letter-on-oneof-pos? pass)
    [((password (cons pos1 pos2) letter chars))
     (let* ([pass-size (vector-length chars)]
            [letter-on-pos? (match-lambda [(? (λ~> (<= pass-size)) pos) (eq? (vector-ref chars (sub1 pos)) letter)]
                                          [else #f])])
           (xor (letter-on-pos? pos1) (letter-on-pos? pos2)))])

  (count letter-on-oneof-pos? (parse-result input-parser input)))