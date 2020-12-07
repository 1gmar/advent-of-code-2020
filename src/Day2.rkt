#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? number?)]
                       [solution-part2 (-> string? number?)]))

(struct password (range letter value))

(define input-parser
  (let ([line-parser (parser-compose (pmin <- +integer)
                                     (char #\-)
                                     (pmax <- +integer)
                                     $spaces
                                     (letter <- $letter)
                                     (char #\:)
                                     $spaces
                                     (letters <- (many1 $letter))
                                     (return (password (list pmin pmax) letter letters)))])
       (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (letter-within-range? pass)
  (let ([letter-equal? (λ (letter) (equal? letter (password-letter pass)))])
       (<= (first (password-range pass))
           (count letter-equal? (password-value pass))
           (second (password-range pass)))))

(define (letter-on-oneof-pos? pass)
  (let* ([pos1 (first (password-range pass))]
         [pos2 (second (password-range pass))]
         [pass-value (password-value pass)]
         [pass-size (length pass-value)]
         [letter (password-letter pass)])
        (xor (and (<= pos1 pass-size) (equal? (list-ref pass-value (sub1 pos1)) letter))
             (and (<= pos2 pass-size) (equal? (list-ref pass-value (sub1 pos2)) letter)))))

(define (solution-part1 input)
  (length (filter letter-within-range? (parse-result input-parser input))))

(define (solution-part2 input)
  (length (filter letter-on-oneof-pos? (parse-result input-parser input))))