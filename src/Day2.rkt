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
                                     (return (password (list pmin pmax) letter (apply vector-immutable letters))))])
       (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (letter-within-range? pass)
  (let ([letter-equal? (λ (letter) (equal? letter (password-letter pass)))])
       (<= (first (password-range pass))
           (vector-count letter-equal? (password-value pass))
           (second (password-range pass)))))

(define (letter-on-oneof-pos? pass)
  (let* ([pass-value (password-value pass)]
         [pass-size (vector-length pass-value)]
         [letter-on-pos? (λ (pos) (and (<= pos pass-size)
                                       (equal? (vector-ref pass-value (sub1 pos)) (password-letter pass))))])
        (xor (letter-on-pos? (first (password-range pass)))
             (letter-on-pos? (second (password-range pass))))))

(define (solution-part1 input)
  (length (filter letter-within-range? (parse-result input-parser input))))

(define (solution-part2 input)
  (length (filter letter-on-oneof-pos? (parse-result input-parser input))))