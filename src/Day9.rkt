#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer? integer?)]
                       [solution-part2 (-> string? integer? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by +integer $eol)))

(define (solution-part1 input preamble-size)
  (define/match (is-valid? preamble-number-pair)
    [((cons preamble number)) (ormap (compose (curry equal? number) (curry apply +)) (combinations preamble 2))])
  (define (slide-preamble preamble numbers size)
    (drop (append preamble (take numbers size)) size))
  (let*-values ([(preamble numbers) (split-at (parse-result input-parser input) preamble-size)]
                [(find-first) (compose stream-first (curry stream-filter (compose not is-valid?)))])
               (cdr (find-first (for/stream ([n (in-list numbers)] [i (in-range (length numbers))])
                                            (cons (slide-preamble preamble numbers i) n))))))

(define (solution-part2 input target-sum)
  (define/match (cons-min-max x min-max-pair)
    [(_ (cons min-x max-x)) (cons (min x min-x) (max x max-x))])
  (define/match (find-min-max lst)
    [('()) '(0 . 0)]
    [((cons head _)) (foldl cons-min-max (cons head head) lst)])
  (define (find-contiguous-set numbers [start 0] [size 2])
    (let* ([lst (take (drop numbers start) size)]
           [sum (apply + lst)])
          (cond [(> target-sum sum) (find-contiguous-set numbers start (add1 size))]
                [(< target-sum sum) (find-contiguous-set numbers (add1 start))]
                [else lst])))
  (match-let ([(cons min-n max-n) (find-min-max (find-contiguous-set (parse-result input-parser input)))])
             (+ min-n max-n)))