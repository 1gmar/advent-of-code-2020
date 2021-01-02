#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer? integer?)]
                       [solution-part2 (-> string? integer? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by +integer $eol)))

(define (solution-part1 input preamble-size)

  (define/match (is-valid? preamble-number-pair)
    [((cons preamble number)) (ormap (λ~>> (apply +) (= number)) (combinations preamble 2))])

  (define (slide-preamble preamble numbers size)
    (~>> (take numbers size)
         (append preamble)
         (drop _ size)))

  (let-values ([(preamble numbers) (split-at (parse-result input-parser input) preamble-size)])
              (~>> (for/stream ([n (in-list numbers)]
                                [i (in-range (length numbers))])
                               (cons (slide-preamble preamble numbers i) n))
                   (stream-filter (λ~>> is-valid? not))
                   stream-first
                   cdr)))

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