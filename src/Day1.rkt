#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by integer $eol)))

(define (solution-part1 input)
  (define/match (find-pair lst value [diff-map #hash()])
    [('() _ _) '()]
    [((cons x xs) _ _) (if (hash-has-key? diff-map x)
                           (list x (hash-ref diff-map x))
                           (find-pair xs value (hash-set diff-map (- value x) x)))])
  (apply * (find-pair (parse-result input-parser input) 2020)))

(define (solution-part2 input)
  (define/match (find-triple lst diff-map)
    [('() _) '()]
    [((cons (cons x-index x-value) xs) _)
     (let ([index-not-equal? (compose not (curry equal? x-index) car)])
          (if (and (hash-has-key? diff-map x-value)
                   (andmap index-not-equal? (hash-ref diff-map x-value)))
              (cons x-value (map cdr (hash-ref diff-map x-value)))
              (find-triple xs diff-map)))])
  (define/match (fold-tuple-in-map combination hmap)
    [((list (cons _ lhs) (cons _ rhs)) _) (hash-set hmap (- 2020 lhs rhs) combination)])
  (let* ([ints (parse-result input-parser input)]
         [indexed-list (map cons (range (length ints)) ints)]
         [diff-map (foldl fold-tuple-in-map #hash() (combinations indexed-list 2))])
        (apply * (find-triple indexed-list diff-map))))