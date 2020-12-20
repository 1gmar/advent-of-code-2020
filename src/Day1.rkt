#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(struct elem (index value))

(define/match (find-pair lst value [diff-map #hash()])
  [('() _ _) '(0 0)]
  [((cons x xs) _ _) (if (hash-has-key? diff-map x)
                         (list x (hash-ref diff-map x))
                         (find-pair xs value (hash-set diff-map (- value x) x)))])

(define/match (find-triple-using-map lst diff-map)
  [('() _) '(0 0 0)]
  [((cons (elem x-index x-value) xs) _)
   (let ([index-not-equal? (compose not (curry equal? x-index) elem-index)])
        (if (and (hash-has-key? diff-map x-value)
                 (andmap index-not-equal? (hash-ref diff-map x-value)))
            (cons x-value (map elem-value (hash-ref diff-map x-value)))
            (find-triple-using-map xs diff-map)))])

(define (find-triple lst value)
  (define/match (fold-tuple-in-map combination hmap)
    [((list (elem _ lhs) (elem _ rhs)) _) (hash-set hmap (- value lhs rhs) combination)])
  (let* ([indexed-list (map elem (range (length lst)) lst)]
         [diff-map (foldl fold-tuple-in-map #hash() (combinations indexed-list 2))])
        (find-triple-using-map indexed-list diff-map)))

(define input-parser
  (trim-spaces-eof (end-or-sep-by integer $eol)))

(define (solution-part1 input)
  (apply * (find-pair (parse-result input-parser input) 2020)))

(define (solution-part2 input)
  (apply * (find-triple (parse-result input-parser input) 2020)))