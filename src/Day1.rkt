#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? number?)]
                       [solution-part2 (-> string? number?)]))

(struct elem (index value))

(define (find-pair-using-map lst value diff-map)
  (match lst
    ['() '(0 0)]
    [(cons x xs)
     (if (hash-has-key? diff-map x)
         (list x (hash-ref diff-map x))
         (find-pair-using-map xs value (hash-set diff-map (- value x) x)))
    ]))

(define (find-pair lst value)
  (find-pair-using-map lst value #hash()))

(define (find-triple-using-map lst diff-map)
  (match lst
    ['() '(0 0 0)]
    [(cons x xs)
     (let ([index-not-equal (λ (e) (not (equal? (elem-index x) (elem-index e))))])
          (if (and (hash-has-key? diff-map (elem-value x))
                   (andmap index-not-equal (hash-ref diff-map (elem-value x))))
              (cons x (hash-ref diff-map (elem-value x)))
              (find-triple-using-map xs diff-map)))
    ]))

(define (find-triple lst value)
  (let* ([indexed-list (map elem (range (length lst)) lst)]
         [fold-tuple-in-map (λ (t m) (hash-set m (- value (elem-value (first t)) (elem-value (second t))) t))]
         [diff-map (foldl fold-tuple-in-map #hash() (combinations indexed-list 2))])
        (find-triple-using-map indexed-list diff-map)))

(define input-parser
  (trim-spaces-eof (end-or-sep-by integer $eol)))

(define (solution-part1 input)
  (match-let ([(list x y) (find-pair (parse-result input-parser input) 2020)])
             (* x y)))

(define (solution-part2 input)
  (let ([triple (find-triple (parse-result input-parser input) 2020)])
       (foldl * 1 (map elem-value triple))))