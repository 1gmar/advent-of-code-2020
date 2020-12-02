#lang racket
(provide solution-part1 solution-part2)

(define (find-pair-using-map lst value map)
  (match lst
    ['() '(0 . 0)]
    [(cons x xs) 
      (cond
        [(hash-has-key? map x) (cons x (hash-ref map x))]
        [else (find-pair-using-map xs value (hash-set map (- value x) x))])
    ]))

(define (find-pair lst value) (find-pair-using-map lst value #hash()))

(define (find-triple-using-map lst map)
  (match lst
    ['() '(0 0 0)]
    [(cons x xs)
      (cond
        [(hash-has-key? map x) (cons x (hash-ref map x))]
        [else (find-triple-using-map xs map)])
    ]))

(define (find-triple lst value) 
  (let*
    (
      [fold-tuple-in-map (λ (t m) (hash-set m (- value (first t) (second t)) t))]
      [diffMap (foldl fold-tuple-in-map #hash() (combinations lst 2))]
    )
    (find-triple-using-map lst diffMap)))

(define (read-numbers input) (map string->number (string-split input "\n")))

(define (solution-part1 input)
  (match-let ([(cons x y) (find-pair (read-numbers input) 2020)]) (* x y)))

(define (solution-part2 input)
  (match-let ([(list x y z) (find-triple (read-numbers input) 2020)]) (* x y z)))