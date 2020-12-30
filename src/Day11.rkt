#lang racket
(require "util/ParseUtils.rkt" racket/generator)
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([return-vector (compose return (curry apply vector))]
         [row-parser (>>= (many1 (oneOf ".L")) return-vector)])
        (>>= (trim-spaces-eof (end-or-sep-by row-parser $eol)) return-vector)))

(define/match (-/ matrix . row-col)
  [(_ (list row col)) (vector-ref (vector-ref matrix row) col)]
  [(_ (list (cons row col))) (vector-ref (vector-ref matrix row) col)])

(define (out-of-range? matrix row-col)
  (define height (vector-length matrix))
  (define width (vector-length (vector-ref matrix 0)))
  (match row-col [(cons x y) (not (and (<= 0 x (sub1 height)) (<= 0 y (sub1 width))))]))

(define (run-simulation get-neighbors occupied-threshold matrix)
  (define height (vector-length matrix))
  (define width (vector-length (vector-ref matrix 0)))
  (define occupied? (curry equal? #\#))

  (define (run-simulation-iter current-matrix)
    (define (next-cell-state row col)
      (define neighbors (get-neighbors current-matrix row col))
      (define cell (-/ current-matrix row col))
      (match cell [#\L #:when (stream-andmap (compose not occupied?) neighbors) #\#]
                  [#\# #:when (<= occupied-threshold (stream-count occupied? neighbors)) #\L]
                  [else cell]))

    (define (next-row-state row)
      (build-vector width (curry next-cell-state row)))

    (let ([next-matrix (build-vector height next-row-state)])
         (if (equal? current-matrix next-matrix) current-matrix (run-simulation-iter next-matrix))))

  (run-simulation-iter matrix))

(define (count-occupied-seats matrix)
  (for/sum ([row (in-vector matrix)]) (vector-count (curry equal? #\#) row)))

(define (solution-part1 input)
  (define (adjacent-cells matrix row col)
    (for/stream ([row-col (in-list (list (cons (sub1 row) (sub1 col)) (cons (sub1 row) col)
                                         (cons (sub1 row) (add1 col)) (cons row (add1 col))
                                         (cons (add1 row) (add1 col)) (cons (add1 row) col)
                                         (cons (add1 row) (sub1 col)) (cons row (sub1 col))))]
                 #:unless (out-of-range? matrix row-col))
                (-/ matrix row-col)))

  (count-occupied-seats (run-simulation adjacent-cells 4 (parse-result input-parser input))))

(define (solution-part2 input)
  (define (in-sight-cells matrix row col)
    (define (iterate start-row start-col row-step col-step)
      (generator () (let loop ([x start-row] [y start-col])
                              (yield (cons x y))
                              (loop (+ x row-step) (+ y col-step)))))

    (define (lookup-path row-step col-step)
      (for/first ([row-col (in-producer (iterate (+ row row-step) (+ col col-step) row-step col-step)
                                        (curry out-of-range? matrix))]
                  #:unless (equal? (-/ matrix row-col) #\.))
                 (-/ matrix row-col)))

    (stream-filter char? (stream (lookup-path -1 -1) (lookup-path -1 0) (lookup-path -1 1) (lookup-path 0 1)
                                 (lookup-path 1 1) (lookup-path 1 0) (lookup-path 1 -1) (lookup-path 0 -1))))

  (count-occupied-seats (run-simulation in-sight-cells 5 (parse-result input-parser input))))