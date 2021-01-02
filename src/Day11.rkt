#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(struct seat-grid (matrix height width))

(define input-parser
  (let* ([new-vector (curry apply vector)]
         [wrap-matrix (λ (matrix) (seat-grid matrix (vector-length matrix) (vector-length (vector-ref matrix 0))))]
         [row-parser (>>= (many1 (oneOf ".L")) (λ~>> new-vector return))])
        (>>= (trim-spaces-eof (end-or-sep-by row-parser $eol)) (λ~>> new-vector wrap-matrix return))))

(define occupied? (curry eq? #\#))

(define/match (-/ grid row col)
  [((seat-grid matrix _ _) _ _) (vector-ref (vector-ref matrix row) col)])

(define (for/neighbors body-closure [when-closure (const #t)])
  (for*/list ([i (in-list '(-1 0 1))]
              [j (in-list '(-1 0 1))]
              #:unless (= 0 i j)
              #:when (when-closure i j))
             (body-closure i j)))

(define (run-simulation get-neighbors occupied-threshold grid)
  (define height (seat-grid-height grid))
  (define width (seat-grid-width grid))

  (define (run-simulation-iter current-grid)

    (define (next-cell-state row col)
      (define neighbors (get-neighbors current-grid row col))
      (define cell (-/ current-grid row col))

      (match cell [#\L #:when (andmap (λ~>> occupied? not) neighbors) #\#]
                  [#\# #:when (<= occupied-threshold (count occupied? neighbors)) #\L]
                  [else cell]))

    (define (next-row-state row)
      (build-vector width (curry next-cell-state row)))

    (let ([next-matrix (build-vector height next-row-state)]
          [current-matrix (seat-grid-matrix current-grid)])
         (if (equal? next-matrix current-matrix)
             current-matrix
             (run-simulation-iter (struct-copy seat-grid current-grid [matrix next-matrix])))))

  (run-simulation-iter grid))

(define (count-occupied-seats matrix)
  (for/sum ([row (in-vector matrix)]) (vector-count occupied? row)))

(define (solution-part1 input)

  (define (adjacent-cells grid row col)
    (define height (seat-grid-height grid))
    (define width (seat-grid-width grid))

    (for/neighbors (λ (i j) (-/ grid (+ row i) (+ col j)))
                   (λ (i j) (and (<= 0 (+ row i) (sub1 height))
                                 (<= 0 (+ col j) (sub1 width))))))

  (~>> (parse-result input-parser input)
       (run-simulation adjacent-cells 4)
       count-occupied-seats))

(define (solution-part2 input)

  (define (in-sight-cells grid row col)
    (define height (seat-grid-height grid))
    (define width (seat-grid-width grid))

    (define/match (dir-range direction coord size)
      [(-1 _ _) (in-range (sub1 coord) -1 -1)]
      [(0 _ _) (make-list size coord)]
      [(1 _ _) (in-range (add1 coord) size)])

    (define/match (look-around direction)
      [((cons row-dir col-dir)) (for/first ([i (dir-range row-dir row height)]
                                            [j (dir-range col-dir col width)]
                                            #:unless (eq? (-/ grid i j) #\.))
                                           (-/ grid i j))])

    (~>> (for/neighbors cons)
         (map look-around)
         (filter char?)))

  (~>> (parse-result input-parser input)
       (run-simulation in-sight-cells 5)
       count-occupied-seats))