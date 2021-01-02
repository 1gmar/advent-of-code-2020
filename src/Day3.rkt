#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([return-vector (λ~>> (apply vector-immutable) return)]
         [line-parser (>>= (many1 (oneOf ".#")) return-vector)])
        (>>= (trim-spaces-eof (end-or-sep-by line-parser $eol)) return-vector)))

(define (count-trees-for-slope map-matrix slope)
  (define height (vector-length map-matrix))
  (define width (vector-length (vector-ref map-matrix 0)))

  (define (tree? row col)
    (eq? #\# (vector-ref (vector-ref map-matrix row) (modulo col width))))

  (match slope [(cons row-step col-step) (for/sum ([row (in-range row-step height row-step)]
                                                   [col (in-range col-step (* height col-step) col-step)]
                                                   #:when (tree? row col))
                                                  1)]))

(define (solution-part1 input)
  (count-trees-for-slope (parse-result input-parser input) '(1 . 3)))

(define (solution-part2 input)
  (let ([map-matrix (parse-result input-parser input)])
       (~>> (list '(1 . 1) '(1 . 3) '(1 . 5) '(1 . 7) '(2 . 1))
            (map (curry count-trees-for-slope map-matrix))
            (apply *))))