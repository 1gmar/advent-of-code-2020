#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([return-vector (compose return (curry apply vector-immutable))]
         [line-parser (>>= (many1 (oneOf ".#")) return-vector)])
        (>>= (trim-spaces-eof (end-or-sep-by line-parser $eol)) return-vector)))

(define/match (count-trees-for-slope map-matrix slope)
  [(_ (cons row-step col-step))
   (let* ([height (vector-length map-matrix)]
          [width (vector-length (vector-ref map-matrix 0))]
          [tree? (match-lambda [(cons row col)
                                (equal? #\# (vector-ref (vector-ref map-matrix row) (modulo col width)))])])
         (count tree? (for/list ([row (in-range row-step height row-step)]
                                 [col (in-range col-step (* height col-step) col-step)])
                                (cons row col))))])

(define (solution-part1 input)
  (count-trees-for-slope (parse-result input-parser input) '(1 . 3)))

(define (solution-part2 input)
  (let ([map-matrix (parse-result input-parser input)])
       (apply * (map (curry count-trees-for-slope map-matrix)
                     (list '(1 . 1) '(1 . 3) '(1 . 5) '(1 . 7) '(2 . 1))))))