#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([return-vector (λ (lst) (return (apply vector-immutable lst)))]
         [line-parser (>>= (many1 (oneOf ".#")) return-vector)])
        (>>= (trim-spaces-eof (end-or-sep-by line-parser $eol)) return-vector)))

(define (count-trees-for-slope map-matrix slope)
  (let* ([row-step (first slope)]
         [column-step (second slope)]
         [height (vector-length map-matrix)]
         [width (vector-length (vector-ref map-matrix 0))]
         [step-in (λ (i step) (+ (* i step) step))]
         [tree? (λ (pos) (let ([row (first pos)] [column (modulo (second pos) width)])
                              (equal? #\# (vector-ref (vector-ref map-matrix row) column))))])
        (count tree? (takef (for/list ([i (in-range height)])
                                      (list (step-in i row-step) (step-in i column-step)))
                            (λ (pos) (< (first pos) height))))))

(define (solution-part1 input)
  (count-trees-for-slope (parse-result input-parser input) '(1 3)))

(define (solution-part2 input)
  (let ([map-matrix (parse-result input-parser input)])
       (apply * (map (λ (slope) (count-trees-for-slope map-matrix slope))
                     (list '(1 1) '(1 3) '(1 5) '(1 7) '(2 1))))))