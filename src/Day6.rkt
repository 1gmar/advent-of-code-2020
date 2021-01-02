#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([person-parser (many1 $letter)]
         [group-parser (end-or-sep-by person-parser $eol)])
        (trim-spaces-eof (end-or-sep-by group-parser $eol))))

(define (solution-part1 input)
  (~>> (parse-result input-parser input)
       (map (λ~>> flatten list->set set-count))
       (apply +)))

(define (solution-part2 input)
  (~>> (parse-result input-parser input)
       (map (λ~>> (map list->set) (apply set-intersect) set-count))
       (apply +)))