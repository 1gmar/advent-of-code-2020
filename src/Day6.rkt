#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([person-parser (many1 $letter)]
         [group-parser (end-or-sep-by person-parser $eol)])
        (trim-spaces-eof (end-or-sep-by group-parser $eol))))

(define (solution-part1 input)
  (apply + (map (compose set-count list->set flatten) (parse-result input-parser input))))

(define (solution-part2 input)
  (let ([count-yes-answers (compose set-count (curry apply set-intersect) (curry map list->set))])
       (apply + (map count-yes-answers (parse-result input-parser input)))))