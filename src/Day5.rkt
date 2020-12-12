#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([format-binary (λ (chars) (append '(#\# #\b) chars))]
         [char-return (λ (chr bit) (>>= (char chr) (λ (_) (return bit))))]
         [row-parser (parser-count 7 (<any> (char-return #\F #\0) (char-return #\B #\1)))]
         [col-parser (parser-count 3 (<any> (char-return #\L #\0) (char-return #\R #\1)))]
         [boarding-pass-parser (parser-compose (row <- row-parser) (col <- col-parser)
                                               (return (map (compose chars->number format-binary) (list row col))))])
        (trim-spaces-eof (end-or-sep-by boarding-pass-parser $eol))))

(define (eval-seat-id row col)
  (+ (* row 8) col))

(define (find-min-max lst)
  (foldl (λ (x min-max) (cons (min (car min-max) x) (max (cdr min-max) x))) '(1024 . 0) lst))

(define (solution-part1 input)
  (apply max (map (curry apply eval-seat-id) (parse-result input-parser input))))

(define (solution-part2 input)
  (match-let* ([seat-ids (map (curry apply eval-seat-id) (parse-result input-parser input))]
               [(cons min-id max-id) (find-min-max seat-ids)])
              (set-first (set-subtract (for/set ([x (in-range min-id max-id)]) x) (list->set seat-ids)))))