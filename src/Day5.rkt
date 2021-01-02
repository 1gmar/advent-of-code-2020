#lang racket
(require (only-in threading λ~>> ~> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define (seat-binary-parser size low-chr high-chr)

  (define (char->bit chr bit)
    (>> (char chr) (return bit)))

  (>>= (parser-count size (<any> (char->bit low-chr #\0) (char->bit high-chr #\1)))
       (λ~>> (append '(#\# #\b)) chars->number return)))

(define input-parser
  (let ([boarding-pass-parser (parser-cons (seat-binary-parser 7 #\F #\B) (seat-binary-parser 3 #\L #\R))])
       (trim-spaces-eof (end-or-sep-by boarding-pass-parser $eol))))

(define/match (eval-seat-id row-col)
  [((cons row col)) (+ (* row 8) col)])

(define (solution-part1 input)
  (~>> (parse-result input-parser input)
       (map eval-seat-id)
       (apply max)))

(define (solution-part2 input)

  (define (find-min-max lst)
    (foldl (match-lambda** [(x (cons x-min x-max)) (cons (min x-min x) (max x-max x))]) '(1024 . 0) lst))

  (match-let* ([seat-ids (map eval-seat-id (parse-result input-parser input))]
               [(cons min-id max-id) (find-min-max seat-ids)])
              (~> (range min-id max-id)
                  list->set
                  (set-subtract (list->set seat-ids))
                  set-first)))