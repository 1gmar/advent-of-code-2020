#lang racket
(require (only-in threading ~>> λ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by integer $eol)))

(define (solution-part1 input)

  (define/match (find-pair value lst [diff-map #hash()])
    [(_ '() _) '()]
    [(_ (cons x xs) _) (if (hash-has-key? diff-map x)
                           (list x (hash-ref diff-map x))
                           (find-pair value xs (hash-set diff-map (- value x) x)))])

  (~>> (parse-result input-parser input)
       (find-pair 2020)
       (apply *)))

(define (solution-part2 input)

  (define/match (find-triple lst diff-map)
    [('() _) '()]
    [((cons (cons index value) xs) _) (if (and (hash-has-key? diff-map value)
                                               (andmap (λ~>> car (= index) not) (hash-ref diff-map value)))
                                          (~>> (hash-ref diff-map value)
                                               (map cdr)
                                               (cons value))
                                          (find-triple xs diff-map))])

  (define/match (eval-complement comb)
    [((list (cons _ lhs) (cons _ rhs))) (- 2020 lhs rhs)])

  (let* ([ints (parse-result input-parser input)]
         [indexed-list (map cons (range (length ints)) ints)]
         [diff-map (for/hash ([comb (in-combinations indexed-list 2)])
                             (values (eval-complement comb) comb))])
        (apply * (find-triple indexed-list diff-map))))