#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by +integer $eol)))

(define (bucket-sort ratings)
  (for/fold ([buckets (make-vector (add1 (apply max ratings)))]
             #:result (for/list ([x (in-vector buckets)] #:when (positive? x)) x))
            ([rating (in-list ratings)])
            (begin (vector-set! buckets rating rating) buckets)))

(define (collect-diffs ratings)
  (define/match (eval-diffs rating acc)
    [(_ (cons diffs prev)) (~>> (- rating prev)
                                list
                                (append diffs)
                                (cons _ rating))])
  (car (foldl eval-diffs '(() . 0) ratings)))

(define (solution-part1 input)

  (define/match (count-diffs diff acc)
    [(1 (list 1s 3s)) (list (add1 1s) 3s)]
    [(3 (list 1s 3s)) (list 1s (add1 3s))])

  (~>> (parse-result input-parser input)
       bucket-sort
       collect-diffs
       (foldl count-diffs '(0 1))
       (apply *)))

(define (solution-part2 input)

  (define (split-on separator diff-list [diff-sections '()])
    (match/values (splitf-at diff-list (λ~>> (= separator) not))
                  [('() '()) diff-sections]
                  [(head '()) (cons head diff-sections)]
                  [('() (cons _ tail)) (split-on separator tail diff-sections)]
                  [(head (cons _ tail)) (split-on separator tail (cons head diff-sections))]))

  (define (compute-section-variations diff-section)

    (define/match (interleave val lst)
      [(_ '()) (list (list val))]
      [(_ (cons x xs)) (cons (cons val lst) (map (curry cons x) (interleave val xs)))])

    (define/match (decimal->binary decimal)
      [(0) '()]
      [(_) (append (decimal->binary (quotient decimal 2)) (list (modulo decimal 2)))])

    (define (binary->decimal binary)

      (define/match (fold-bits bit acc)
        [(_ (cons i decimal)) (cons (add1 i) (+ (* (expt 2 i) bit) decimal))])

      (cdr (foldr fold-bits '(0 . 0) binary)))

    (define (gen-invalid-variations n)
      (for/list ([x (in-range (expt 2 (- n 3)))])
                (~>> (decimal->binary x)
                     (interleave '(0 0 0))
                     (map (λ~>> flatten binary->decimal)))))

    (define (count-invalid-variations-for n)
      (~>> (gen-invalid-variations n)
           flatten
           (apply set)
           set-count))

    (let* ([n (sub1 (length diff-section))]
           [invalid-variations (if (< n 3) 0 (count-invalid-variations-for n))])
          (- (expt 2 n) invalid-variations)))

  (~>> (parse-result input-parser input)
       bucket-sort
       collect-diffs
       (split-on 3)
       (map compute-section-variations)
       (apply *)))