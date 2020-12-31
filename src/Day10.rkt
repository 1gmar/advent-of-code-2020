#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (trim-spaces-eof (end-or-sep-by +integer $eol)))

(define (bucket-sort ratings)
  (for/fold ([buckets (make-vector (add1 (apply max ratings)))]
             #:result (for/list ([x (in-vector buckets)] #:when (positive? x)) x))
            ([rating (in-list ratings)])
            (begin (vector-set! buckets rating rating) buckets)))

(define/match (eval-diffs rating acc)
  [(_ (cons diffs prev)) (cons (append diffs (list (- rating prev))) rating)])

(define (solution-part1 input)
  (let* ([sorted-ratings (bucket-sort (parse-result input-parser input))]
         [diff-list (car (foldl eval-diffs '(() . 0) sorted-ratings))])
        (apply * (foldl (match-lambda** [(1 (list 1s 3s)) (list (add1 1s) 3s)]
                                        [(3 (list 1s 3s)) (list 1s (add1 3s))]) '(0 1) diff-list))))

(define (solution-part2 input)
  (define (split-on separator diff-list [diff-sections '()])
    (match/values (splitf-at diff-list (compose not (curry equal? separator)))
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
                (map (compose binary->decimal flatten) (interleave '(0 0 0) (decimal->binary x)))))
    (let* ([n (sub1 (length diff-section))]
           [invalid-variations (if (< n 3) 0 (set-count (apply set (flatten (gen-invalid-variations n)))))])
          (- (expt 2 n) invalid-variations)))

  (let* ([sorted-ratings (bucket-sort (parse-result input-parser input))]
         [diff-list (car (foldl eval-diffs '(() . 0) sorted-ratings))]
         [diff-sections (split-on 3 diff-list)])
        (apply * (map compute-section-variations diff-sections))))