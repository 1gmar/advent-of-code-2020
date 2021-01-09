#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let ([bus-id-parser (<any> +integer (skipMany1 (char #\x)))])
       (trim-spaces-eof (parser-compose (timestamp <- +integer)
                                        $eol
                                        (bus-ids <- (sepBy1 bus-id-parser (char #\,)))
                                        (return (cons bus-ids timestamp))))))

(define (solution-part1 input)
  (match-define (cons bus-ids timestamp) (parse-result input-parser input))

  (define (eval-earliest-timestamp bus-id)
    (~>> bus-id
         (/ timestamp)
         exact-ceiling
         (* bus-id)
         (cons bus-id)))

  (~>> (filter-not null? bus-ids)
       (map eval-earliest-timestamp)
       (argmin cdr)
       ((match-lambda [(cons bus-id bus-timestamp) (* (- bus-timestamp timestamp) bus-id)]))))

(define (solution-part2 input)

  (define (estimate-delays bus-ids)
    (for/list ([bus-delay (in-naturals)]
               [bus-id (in-list bus-ids)] #:unless (null? bus-id))
              (cons bus-id bus-delay)))

  (define/match (bus-starts-at-delay? timestamp _)
    [(_ (cons bus-id bus-delay)) (zero? (modulo (+ timestamp bus-delay) bus-id))])

  (define/match (find-1st-match-timestamp time-offset interval bus)
    [(_ _ (cons bus-id bus-delay)) (for/first ([timestamp (in-range time-offset +inf.0 interval)]
                                               #:when (bus-starts-at-delay? timestamp bus))
                                              timestamp)])

  (define/match (fold-bus-timestamps bus _)
    [((cons bus-id _) (cons time-offset interval)) (cons (find-1st-match-timestamp time-offset interval bus)
                                                         (* interval bus-id))])

  (~>> (parse-result input-parser input)
       car
       estimate-delays
       (foldl fold-bus-timestamps '(1 . 1))
       car))