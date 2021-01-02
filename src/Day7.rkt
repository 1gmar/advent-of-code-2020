#lang racket
(require (only-in threading λ~>> ~> ~>>)
         (only-in graph directed-graph get-neighbors edge-weight)
         "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define color-parser
  (>>= (parser-seq (many1 $letter) (many1 $space) (many1 $letter) #:combine-with append)
       (λ~>> list->string return)))

(define comma-sep-bag-rules
  (sepBy1 (parser-seq +integer
                      (~ $spaces)
                      color-parser
                      (~ $spaces)
                      (~ (oneOfStrings "bags" "bag"))
                      #:combine-with cons)
          (string ", ")))

(define input-parser
  (let* ([rules-parser (<any> comma-sep-bag-rules (>> (string "no other bags") (return '())))]
         [line-parser (parser-seq color-parser
                                  (~ $spaces)
                                  (~ (string "bags contain"))
                                  (~ $spaces)
                                  rules-parser
                                  (~ (char #\.))
                                  #:combine-with (λ (color rules) (cons rules color)))])
        (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (create-graph bag-rules #:reverse-edges? [reverse-edges? #f])

  (define/match (collect-edges bag-rules acc)
    [((cons rules color) (cons edges weights)) (cons (append (cartesian-product (list color) (map cdr rules)) edges)
                                                     (append (map car rules) weights))])

  (match-let ([(cons edges weights) (foldl collect-edges '(() . ()) bag-rules)])
             (directed-graph (if reverse-edges? (map reverse edges) edges) weights)))

(define (solution-part1 input)

  (define (collect-parents graph parents [acc (set)])
    (cond [(set-empty? parents) acc]
          [else (collect-parents graph (~> parents (set-map (curry get-neighbors graph)) flatten list->set)
                                       (set-union parents acc))]))

  (let ([graph (create-graph (parse-result input-parser input) #:reverse-edges? #t)])
       (~>> (get-neighbors graph "shiny gold")
            list->set
            (collect-parents graph)
            set-count)))

(define (solution-part2 input)

  (define (count-total-bags graph source)
    (let* ([child-bags (get-neighbors graph source)]
           [weights (map (curry edge-weight graph source) child-bags)]
           [sum-recursively (λ (weight color result) (+ (* weight (count-total-bags graph color)) result))])
          (+ (apply + weights) (foldl sum-recursively 0 weights child-bags))))

  (let ([graph (create-graph (parse-result input-parser input))])
       (count-total-bags graph "shiny gold")))