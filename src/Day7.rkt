#lang racket
(require "util/ParseUtils.rkt" (only-in graph directed-graph get-neighbors edge-weight))
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define color-parser
  (>>= (parser-seq (many1 $letter) (many1 $space) (many1 $letter) #:combine-with append)
       (compose return list->string)))

(define comma-sep-bags
  (sepBy1 (parser-compose (qty <- +integer)
                          $spaces
                          (color <- color-parser)
                          $spaces
                          (oneOfStrings "bags" "bag")
                          (return (cons color qty)))
          (string ", ")))

(define input-parser
  (let* ([contents-parser (<any> comma-sep-bags (>> (string "no other bags") (return '())))]
         [line-parser (parser-compose (color <- color-parser)
                                      $spaces
                                      (string "bags contain")
                                      $spaces
                                      (contents <- contents-parser)
                                      (char #\.)
                                      (return (cons contents color)))])
        (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (create-graph bag-rules #:reverse-edges? [reverse-edges? #f])
  (define/match (collect-edges bag-rules acc)
    [((cons rules color) (cons edges weights)) (cons (append (cartesian-product (list color) (map car rules)) edges)
                                                     (append (map cdr rules) weights))])
  (match-let ([(cons edges weights) (foldl collect-edges '(() . ()) bag-rules)])
             (directed-graph (if reverse-edges? (map reverse edges) edges) weights)))

(define (solution-part1 input)
  (define (collect-parents graph parents [acc (set)])
    (cond [(set-empty? parents) acc]
          [else (collect-parents graph (list->set (flatten (set-map parents (curry get-neighbors graph))))
                                       (set-union parents acc))]))
  (let ([graph (create-graph (parse-result input-parser input) #:reverse-edges? #t)])
       (set-count (collect-parents graph (list->set (get-neighbors graph "shiny gold"))))))

(define (solution-part2 input)
  (define (count-total-bags graph source)
    (let* ([child-bags (get-neighbors graph source)]
           [weights (map (curry edge-weight graph source) child-bags)]
           [sum-recursively (λ (weight color result) (+ (* weight (count-total-bags graph color)) result))])
          (+ (apply + weights) (foldl sum-recursively 0 weights child-bags))))
  (let ([graph (create-graph (parse-result input-parser input))])
       (count-total-bags graph "shiny gold")))