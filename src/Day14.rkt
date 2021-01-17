#lang racket
(require (only-in threading ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(struct block (bitmask assignments))

(define input-parser
  (let* ([bitmask-parser (parser-one (string "mask = ") (~> (parser-count 36 (oneOf "01X"))) $eol)]
         [assignment-parser (parser-seq (~ (string "mem[")) +integer
                                        (~ (string "] = ")) +integer
                                        (~ (many $eol)) #:combine-with cons)]
         [assignments-parser (many1Until assignment-parser (any-try (lookAhead (string "mask")) $eof))]
         [block-parser (parser-compose (bitmask <- bitmask-parser)
                                       (assignments <- assignments-parser)
                                       (return (block bitmask assignments)))])
        (trim-spaces-eof (many1 block-parser))))

(define (integer->36-bit-list int)
  (let ([bit-list (string->list (number->string int 2))])
       (~>> (- 36 (length bit-list))
            (make-list _ #\0)
            (append _ bit-list))))

(define (36-bit-list->integer bit-list)
  (~>> (append '(#\# #\b) bit-list) chars->number))

(define (run-program input in-addresses in-values)
  (for*/hash ([block (in-list (parse-result input-parser input))]
              [bitmask (in-value (block-bitmask block))]
              [assignment (in-list (block-assignments block))]
              [address (in-addresses bitmask assignment)]
              [value (in-values bitmask assignment)])
             (values address value)))

(define (sum-values memory)
  (apply + (hash-values memory)))

(define (solution-part1 input)

  (define (apply-bitmask bitmask int)

    (define (apply-bitwise mask-bit bit)
      (match mask-bit [#\X bit]
                      [else mask-bit]))

    (~>> (integer->36-bit-list int)
         (map apply-bitwise bitmask)
         36-bit-list->integer))

  (sum-values (run-program input (λ (_ assignment) (in-value (car assignment)))
                                 (λ (bitmask assignment) (in-value (apply-bitmask bitmask (cdr assignment)))))))

(define (solution-part2 input)

  (define (apply-bitmask bitmask address)

    (define (apply-bitwise mask-bit bit)
      (match mask-bit [#\X '(#\0 #\1)]
                      [#\1 '(#\1)]
                      [else (list bit)]))

    (define (collect-addresses addr-bit acc)
      (match addr-bit [(list bit) (map (curry cons bit) acc)]
                      ['(#\0 #\1) (append (map (curry cons #\0) acc)
                                          (map (curry cons #\1) acc))]))

    (~>> (integer->36-bit-list address)
         (map apply-bitwise bitmask)
         (foldr collect-addresses '(()))
         (map 36-bit-list->integer)))

  (sum-values (run-program input (λ (bitmask assignment) (in-list (apply-bitmask bitmask (car assignment))))
                                 (λ (_ assignment) (in-value (cdr assignment))))))