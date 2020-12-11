#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define (int-range-parser len low high)
  (>>= (parser-count len $digit)
       (λ (digits) (if (<= low (chars->number digits) high) (return digits) $err))))

(define (height-parser len low high height-unit)
  (parser-seq (int-range-parser len low high) (string height-unit) $eof #:combine-with append))

(define field-parser-map
  (hash "byr" (try-or-else (int-range-parser 4 1920 2002) empty)
        "iyr" (try-or-else (int-range-parser 4 2010 2020) empty)
        "eyr" (try-or-else (int-range-parser 4 2020 2030) empty)
        "pid" (try-or-else (parser-one (~> (parser-count 9 $digit)) $eof) empty)
        "ecl" (try-or-else (parser-one (~> (oneOfStrings "amb" "blu" "brn" "gry" "grn" "hzl" "oth")) $eof) empty)
        "hgt" (try-or-else (any-try (height-parser 3 150 193 "cm") (height-parser 2 59 76 "in")) empty)
        "hcl" (try-or-else (parser-seq (char #\#) (parser-count 6 (any-try $digit (oneOf "abcdef"))) (~ $eof)
                                       #:combine-with cons) empty)))

(define mandatory-fields (hash-keys field-parser-map))

(define input-parser
  (let* ([return-string (λ (chars) (return (list->string chars)))]
         [passport-field-keys (>>= (apply oneOfStrings (cons "cid" mandatory-fields)) return-string)]
         [passport-field-values (>>= (many1 (<any> $alphaNum (char #\#))) return-string)]
         [passport-field (parser-seq passport-field-keys (~ (char #\:)) passport-field-values #:combine-with cons)]
         [passport-parser (>>= (end-or-sep-by passport-field (<any> $eol $spaces))
                               (λ (kv-pairs) (return (make-immutable-hash kv-pairs))))])
        (trim-spaces-eof (end-or-sep-by passport-parser $eol))))

(define (field-value-valid? passport field-key)
  (and (hash-has-key? passport field-key)
       (not (empty? (parse-result (hash-ref field-parser-map field-key) (hash-ref passport field-key))))))

(define (passport-valid? criteria passport)
  (andmap (λ (field-key) (criteria passport field-key)) mandatory-fields))

(define (solution-part1 input)
  (count (curry passport-valid? hash-has-key?) (parse-result input-parser input)))

(define (solution-part2 input)
  (count (curry passport-valid? field-value-valid?) (parse-result input-parser input)))