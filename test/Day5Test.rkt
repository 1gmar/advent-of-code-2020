#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day5.rkt")

(define real-input (port->string (open-input-file "resources/input/day5.txt") #:close? #t))

(define/provide-test-suite day-5-suite
  (test-case "Part 1" (check-equal? (solution-part1 "FBFBBFFRLR") 357)
                      (check-equal? (solution-part1 "BFFFBBFRRR") 567)
                      (check-equal? (solution-part1 "FFFBBBFRRR") 119)
                      (check-equal? (solution-part1 "BBFFBBFRLL") 820)
                      (check-equal? (solution-part1 real-input) 890))
  (test-case "Part 2" (check-equal? (solution-part2 real-input) 651)))