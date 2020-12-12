#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day2.rkt")

(define test-input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n")
(define real-input (port->string (open-input-file "resources/input/day2.txt") #:close? #t))

(define/provide-test-suite day-2-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 2)
                      (check-equal? (solution-part1 real-input) 517))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 1)
                      (check-equal? (solution-part2 real-input) 284)))