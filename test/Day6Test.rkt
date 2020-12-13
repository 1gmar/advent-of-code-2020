#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day6.rkt")

(define test-input
"abc

a
b
c

ab
ac

a
a
a
a

b")
(define real-input (port->string (open-input-file "resources/input/day6.txt") #:close? #t))

(define/provide-test-suite day-6-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 11)
                      (check-equal? (solution-part1 real-input) 6291))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 6)
                      (check-equal? (solution-part2 real-input) 3052)))