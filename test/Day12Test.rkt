#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day12.rkt")

(define test-input
"F10
N3
F7
R90
F11")
(define real-input (port->string (open-input-file "resources/input/day12.txt") #:close? #t))

(define/provide-test-suite day-12-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 25)
                      (check-equal? (solution-part1 real-input) 1687))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 286)
                      (check-equal? (solution-part2 real-input) 20873)))