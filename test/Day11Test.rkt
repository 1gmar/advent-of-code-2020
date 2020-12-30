#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day11.rkt")

(define test-input
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")
(define real-input (port->string (open-input-file "resources/input/day11.txt") #:close? #t))

(define/provide-test-suite day-11-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 37)
                      (check-equal? (solution-part1 real-input) 2243))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 26)
                      (check-equal? (solution-part2 real-input) 2027)))