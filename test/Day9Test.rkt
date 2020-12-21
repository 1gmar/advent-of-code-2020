#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day9.rkt")

(define test-input
"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")
(define real-input (port->string (open-input-file "resources/input/day9.txt") #:close? #t))

(define/provide-test-suite day-9-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input 5) 127)
                      (check-equal? (solution-part1 real-input 25) 552655238))
  (test-case "Part 2" (check-equal? (solution-part2 test-input 127) 62)
                      (check-equal? (solution-part2 real-input 552655238) 70672245)))