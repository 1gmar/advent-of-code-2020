#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day1.rkt")

(define test-input "1721\n979\n366\n299\n675\n1456")
(define test-input2 "1000\n20\n1500\n500\n521\n99")
(define real-input (port->string (open-input-file "resources/input/day1.txt") #:close? #t))

(define/provide-test-suite day-1-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 514579)
                      (check-equal? (solution-part1 real-input) 1019904))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 241861950)
                      (check-equal? (solution-part2 real-input) 176647680)
                      (check-equal? (solution-part2 test-input2) (* 1500 500 20))))