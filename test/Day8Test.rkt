#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day8.rkt")

(define test-input
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
(define real-input (port->string (open-input-file "resources/input/day8.txt") #:close? #t))

(define/provide-test-suite day-8-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 5)
                      (check-equal? (solution-part1 real-input) 1446))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 8)
                      (check-equal? (solution-part2 real-input) 1403)))