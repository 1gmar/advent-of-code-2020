#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day14.rkt")

(define test-input
"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")
(define test-input-2
"mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")
(define real-input (port->string (open-input-file "resources/input/day14.txt") #:close? #t))

(define/provide-test-suite day-14-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 165)
                      (check-equal? (solution-part1 real-input) 17028179706934))
  (test-case "Part 2" (check-equal? (solution-part2 test-input-2) 208)
                      (check-equal? (solution-part2 real-input) 3683236147222)))