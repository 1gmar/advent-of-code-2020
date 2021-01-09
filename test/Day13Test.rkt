#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day13.rkt")

(define real-input (port->string (open-input-file "resources/input/day13.txt") #:close? #t))

(define/provide-test-suite day-13-suite
  (test-case "Part 1" (check-equal? (solution-part1 "939\n7,13,x,x,59,x,31,19") 295)
                      (check-equal? (solution-part1 real-input) 4135))
  (test-case "Part 2" (check-equal? (solution-part2 "939\n7,13,x,x,59,x,31,19") 1068781)
                      (check-equal? (solution-part2 "0\n17,x,13,19") 3417)
                      (check-equal? (solution-part2 "0\n67,7,59,61") 754018)
                      (check-equal? (solution-part2 "0\n67,x,7,59,61") 779210)
                      (check-equal? (solution-part2 "0\n67,7,x,59,61") 1261476)
                      (check-equal? (solution-part2 "0\n1789,37,47,1889") 1202161486)
                      (check-equal? (solution-part2 real-input) 640856202464541)))