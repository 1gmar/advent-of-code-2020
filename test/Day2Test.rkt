#lang racket
(require
  (only-in rackunit check-equal?)
  (only-in racket/port port->string)
  "../src/Day2.rkt")

(define test-case "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n")
(define real-input (port->string (open-input-file "resources/input/day2.txt") #:close? #t))

(check-equal? (solution-part1 test-case) 2)
(check-equal? (solution-part1 real-input) 517)
(check-equal? (solution-part2 test-case) 1)
(check-equal? (solution-part2 real-input) 284)