#lang racket

(require 
  (only-in rackunit check-equal?)
  (only-in racket/port port->string)
  "../src/Day1.rkt")

(define test-case "1721\n979\n366\n299\n675\n1456")
(define real-input (port->string (open-input-file "resources/input/day1.txt") #:close? #t))

(check-equal? (solution-part1 test-case) 514579)
(check-equal? (solution-part1 real-input) 1019904)
(check-equal? (solution-part2 test-case) 241861950)
(check-equal? (solution-part2 real-input) 176647680)