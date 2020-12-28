#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day10.rkt")

(define test-input
"16
10
15
5
1
11
7
19
6
12
4")
(define test-input-2
"28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")
(define real-input (port->string (open-input-file "resources/input/day10.txt") #:close? #t))

(define/provide-test-suite day-10-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 35)
                      (check-equal? (solution-part1 test-input-2) 220)
                      (check-equal? (solution-part1 real-input) 2046))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 8)
                      (check-equal? (solution-part2 test-input-2) 19208)
                      (check-equal? (solution-part2 real-input) 1157018619904)))