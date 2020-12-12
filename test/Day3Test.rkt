#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day3.rkt")

(define test-input
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")
(define real-input (port->string (open-input-file "resources/input/day3.txt") #:close? #t))

(define/provide-test-suite day-3-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 7)
                      (check-equal? (solution-part1 real-input) 252))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 336)
                      (check-equal? (solution-part2 real-input) 2608962048)))