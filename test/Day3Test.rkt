#lang racket
(require
  (only-in rackunit check-equal?)
  (only-in racket/port port->string)
  "../src/Day3.rkt")

(define test-case
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

(check-equal? (solution-part1 test-case) 7)
(check-equal? (solution-part1 real-input) 252)
(check-equal? (solution-part2 test-case) 336)
(check-equal? (solution-part2 real-input) 2608962048)