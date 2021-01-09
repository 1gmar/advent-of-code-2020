#lang racket
(require
  (only-in rackunit test-suite)
  (only-in rackunit/text-ui run-tests)
  "Day1Test.rkt" "Day2Test.rkt" "Day3Test.rkt" "Day4Test.rkt" "Day5Test.rkt" "Day6Test.rkt" "Day7Test.rkt"
  "Day8Test.rkt" "Day9Test.rkt" "Day10Test.rkt" "Day11Test.rkt" "Day12Test.rkt" "Day13Test.rkt")

(run-tests
  (test-suite "Advent of Code Test Suite"
    day-1-suite
    day-2-suite
    day-3-suite
    day-4-suite
    day-5-suite
    day-6-suite
    day-7-suite
    day-8-suite
    day-9-suite
    day-10-suite
    day-11-suite
    day-12-suite
    day-13-suite) 'verbose)