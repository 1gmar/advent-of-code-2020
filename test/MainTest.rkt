#lang racket
(require
  (only-in rackunit test-suite)
  (only-in rackunit/text-ui run-tests)
  "Day1Test.rkt" "Day2Test.rkt" "Day3Test.rkt" "Day4Test.rkt" "Day5Test.rkt" "Day6Test.rkt")

(run-tests
  (test-suite "Advent of Code Test Suite"
    day-1-suite
    day-2-suite
    day-3-suite
    day-4-suite
    day-5-suite
    day-6-suite) 'verbose)