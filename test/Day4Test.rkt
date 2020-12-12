#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day4.rkt")

(define test-input
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")
(define real-input (port->string (open-input-file "resources/input/day4.txt") #:close? #t))

(define/provide-test-suite day-4-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 2)
                      (check-equal? (solution-part1 real-input) 235))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 2)
                      (check-equal? (solution-part2 real-input) 194)))