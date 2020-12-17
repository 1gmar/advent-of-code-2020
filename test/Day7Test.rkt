#lang racket
(require (only-in rackunit check-equal? test-case define/provide-test-suite)
         "../src/Day7.rkt")

(define test-input
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")
(define test-input-2
"shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")
(define real-input (port->string (open-input-file "resources/input/day7.txt") #:close? #t))

(define/provide-test-suite day-7-suite
  (test-case "Part 1" (check-equal? (solution-part1 test-input) 4)
                      (check-equal? (solution-part1 real-input) 119))
  (test-case "Part 2" (check-equal? (solution-part2 test-input) 32)
                      (check-equal? (solution-part2 test-input-2) 126)
                      (check-equal? (solution-part2 real-input) 155802)))