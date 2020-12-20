#lang racket
(require "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define input-parser
  (let* ([operation (>>= (oneOfStrings "nop" "acc" "jmp") (compose return string->symbol list->string))]
         [line-parser (parser-seq operation (~ $spaces) integer #:combine-with cons)])
        (>>= (trim-spaces-eof (end-or-sep-by line-parser $eol))
             (compose return vector->immutable-vector list->vector))))

(define (run-program program [iptr 0] [acc 0] [visited-iptrs (set)])
  (define/match (eval-instr instr)
    [((cons 'nop _)) (run-program program (add1 iptr) acc (set-add visited-iptrs iptr))]
    [((cons 'acc value)) (run-program program (add1 iptr) (+ acc value) (set-add visited-iptrs iptr))]
    [((cons 'jmp value)) (run-program program (+ iptr value) acc (set-add visited-iptrs iptr))])
  (cond [(set-member? visited-iptrs iptr) (cons acc #f)]
        [(equal? (vector-length program) iptr) (cons acc #t)]
        [else (eval-instr (vector-ref program iptr))]))

(define (solution-part1 input)
  (car (run-program (parse-result input-parser input))))

(define (solution-part2 input)
  (define/match (flip-instr instr)
    [((cons 'nop int)) (cons 'jmp int)]
    [((cons 'jmp int)) (cons 'nop int)])
  (define (vector-set vect pos value)
    (list->vector (list-set (vector->list vect) pos value)))
  (define/match (find-termination-acc program nops-jmps-poss)
    [(_ '()) 0]
    [(_ (cons pos poss)) (match-let* ([instr (flip-instr (vector-ref program pos))]
                                      [(cons acc terminated?) (run-program (vector-set program pos instr))])
                                     (if terminated? acc (find-termination-acc program poss)))])
  (let* ([program (parse-result input-parser input)]
         [indexed-program (map cons (range (vector-length program)) (vector->list program))]
         [nops-jmps-poss (map car (filter-not (compose (curry equal? 'acc) cadr) indexed-program))])
        (find-termination-acc program nops-jmps-poss)))