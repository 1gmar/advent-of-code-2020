#lang racket
(require (only-in racket/generic define-generics)
         (only-in threading λ~>> ~>>)
         "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(define-generics navigable
  (move-dir navigable instr)
  (move-forward navigable distance)
  (rotate navigable instr)
  (get-pos navigable))

(struct point (x y))

(define input-parser
  (let* ([movement-parser (parser-cons (oneOf "NESWF") +integer)]
         [degrees-parser (>>= (oneOfStrings "90" "180" "270") (λ~>> chars->number return))]
         [rotation-parser (parser-cons (oneOf "LR") degrees-parser)]
         [line-parser (<any> movement-parser rotation-parser)])
        (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define (manhattan-distance ship)
  (match-let ([(point x y) (get-pos ship)]) (+ (abs x) (abs y))))

(define/match (move instr pos)
  [((cons #\N distance) (point x y)) (point x (+ y distance))]
  [((cons #\E distance) (point x y)) (point (+ x distance) y)]
  [((cons #\S distance) (point x y)) (point x (- y distance))]
  [((cons #\W distance) (point x y)) (point (- x distance) y)])

(define/match (execute-instruction instr ship)
  [((cons (or #\N #\E #\S #\W) _) _) (move-dir ship instr)]
  [((cons #\F distance) _) (move-forward ship distance)]
  [((cons (or #\R #\L) _) _) (rotate ship instr)])

(define (find-final-position input ship)
  (~>> (parse-result input-parser input)
       (foldl execute-instruction ship)
       manhattan-distance))

(define (solution-part1 input)

  (struct ship (pos dir)
    #:methods gen:navigable
    [(define/match (move-dir _ship instr)
       [((ship pos _) _) (struct-copy ship _ship [pos (move instr pos)])])
     (define/match (move-forward _ship distance)
       [((ship pos dir) _) (struct-copy ship _ship [pos (move (cons dir distance) pos)])])
     (define/match (rotate _ship instr)
       [((ship _ dir) _) (struct-copy ship _ship [dir (rotate-dir instr dir)])])
     (define get-pos (λ~>> ship-pos))])

  (define direction-vector (vector-immutable #\N #\E #\S #\W))

  (define (rotate-dir instr dir)

    (define (eval-direction rot-f degrees)
      (~>> (quotient degrees 90)
           (rot-f (vector-member dir direction-vector))
           (modulo _ 4)
           (vector-ref direction-vector)))

    (match instr [(cons #\R degrees) (eval-direction + degrees)]
                 [(cons #\L degrees) (eval-direction - degrees)]))

  (find-final-position input (ship (point 0 0) #\E)))

(define (solution-part2 input)

  (struct ship (pos dir waypoint)
    #:methods gen:navigable
    [(define/match (move-dir _ship instr)
       [((ship pos _ waypoint) _) (struct-copy ship _ship [waypoint (move instr waypoint)])])
     (define/match (move-forward _ship distance)
       [((ship pos _ waypoint) _) (struct-copy ship _ship [pos (move-to-waypoint pos waypoint distance)])])
     (define/match (rotate _ship instr)
       [((ship _ dir waypoint) _) (struct-copy ship _ship [waypoint (rotate-waypoint instr waypoint)])])
     (define get-pos (λ~>> ship-pos))])

  (define/match (move-to-waypoint pos waypoint distance)
    [((point x y) (point wp-x wp-y) _) (point (+ (* distance wp-x) x) (+ (* distance wp-y) y))])

  (define (rotate-waypoint instr waypoint)
    (match-define (point x y) waypoint)

    (define (rotate-coord rot-fun degrees)
      (let* ([z-init (make-rectangular x y)]
             [rotated-angle (rot-fun (angle z-init) (degrees->radians degrees))]
             [z-rotated (make-polar (magnitude z-init) rotated-angle)])
            (point (exact-round (real-part z-rotated)) (exact-round (imag-part z-rotated)))))

    (match instr [(cons #\R degrees) (rotate-coord - degrees)]
                 [(cons #\L degrees) (rotate-coord + degrees)]))

  (find-final-position input (ship (point 0 0) #\E (point 10 1))))