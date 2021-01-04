#lang racket
(require (only-in threading λ~>> ~>>) "util/ParseUtils.rkt")
(provide (contract-out [solution-part1 (-> string? integer?)]
                       [solution-part2 (-> string? integer?)]))

(struct point (x y))
(struct ship (pos dir))

(define input-parser
  (let* ([movement-parser (parser-cons (oneOf "NESWF") +integer)]
         [degrees-parser (>>= (oneOfStrings "90" "180" "270") (λ~>> chars->number return))]
         [rotation-parser (parser-cons (oneOf "LR") degrees-parser)]
         [line-parser (<any> movement-parser rotation-parser)])
        (trim-spaces-eof (end-or-sep-by line-parser $eol))))

(define/match (manhattan-distance pos)
  [((point x y)) (+ (abs x) (abs y))])

(define/match (move instr pos)
  [((cons #\N distance) (point x y)) (point x (+ y distance))]
  [((cons #\E distance) (point x y)) (point (+ x distance) y)]
  [((cons #\S distance) (point x y)) (point x (- y distance))]
  [((cons #\W distance) (point x y)) (point (- x distance) y)])

(define (solution-part1 input)
  (define direction-vector (vector-immutable #\N #\E #\S #\W))

  (define (rotate instr dir)

    (define (eval-direction rot-f degrees)
      (~>> (quotient degrees 90)
           (rot-f (vector-member dir direction-vector))
           (modulo _ 4)
           (vector-ref direction-vector)))

    (match instr [(cons #\R degrees) (eval-direction + degrees)]
                 [(cons #\L degrees) (eval-direction - degrees)]))

  (define/match (execute-instruction instr ship_)
    [((cons (or #\N #\E #\S #\W) _) (ship pos _)) (struct-copy ship ship_ [pos (move instr pos)])]
    [((cons #\F distance) (ship pos dir)) (struct-copy ship ship_ [pos (move (cons dir distance) pos)])]
    [((cons (or #\R #\L) _) (ship _ dir)) (struct-copy ship ship_ [dir (rotate instr dir)])])

  (~>> (parse-result input-parser input)
       (foldl execute-instruction (ship (point 0 0) #\E))
       ship-pos
       (manhattan-distance)))

(define (solution-part2 input)

  (define/match (move-ship _ship waypoint distance)
    [((ship (point sh-x sh-y) _) (point wp-x wp-y) _)
     (struct-copy ship _ship [pos (point (+ (* distance wp-x) sh-x)
                                         (+ (* distance wp-y) sh-y))])])

  (define (rotate instr waypoint)
    (match-define (point x y) waypoint)

    (define (trig-fun fun theta)
      (exact-round (fun (degrees->radians theta))))

    (define (rotate-coord degrees)
      (point (- (* (trig-fun cos degrees) x) (* (trig-fun sin degrees) y))
             (+ (* (trig-fun sin degrees) x) (* (trig-fun cos degrees) y))))

    (match instr [(cons #\R degrees) (rotate-coord (- 360 degrees))]
                 [(cons #\L degrees) (rotate-coord degrees)]))

  (define/match (execute-instruction instr ship-waypoint)
    [((cons (or #\N #\E #\S #\W) _) (cons ship waypoint)) (cons ship (move instr waypoint))]
    [((cons #\F distance) (cons ship waypoint)) (cons (move-ship ship waypoint distance) waypoint)]
    [((cons (or #\R #\L) _) (cons ship waypoint)) (cons ship (rotate instr waypoint))])

  (~>> (parse-result input-parser input)
       (foldl execute-instruction (cons (ship (point 0 0) #\E) (point 10 1)))
       ((λ~>> car ship-pos))
       (manhattan-distance)))