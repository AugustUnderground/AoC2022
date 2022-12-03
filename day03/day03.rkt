#lang racket

(define lines (file->lines "./day03.txt"))
(define split-idx (map (lambda (l) (/ (string-length l) 2)) lines))

(define compartment-1 (map (lambda (l i) (string->list (substring l 0 i))) lines split-idx))
(define compartment-2 (map (lambda (l i) (string->list (substring l i (string-length l)))) lines split-idx))

(define wrong-items (map (lambda (c1 c2) (car (set-intersect c1 c2))) compartment-1 compartment-2))

(define (priority i) (- (char->integer i) (if (char-lower-case? i) 96 38)))

(define prio (map priority wrong-items))

(define sum-1 (foldl + 0 prio))

(define (chunks xs n) (if (null? xs) '() (cons (take xs n) (chunks (drop xs n) n))))

(define groups (chunks (map string->list lines) 3))

(define badges (map (lambda (g) (car (apply set-intersect g))) groups))

(define sum-2 (foldl + 0 (map priority badges)))

(fprintf (current-output-port) "Puzzle 1: ~a\n" sum-1)
(fprintf (current-output-port) "Puzzle 2: ~a\n" sum-2)
