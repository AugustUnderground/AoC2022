#lang racket

(define lines (file->lines "./day04.txt"))

(define (as-range input)
  (let* ([r (map string->number (string-split input "-"))]
         [s (car r)]
         [t (+ (cadr r) 1)])
    (range s t)))

(define ranges (map (lambda (l) (map as-range (string-split l ","))) lines))

(define (is-subset? s1 s2) (if (or (subset? s1 s2) (subset? s2 s1)) 1 0))
(define (intersect? s1 s2) (if (empty? (append (set-intersect s1 s2) (set-intersect s2 s1))) 0 1))

(define sum-1 (foldl + 0 (map (lambda (r) (apply is-subset? r)) ranges)))
(define sum-2 (foldl + 0 (map (lambda (r) (apply intersect? r)) ranges)))

(fprintf (current-output-port) "Puzzle 1: ~a\n" sum-1)
(fprintf (current-output-port) "Puzzle 2: ~a\n" sum-2)
