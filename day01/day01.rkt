#lang racket

(define lines (file->lines "./day01.txt"))
(define split-idx (indexes-of lines ""))
(define empty-line? (lambda (l) (not (string=? "" l))))

(define puzzle
  (lambda (ls cs top)
    (let* ([cals (map string->number (takef ls empty-line?))]
           [total-elf (cons (foldl + 0 cals) cs)])
      (if (> (length ls) (+ (length cals) 1))
        (puzzle (drop ls (+ (length cals) 1)) total-elf top)
        (foldl + 0 (take (sort total-elf >) top))))))

(define max-cals (puzzle lines '() 1))
(define top-cals (puzzle lines '() 3))

(fprintf (current-output-port) "Puzzle 1: Maximum number of calories: ~a\n" max-cals)
(fprintf (current-output-port) "Puzzle 2: Top 3 Total of calories: ~a\n" top-cals)
