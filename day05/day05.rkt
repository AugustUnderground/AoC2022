#lang racket

(define lines (file->lines "./day05.txt"))

(match-define-values (s m) (split-at lines (index-of lines "")))

(define moves (map (lambda (ms) (filter-map string->number ms))
                   (cdr (map (lambda (mv) (string-split mv " ")) m))))

(define num-stacks (filter-map string->number (string-split (last s) " " #:trim? #t)))

(define rows (map (lambda (c) 
                    (filter (lambda (k) (not (string-ci=? k ""))) 
                            (string-split 
                              (string-replace 
                                (string-replace 
                                  (string-replace c "    " "[0] ") 
                                  "[" " ") 
                                "]" " ") 
                              " " #:trim? #t))) 
                  (drop-right s 1)))

(define stacks (map (lambda (col) 
                      (filter (lambda (c) (not (string-ci=? c "0"))) col))
                    (apply map list rows)))

(define (move-9000 n f t ss)
  (if (<= n 0) ss
    (let* ([from (list-ref ss f)]
           [to (list-ref ss t)]
           [f-not-empty (not (null? from))]
           [ns (list-update (if f-not-empty (list-update ss f rest) ss)
                            t (lambda (x) (if f-not-empty (cons (first from) x) x)))])
        (move-9000 (- n 1) f t ns))))

(define (make-moves-9000 ms ss)
  (if (null? ms) ss
    (let* ([n (first (car ms))]
           [f (- (second (car ms)) 1)]
           [t (- (third (car ms)) 1)])
    (make-moves-9000 (cdr ms) (move-9000 n f t ss)))))

(define stacks-9000 (make-moves-9000 moves stacks))
(define res-9000 (apply string-append (map first stacks-9000)))

(define (move-9001 n f t ss)
    (let* ([from (list-ref ss f)]
           [to (list-ref ss t)])
        (list-update (list-update ss f (lambda (ff) (drop ff n))) t
                            (lambda (tt) (append (take from n) tt)))))

(define (make-moves-9001 ms ss)
  (if (null? ms) ss
    (let* ([n (first (car ms))]
           [f (- (second (car ms)) 1)]
           [t (- (third (car ms)) 1)])
    (make-moves-9001 (cdr ms) (move-9001 n f t ss)))))

(define stacks-9001 (make-moves-9001 moves stacks))

(define res-9001 (apply string-append (map first stacks-9001)))

(fprintf (current-output-port) "Puzzle 1: ~a\n" res-9000)
(fprintf (current-output-port) "Puzzle 2: ~a\n" res-9001)
