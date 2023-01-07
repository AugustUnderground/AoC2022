#lang racket

(require algorithms)
(require relation/function)
(require threading)

(define (nub l)        (~> l list->set set->list))
(define (member? l e)  (~> e (member l) list?))
(define (n-rows m)     (length m))
(define (n-cols m)     (length (car m)))
(define (mref m p)     (~> m (list-ref (car p)) (list-ref (cdr p))))
(define (mset m p v)   (list-set m (car p)
                                   (list-set (list-ref m (car p)) (cdr p) v)))
(define (midx m v)     (apply append
                              (filter-not empty?
                                          (zip-with (λ (r cs)
                                                       (map (partial cons r) cs))
                                                    (range (n-rows m))
                                                    (map (λ (r) (indexes-of r v))
                                                         m)))))

(define lines (file->lines "./day12.txt"))

(define mtx (map (λ (l) (~>> l string->list
                               (map char->integer)
                               (map (λ (c) (- c 96)))
                               (map (partial max 0))))
                 lines))

(define-values (s t) (apply values (midx mtx 0)))

(define heights (mset (mset mtx t 26) s 1))

(define (cross coord)
  (let* ([r     (car coord)]
         [c     (cdr coord)]
         [north (cons (- r 1) c)]
         [south (cons (+ r 1) c)]
         [west  (cons    r (- c 1))]
         [east  (cons    r (+ c 1))])
    (list east south north west)))

(define (mx m p)
  (let* ([nr (n-rows m)]
         [nc (n-cols m)]
         [rv (mref m p)]
         [l (λ (e) (let* ([y (car e)] [x (cdr e)])
                (and (>= y 0) (>= x 0) (< y nr) (< x nc)
                     (<= (mref m e) (+ rv 1)))))])
    (~>> p cross (filter l))))

(define (count-steps grid wave idx [inv #f])
  (let* ([front (filter (compose1 (partial = 0) (partial mref wave))
                        (nub (apply append (map (partial mx grid)
                                                (midx wave idx)))))]
         [next-idx (+ 1 idx)]
         [next-wave (foldl (λ (c w) (mset w c next-idx)) wave front)])
    (cond [(member? front t) idx] [(empty? front) idx] 
          [(and inv (any? (map (λ (f) (= 26 (mref grid f))) front))) idx]
          [else (count-steps grid next-wave next-idx inv)])))

(define init-wave (mset (repeat (n-rows heights)
                                (repeat (n-cols heights) 0)) s 1))
(define num-steps (count-steps heights init-wave 1))

(define inv-heights  (map (λ (r) (map (partial - 27) r)) heights))
(define inv-wave (mset (repeat (n-rows heights) (repeat (n-cols heights) 0)) t 1))
(define inv-num-steps (count-steps inv-heights inv-wave 1 #t))

(fprintf (current-output-port) "Puzzle 1: ~a\n" num-steps)
(fprintf (current-output-port) "Puzzle 2: ~a\n" inv-num-steps)
