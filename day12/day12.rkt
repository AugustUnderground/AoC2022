#lang racket

(require algorithms)
(require relation/function)
(require threading)

(define (list->pair l) (cons (first l) (second l)))
(define (nub l) (~> l list->set set->list))

(define lines (file->lines "./day12ex.txt"))

(define heights (map (λ (l) (~>> l string->list
                                   (map char->integer)
                                   (map (λ (c) (- c 96)))
                                   (map (partial max 0))))
                     lines))

(define (n-rows m)   (length m))
(define (n-cols m)   (length (car m)))
(define (mref m p)   (~> m (list-ref (car p)) (list-ref (cdr p))))
(define (mset m p v) (list-set m (car p) (list-set (list-ref m (car p)) (cdr p) v)))
(define (midx m v)   (map list->pair (filter (λ (p) (all? p))
                                             (zip (range (n-rows m))
                                                  (map (λ (r) (index-of r v)) m)))))


(define-values (s t) (apply values (midx heights 0)))

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
         [l (λ (e) 
               (let* ([y (car e)]
                      [x (cdr e)]
                      [v (mref m e)])
                 (and (>= y 0) (>= x 0) (< y nr) (< x nc) (<= v (+ rv 1)))))])
    (~>> p cross (filter l))))

(define (find-path grid path)
    (let* ([next-points (mx grid (car path))])
      (cond [(member t next-points) (cons t path)]
            [(empty? next-points) '()]
            [else (find-path )]
        )
      (find-path grid new-path (+ steps 1))))

(define p (cons 2 2))
(member '(1 . 5) (cross t))

(define v (mref heights p))

(define foo
  (cons p (mx heights p))
)

(filter (λ (f) (<= (mref heights f) (+ v 1)))  foo)

(define bar (nub (foldl append '() (map (partial mx heights) foo))))




;(fprintf (current-output-port) "Puzzle 2: ~a\n" monkey-business-long)
