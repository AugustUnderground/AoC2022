#lang racket

(require algorithms)

(define lines (file->lines "./day09.txt"))

(define head-motions (map (lambda (l)
                            (let ([m (string-split l)])
                              (cons (first m) (string->number (second m)))))
                          lines))

(define (distance c1 c2)
  (let* ([dx (abs (- (car c1) (car c2)))]
         [dy (abs (- (cdr c1) (cdr c2)))])
    (cons dx dy)))

(define (touching? c1 c2)
  (let* ([d (distance c1 c2)]
         [dx (car d)]
         [dy (cdr d)])
    (<= (+ dx dy) 1)))

(define (diagonal? c1 c2)
  (let* ([d  (distance c1 c2)]
         [dx (car d)]
         [dy (cdr d)]
         [x1 (car c1)]
         [x2 (car c2)]
         [y1 (cdr c1)]
         [y2 (cdr c2)])
    (and (not (= x1 x2))
         (not (= y1 y2))
         (= (+ dx dy) 2))))

(define (aligned? c1 c2)
  (or (= (car c1) (car c2)) (= (cdr c1) (cdr c2))))

(define (around c)
  (let* ([x (car c)]
         [y (cdr c)])
    (list (cons (+ x 1)   y)
          (cons (- x 1)   y)
          (cons    x   (+ y 1))
          (cons    x   (- y 1)))))

(define (cross c)
  (let* ([x (car c)]
         [y (cdr c)])
    (list (cons (+ x 1)   (+ y 1))
          (cons (+ x 1)   (- y 1))
          (cons (- x 1)   (+ y 1))
          (cons (- x 1)   (- y 1)))))

(define (step m p)
    (cond [(string=? (car m) "L")
           (cons (- (car p) 1) (cdr p))]
          [(string=? (car m) "R")
           (cons (+ (car p) 1) (cdr p))]
          [(string=? (car m) "U")
           (cons (car p) (+ (cdr p) 1))]
          [(string=? (car m) "D")
           (cons (car p) (- (cdr p) 1))]))

(define (diag-step h t) 
  (let* ([ds (filter (lambda (p) (diagonal? t p)) (around h))]
         [dds (filter (lambda (p) (diagonal? t p)) (cross h))])
   (if (empty? ds) (car dds) (car ds))))

(define (follow-step h t)
  (let* ([xh (car h)]
         [yh (cdr h)]
         [xt (car t)]
         [yt (cdr t)])
    (cond [(and (= xh xt) (> yh yt)) (cons xt (+ yt 1))]
          [(and (= xh xt) (< yh yt)) (cons xt (- yt 1))]
          [(and (> xh xt) (= yh yt)) (cons (+ xt 1) yt)]
          [(and (< xh xt) (= yh yt)) (cons (- xt 1) yt)])))

(define (follow h t)
  (cond [(touching? t h) t]
        [(aligned? t h) (follow-step h t)]
        [(diagonal? t h) t]
        [else (diag-step h t)]))

(define (move ms hps tps)
  (if (empty? ms) tps
    (let* ([m   (car ms)]
           [ms- (if (<= (cdr m) 1) (cdr ms)
                    (cons (cons (car m) (- (cdr m) 1)) (cdr ms)))]
           [hp (car hps)]
           [hp- (step m hp)]
           [hps- (cons hp- hps)]
           [tp (car tps)]
           [tp- (follow hp- tp)]
           [tps- (cons tp- tps)])
      (move ms- hps- tps-))))

(define tail-positions (move head-motions '((0 . 0)) '((0 . 0))))

(define uniq-1 (length (set->list (list->set tail-positions))))

(define (follow-2 ps ns)
  (if (empty? ps) 
      (reverse (drop-right ns 1))
    (follow-2 (cdr ps) (cons (follow (car ns) (car ps)) ns))))

(define (move-2 ms hps tps)
  (if (empty? ms) tps
    (let* ([m   (car ms)]
           [ms- (if (<= (cdr m) 1) (cdr ms)
                       (cons (cons (car m) (- (cdr m) 1)) (cdr ms)))]
           [hp (car hps)]
           [hp- (step m hp)]
           [hps- (cons hp- hps)]
           [tps- (zip-with cons (follow-2 (map car tps) (list hp-)) tps)])
      (move-2 ms- hps- tps-))))

(define tail-len 9)

(define long-tail-positions (move-2 head-motions '((0 . 0)) (repeat tail-len '(( 0 . 0 )))))

(define uniq-2 (length (set->list (list->set (last long-tail-positions)))))

(fprintf (current-output-port) "Puzzle 1: ~a\n" uniq-1)
(fprintf (current-output-port) "Puzzle 2: ~a\n" uniq-2)
