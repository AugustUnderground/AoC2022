#lang racket

(define lines (file->lines "./day11.txt"))

(define (lines->monkeys ls)
  (if (empty? ls) ls
    (let* ([idx (indexes-of ls "")]
           [t (if (empty? idx) (length ls) (car idx))]
           [m (take ls t)]
           [ms (if (<= (length ls) t) '() (drop ls (+ 1 t)))])
      (cons m (lines->monkeys ms)))))

(define monkey-strings (lines->monkeys lines))

(define (parse-op str)
  (let* ([tkn (string-split (cadr (string-split str "=")))]
         [op (cadr tkn)]
         [v1 (car tkn)]
         [v2 (caddr tkn)]
         [expr (string-join (list op " " v1 " " v2) ""
                            #:before-first "("
                            #:after-last ")")])
    (eval
      (read
        (open-input-string
          (string-join (list "(lambda (old) " expr ")") "")))
      (make-base-namespace))))

(define (strings->monkeys m)
  (let* ([items (map (compose string->number string-trim) 
                  (string-split (second (string-split (second m) ":")) ","))]
         [operation (parse-op (third m))]
         [mod (string->number (last (string-split (fourth m))))]
         [target-true (string->number (last (string-split (fifth m))))]
         [target-false (string->number (last (string-split (sixth m))))]
         [test (lambda (w) (if (= 0 (modulo w mod)) target-true target-false))])
    (hash 'items items
          'operation operation
          'test test
          'inspections 0
          'mod mod)))

(define monkeys (map strings->monkeys monkey-strings))

(define (monkey-round ms #:mid [mid 0] #:relief [relief 3])
  (if (>= mid (length ms)) ms
    (let* ([m (list-ref ms mid)]
           [p (apply lcm (map (lambda (l) (hash-ref l 'mod)) ms))]
           [i (map (compose (lambda (j) (if (> j p) (modulo j p) j))
                            floor (lambda (w) (/ w relief))
                            (hash-ref m 'operation))
                   (hash-ref m 'items))]
           [t (map (hash-ref m 'test) i)]
           [m1 (list-set ms mid (hash-set (list-ref ms mid) 'items '()))]
           [m2 (list-set m1 mid (hash-update (list-ref m1 mid)
                                             'inspections
                                             (lambda (is) (+ is (length i)))))]
           [nm (foldl (lambda (a b r)
                        (list-set r b
                                  (hash-update
                                    (list-ref r b)
                                    'items (lambda (as) (cons a as)))))
                      m2 i t)])
      (monkey-round nm #:mid (+ 1 mid) #:relief relief))))

(define (inspections mp) 
    (sort (map (lambda (m) (hash-ref m 'inspections)) mp) >))

(define monkey-play (foldl (lambda (_ ms) (monkey-round ms)) monkeys (range 20)))

(define monkey-business (apply * (take (inspections monkey-play) 2)))

(define monkey-play-long (foldl (lambda (_ ms) (monkey-round ms #:relief 1))
                                monkeys (range 10000)))

(define monkey-business-long (apply * (take (inspections monkey-play-long) 2)))

(fprintf (current-output-port) "Puzzle 1: ~a\n" monkey-business)
(fprintf (current-output-port) "Puzzle 2: ~a\n" monkey-business-long)
