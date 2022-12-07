#lang racket

(require nested-hash)

(define lines (file->lines "./day07.txt"))

(define (parse-lines ls current-path tree)
  (if (empty? ls) tree
    (let* ([line (car ls)]
           [tokens (string-split line)])
      (if (string=? "$" (car tokens)) 
        (cond [ (string=? (cadr tokens) "cd")
                (cond [(string=? (caddr tokens) "..")
                       (parse-lines (cdr ls) (cdr current-path) tree)]
                      [(string=? (caddr tokens) "/")
                       (parse-lines (cdr ls) '("/") tree)]
                      [else 
                       (let* ([path (cons (caddr tokens) current-path)])
                             (parse-lines (cdr ls) path tree))]) ]
              [ (string=? (cadr tokens) "ls")
                (let* ([contents (takef (cdr ls)
                                        (lambda (l)
                                          (not (string=? "$" (first (string-split l))))))]
                       [cataloque (lambda (c t)
                                    (let* ([s (string-split c)])
                                      (if (string=? (car s) "dir") t
                                          (nested-hash-set t (reverse (cons (cadr s) current-path))
                                                             (string->number (car s))))
                                    )) ])
                  (parse-lines (drop ls (+ 1 (length contents)))
                               current-path
                               (foldl cataloque tree contents))) ])
        (error "fucked up parsing")))))

(define file-tree (parse-lines lines '() (hash)))
(define dir-names (remove-duplicates (map (lambda (f) (apply string-append (drop-right f 1)))
                                          (hash-keys file-tree))))

(define (add-size sizes size path)
    (if (empty? path) sizes
      (let* ([key (apply string-append path)]
             [new-sizes (if (hash-has-key? sizes key)
                            (hash-set sizes key (+ (hash-ref sizes key) size))
                            (hash-set sizes key size))])
        (add-size new-sizes size (drop-right path 1)))))

(define (calculate-sizes paths sizes)
  (if (empty? paths) sizes
    (let* ([path (car paths)]
           [size (nested-hash-ref file-tree path)]
           [new-sizes (add-size sizes size (drop-right path 1))])
      (calculate-sizes (cdr paths) new-sizes))))

(define dir-sizes (calculate-sizes (hash-keys file-tree) (hash)))

(define puzzle-1 (foldl + 0 (filter (lambda (s) (<= s 100000))
                                    (hash-values dir-sizes))))

(fprintf (current-output-port) "Puzzle 1: ~a\n" puzzle-1)

(define total-space 70000000)
(define update-space 30000000)

(define unused-space (- total-space (hash-ref dir-sizes "/")))
(define needed-space (- update-space unused-space))

(define puzzle-2 (car (sort (filter (lambda (d) (>= d needed-space))
                                    (hash-values dir-sizes)) <)))

(fprintf (current-output-port) "Puzzle 2: ~a\n" puzzle-2)
