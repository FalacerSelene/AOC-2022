#lang racket

(require threading
         "../utils.rkt")

(module+ test
  (require rackunit))

(define (read-grid input-lines)
  (for/list ([line (in-list input-lines)])
    (~>> line
         string->list
         (map char->integer)
         (map (curryr - (char->integer #\0))))))

(module+ test
  (test-case "read-grid"
    (check-equal? (read-grid '("123" "456" "7"))
                  '((1 2 3) (4 5 6) (7)))))

(define (visibles-from-left grid)
  (for/list ([line (in-list grid)])
    (define max+rev-visibles
      (foldl (λ (elem acc)
               (let ([biggest-so-far (car acc)]
                     [rest (cdr acc)])
                 (if (> elem biggest-so-far)
                   (cons elem (cons #t rest))
                   (cons biggest-so-far (cons #f rest)))))
             (cons -1 '())
             line))

    (reverse (cdr max+rev-visibles))))

(define (visibles-in-grid grid)
  (define vis visibles-from-left)
  (define ref reflect-rectangular-matrix)

  (define left-visible (vis grid))

  (define right-visible (~>> grid
                             (map reverse)
                             vis
                             (map reverse)))

  (define top-visible (~> grid
                          ref
                          vis
                          ref))

  (define bot-visible (~> grid
                          reverse
                          ref
                          vis
                          ref
                          reverse))

  (for/list ([ls (in-list left-visible)]
             [rs (in-list right-visible)]
             [ts (in-list top-visible)]
             [bs (in-list bot-visible)])
    (for/list ([l (in-list ls)]
               [r (in-list rs)]
               [t (in-list ts)]
               [b (in-list bs)])
      (or l r t b))))

(module+ test
  (test-case "visibles-in-grid"
    (check-equal? (visibles-in-grid (read-grid '("111" "101" "101")))
                  '((#t #t #t) (#t #f #t) (#t #t #t)))
    (let ([test-input (list "30373"
                            "25512"
                            "65332"
                            "33549"
                            "35390")])
      (define vis (visibles-in-grid (read-grid test-input)))
      (check-equal? vis '((#t #t #t #t #t)
                          (#t #t #t #f #t)
                          (#t #t #f #t #t)
                          (#t #f #t #f #t)
                          (#t #t #t #t #t))))))

(define/ec (scenic-score-at grid xref yref)
  (when (or (= xref 0)
            (= yref 0)
            (= xref (sub1 (length (car grid))))
            (= yref (sub1 (length grid))))
    (return 0))
  ;; Just do it imperative
  (define (get-at x y) (~> grid (list-ref y) (list-ref x)))
  (define this-height (get-at xref yref))
  (define-syntax-rule (inc! x) (set! x (+ x 1)))
  (define left 0)
  (for ([x (in-range (sub1 xref) -1 -1)])
    (define that-height (get-at x yref))
    (when (< that-height this-height)
      (inc! left))
    #:break (if (>= that-height this-height)
                (begin
                  (inc! left)
                  #t)
                #f)
    (void))

  (define right 0)
  (for ([x (in-range (add1 xref) (length (car grid)))])
    (define that-height (get-at x yref))
    (when (< that-height this-height)
      (inc! right))
    #:break (if (>= that-height this-height)
                (begin
                  (inc! right)
                  #t)
                #f)
    (void))

  (define up 0)
  (for ([y (in-range (sub1 yref) -1 -1)])
    (define that-height (get-at xref y))
    (when (< that-height this-height)
      (inc! up))
    #:break (if (>= that-height this-height)
                (begin
                  (inc! up)
                  #t)
                #f)
    (void))

  (define down 0)
  (for ([y (in-range (add1 yref) (length grid))])
    (define that-height (get-at xref y))
    (when (< that-height this-height)
      (inc! down))
    #:break (if (>= that-height this-height)
                (begin
                  (inc! down)
                  #t)
                #f)
    (void))

  (define scenic-score (* up down left right))
  scenic-score)

(define (max-scenic-score grid)
  (define m 0)
  (define ylen (length grid))
  (define xlen (length (car grid)))
  (for ([y (in-range ylen)]
        #:when #t
        [x (in-range xlen)])
    (define t (scenic-score-at grid x y))
    (when (> t m)
      (set! m t)))
  m)

(module+ main
  (define input (sequence->list (in-lines)))
  (define grid (read-grid input))
  (define visibles (visibles-in-grid grid))
  (define total
    (for/sum ([vs (in-list visibles)])
      (count identity vs)))
  (displayln (format "Part one: ~a" total))
  (displayln (format "Part two: ~a" (max-scenic-score grid)))
  )

