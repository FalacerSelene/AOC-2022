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

(module+ main
  (define input (sequence->list (in-lines)))
  (define grid (read-grid input))
  (define visibles (visibles-in-grid grid))
  (define total
    (for/sum ([vs (in-list visibles)])
      (count identity vs)))
  (displayln (format "Part one: ~a" total)))

