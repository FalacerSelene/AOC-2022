#lang racket

(require racket/stxparam
         racket/unsafe/ops)

(provide define/ec
         lambda/ec
         return
         index-list
         sliding-window
         groups-of)

(module+ test
  (require rackunit))

(define-syntax-parameter return (syntax-rules ()))

(define-syntax lambda/ec
  (syntax-rules ()
    [(lambda/ec (args ...) body ...)
     (lambda (args ...)
       (let/ec ~>exit
         (syntax-parameterize ([return (syntax-rules () [(return x) (~>exit x)])])
           body ...)))]
    [(lambda/ec name (args ...) body ...)
     (lambda (args ...)
       (let/ec name
         body ...))]))

(define-syntax define/ec
  (syntax-rules ()
    [(define/ec (fname args ...) body ...)
     (define fname (lambda/ec (args ...) body ...))]
    [(define/ec name (fname args ...) body ...)
     (define fname (lambda/ec name (args ...) body ...))]))

(define (sliding-groups-of lst n slide-count)
  (let @ ([lst lst]
          [len (length lst)]
          [thus '()])
    (if (< len n)
        (reverse thus)
        (@ (drop lst slide-count)
           (- len slide-count)
           (cons (take lst n) thus)))))

(define (groups-of lst n)
  (sliding-groups-of lst n n))

(module+ test
  (test-case "groups-of"
    (check-equal? (groups-of '(a b c) 1) '((a) (b) (c)))
    (check-equal? (groups-of '(a b c d) 2) '((a b) (c d)))
    (check-equal? (groups-of '(a b c d) 3) '((a b c)))))

(define (sliding-window lst n)
  (sliding-groups-of lst n 1))

(module+ test
  (test-case "sliding-window"
    (check-equal? (sliding-window '(a b c) 1) '((a) (b) (c)))
    (check-equal? (sliding-window '(a b c d) 2) '((a b) (b c) (c d)))
    (check-equal? (sliding-window '(a b c d) 3) '((a b c) (b c d)))))

(define (index-list l)
  (for/list ([(e idx) (in-indexed l)])
    (cons e idx)))

