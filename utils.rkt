#lang racket

(require racket/stxparam
         racket/unsafe/ops)

(provide define/ec
         lambda/ec
         return)

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

(define (groups-of lst n)
  (define (groups-of* lst n)
    (if (empty? lst)
        lst
        (cons (take lst n) (groups-of* (drop lst n) n))))
  (if (zero? (modulo (length lst) n))
      (groups-of* lst n)
      #f))

