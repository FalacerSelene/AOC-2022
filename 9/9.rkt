#lang racket

(require threading
         "../utils.rkt")

(module+ test
  (require rackunit))

(define (head-move direction head-pos tail-pos)
  (match-define (cons head-x head-y) head-pos)
  (match-define (cons tail-x tail-y) tail-pos)
  (define-values (new-head-x new-head-y)
    (case direction
      [(up) (values head-x (add1 head-y))]
      [(down) (values head-x (sub1 head-y))]
      [(left) (values (sub1 head-x) head-y)]
      [(right) (values (add1 head-x) head-y)]))

  (define-values (x-delta y-delta)
    (values (- new-head-x tail-x)
            (- new-head-y tail-y)))

  (when (or (>= (abs x-delta) 2)
            (>= (abs y-delta) 2))

    (when (positive? x-delta)
      (inc! tail-x))

    (when (negative? x-delta)
      (dec! tail-x))

    (when (positive? y-delta)
      (inc! tail-y))

    (when (negative? y-delta)
      (dec! tail-y)))

  (values (cons new-head-x new-head-y)
          (cons tail-x tail-y)))

(module+ test
  (test-case "head-move"
    (let-values ([(head tail)
                  (head-move 'up (cons 0 0) (cons 0 0))])
      (check-equal? head '(0 . 1))
      (check-equal? tail '(0 . 0)))

    (let-values ([(head tail)
                  (head-move 'up (cons 2 2) (cons 1 1))])
      (check-equal? head '(2 . 3))
      (check-equal? tail '(2 . 2)))))

(define (head-move-origin-set move-list)
  (let @ ([moves move-list]
          [head-pos (cons 0 0)]
          [tail-pos (cons 0 0)]
          [tail-set (set)])
    (if (empty? moves)
        tail-set
        (let-values ([(new-head new-tail)
                      (head-move (car moves) head-pos tail-pos)])
          (displayln (format "~a :: ~a" new-head new-tail))
          (@ (cdr moves) new-head new-tail (set-add tail-set tail-pos))))))

(define (line-to-move line)
  (define direction
    (case (string-ref line 0)
      [(#\L) 'left]
      [(#\R) 'right]
      [(#\U) 'up]
      [(#\D) 'down]))
  (define cnt (string->number (substring line 2)))
  (make-list cnt direction))

(define (move-list lines)
  (let @ ([lines lines]
          [move-list '()])
    (if (empty? lines)
        (reverse move-list)
        (let ([next-move (line-to-move (car lines))])
          (@ (cdr lines) (append next-move move-list))))))

(define (lines-to-tail-pos-count lines)
  (~> lines move-list head-move-origin-set set-count))

(module+ main
  (define input (sequence->list (in-lines)))
  (define pos-count (lines-to-tail-pos-count input))
  (displayln (format "Part One: ~a" pos-count)))
