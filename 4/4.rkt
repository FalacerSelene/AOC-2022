#lang racket

(module+ test
  (require rackunit))

(define all andmap)

;;; Read assignments from a line
;;;
;;; Returns two values, left line and right line
(define (parse-assignments line)
  (let/ec return
    ;; '("1-2" "3-5" "4-5" ...)
    (define spreads (string-split line ","))

    ;;  '("1-2" "3-5")
    (unless (= (length spreads) 2)
      (return (values #f #f)))

    ;; '((1 2) (3 5))
    (define ranges (map (λ (x) (string-split x "-")) spreads))

    ;; Validate all length 2.
    (unless (all (λ (x) (= (length x) 2))
                 ranges)
      (return (values #f #f)))

    ;; '((set 1 2) (set 3 4 5))
    (define sets
      (map (λ (rng) (let ([first (string->number (car rng))]
                          [last (string->number (cadr rng))])
                      (for/set ([s (in-inclusive-range first last)])
                        s)))
           ranges))

    (values (car sets)
            (cadr sets))))

(module+ test
  (test-case "parse-assignments"
    (let-values ([(left right) (parse-assignments "2-4,6-8")])
      (check-equal? left (set 2 3 4))
      (check-equal? right (set 6 7 8)))))

(define (either-subset? one two)
  (or (subset? one two)
      (subset? two one)))

(module+ main
  (define input (sequence->list (in-lines)))
  ;; Part one
  (for/sum ([l (in-list input)])
    (define-values (left right) (parse-assignments l))
    (unless (and left right)
      (error 'parse-error "Failed to parse line: ~a" l))
    (if (either-subset? left right)
        1
        0))
  ;; Part two
  (for/sum ([l (in-list input)])
    (define-values (left right) (parse-assignments l))
    (unless (and left right)
      (error 'parse-error "Failed to parse line: ~a" l))
    (if (set-empty? (set-intersect left right))
        0
        1)))
