#lang racket

(require "../utils.rkt")

(module+ test
  (require rackunit))

(define (char-priority c)
  (cond
    [(and (char>=? c #\a) (char<=? c #\z)) (+ 1 (- (char->integer c) (char->integer #\a)))]
    [(and (char>=? c #\A) (char<=? c #\Z)) (+ 27 (- (char->integer c) (char->integer #\A)))]
    [else #f]))

(module+ test
  (test-case "char-priority"
    (check-equal? (char-priority #\p) 16)
    (check-equal? (char-priority #\L) 38)
    (check-equal? (char-priority #\P) 42)
    (check-equal? (char-priority #\v) 22)
    (check-equal? (char-priority #\t) 20)
    (check-equal? (char-priority #\s) 19)
    (check-equal? (char-priority #\space) #f)))

;;; Read a single line of text representing a backpack.
;;;
;;; Returns two values, the hashset of items in the left pocket, and the
;;; hashset of items in the right pocket.
(define (backpack-line-split line)
  (define norm-line (regexp-replace* (regexp "[^a-zA-Z]") line ""))
  (define strlen (string-length norm-line))
  (if (= 1 (modulo strlen 2))
    (values #f #f)
    (let ([left-line (substring norm-line 0 (/ strlen 2))]
          [right-line (substring norm-line (/ strlen 2) strlen)])
      (values (string->list left-line)
              (string->list right-line)))))

(define (backpack-line-set line)
  (define-values (left right) (backpack-line-split line))
  (set-union (list->set left) (list->set right)))

(module+ test
  (test-case "backpack-line-split"
    (let-values ([(left right) (backpack-line-split "a1b2c3c4")])
      (check-equal? left (list #\a #\b))
      (check-equal? right (list #\c #\c)))
    (let-values ([(left right) (backpack-line-split "vJrwpWtwJgWrhcsFMMfFFhFp")])
      (check-equal? left (list #\v #\J #\r #\w #\p #\W #\t #\w #\J #\g #\W #\r))
      (check-equal? right (list #\h #\c #\s #\F #\M #\M #\f #\F #\F #\h #\F #\p))))
  (test-case "backpack-line-set"
    (check-equal? (backpack-line-set "abcc4") (set #\a #\b #\c))))

(define (common-items left right)
  (set-intersect (list->set left) (list->set right)))

(module+ test
  (test-case "common-items"
    (check-equal?
      (common-items (list #\v #\J #\r #\w #\p #\W #\t #\w #\J #\g #\W #\r)
                    (list #\h #\c #\s #\F #\M #\M #\f #\F #\F #\h #\F #\p))
      (set #\p))))

(define (repacking-priority pack-line)
  (let*-values ([(left right) (backpack-line-split pack-line)]
                [(shared-items) (common-items left right)])
    (if (= (set-count shared-items) 1)
      (char-priority (set-first shared-items))
      #f)))

(module+ main
  (define input (sequence->list (in-lines)))
  (displayln "Part one")
  (define total-priority
    (for/sum ([line (in-list input)]
              #:when non-empty-string?)
      (repacking-priority line)))
  (displayln (format "Total priority ~a" total-priority))
  (displayln "Part two")
  (define three-groups
    (for/sum ([lines (in-list (groups-of input 3))])
      (define sets (map backpack-line-set lines))
      (define common (apply set-intersect sets))
      (when (not (= (set-count common) 1))
        (error 'too-many-badges))
      (char-priority (set-first common))))
  (displayln (format "Total priority ~a" three-groups)))
