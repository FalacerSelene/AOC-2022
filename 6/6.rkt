#lang racket

(require "../utils.rkt")

(module+ test
  (require rackunit))

(define (all-unique? lst)
  (= (set-count (list->set lst)) (length lst)))

(module+ test
  (test-case "all-unique?"
    (check-true (all-unique? '(a b c d)))
    (check-false (all-unique? '(a b c d a)))))

;;; Note to convert to the number the puzzle wants you need to add scan-size
;;; to the returned index, because we use a different convention.
(define (find-first-loc-all-unique str scan-size)
  (define chars (string->list str))
  (define windows (sliding-window chars scan-size))
  (define indexed-windows (index-list windows))
  (findf (λ (w) (all-unique? (car w))) indexed-windows))

(module+ test
  (test-case "find-first-loc-all-unique"
    ;; Per above comment these are all "off" by 4 from the spec examples.
    (check-equal? (cdr (find-first-loc-all-unique "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4)) 3)
    (check-equal? (cdr (find-first-loc-all-unique "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)) 1)
    (check-equal? (cdr (find-first-loc-all-unique "nppdvjthqldpwncqszvftbrmjlhg" 4)) 2)
    (check-equal? (cdr (find-first-loc-all-unique "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)) 6)
    (check-equal? (cdr (find-first-loc-all-unique "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4)) 7)))

(module+ main
  (define line (read-line))
  (define sop-code (find-first-loc-all-unique line 4))
  (if sop-code
      (displayln (format "SOP code at ~a, code ~a"
                         (+ 4 (cdr sop-code))
                         (car sop-code)))
      (displayln "No SOP code found"))
  (define som-code (find-first-loc-all-unique line 14))
  (if som-code
      (displayln (format "SOM code at ~a, code ~a"
                         (+ 14 (cdr som-code))
                         (car som-code)))
      (displayln "No SOM code found")))


