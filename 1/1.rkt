#lang racket

(module+ test
  (require rackunit))

(define empty-string? (negate non-empty-string?))

(define (index-list l)
  (for/list ([(e idx) (in-indexed l)])
    (cons e idx)))

;;; Read calories for a single elf. That is, read elements from the list until
;;; the first empty element. Then, convert the read elements to numbers and
;;; sum them up. Returns two values, the summed number, and the resumption
;;; point of the list.
(define/contract (read-elf calorie-list)
  (-> (listof string?) (values number? (listof string?)))
  (define c-list (dropf calorie-list empty-string?))
  (define-values (this-elf tail-list)
    (splitf-at c-list non-empty-string?))
  (values (foldl + 0 (map string->number this-elf))
          (if (empty? tail-list)
              tail-list
              (dropf tail-list empty-string?))))

(module+ test
  (test-case "read-elf"
    (let-values ([(f r) (read-elf '("1" "2" "" "" "2" "" "3"))])
      (check-equal? f 3)
      (check-equal? r '("2" "" "3")))
    (let-values ([(f r) (read-elf '("1"))])
      (check-equal? f 1)
      (check-equal? r '()))))

;;; Read all calories, returning a list of all calories, indexed by number of
;;; elf.
(define/contract (read-all-elfs calorie-list)
  (-> (listof string?) (listof (cons/c number? number?)))
  (define (@ so-far remaining)
    (if (empty? remaining)
        (reverse so-far)
        (let-values ([(next remaining) (read-elf remaining)])
          (@ (cons next so-far) remaining))))
  (define elfs (@ '() calorie-list))
  (sort (index-list elfs) > #:key car))

(module+ test
  (test-case "read-all-elfs"
    (check-equal? '(3 2 3) (read-all-elfs '("1" "2" "" "" "2" "" "3")))
    (check-equal? '() (read-all-elfs '()))
    (check-equal? '(1) (read-all-elfs '("" "" "1" "" "")))))

(module+ main
  (define lines (map string-trim (sequence->list (in-lines))))
  (define elfs (read-all-elfs lines))
  (define (display-top-n elfs n)
    (when (> n (length elfs))
      (error 'not-enough-elfs "Expected ~a elfs got ~a" n (length elfs)))
    (define cals (foldl (lambda (elem acc) (+ acc (car elem)))
                        0
                        (take elfs n)))
    (displayln (format "Top ~a elf(s) have ~a calories"
                       n
                       cals)))
  (display-top-n elfs 1)
  (display-top-n elfs 3))
