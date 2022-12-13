#lang racket

(require threading
         "../utils.rkt")

(module+ test
  (require rackunit))

;;; Each spec line is a series of whitespace separated specifiers. Each
;;; specifier is 3 characters long, and is either "   " or "[x]", where x is
;;; any single character.
;;;
;;; Function returns a list of either #f or the character specified. The
;;; length of the list depends on the length of the line.
(define (read-starting-spec-line spec-line)
  ;; Replace empty specifiers with a '!' character.
  (define bang-line% (regexp-replace "^   " spec-line "[!]"))
  (define bang-line (regexp-replace* "    " bang-line% " [!]"))
  (define char-line (regexp-replace* "[^a-zA-Z!]" bang-line ""))
  (define char-list (string->list char-line))
  (map (λ (c) (if (char=? c #\!) #f c)) char-list))

(module+ test
  (test-case "read-starting-spec-line"
    (check-equal? (read-starting-spec-line "    [a]    ") '(#f #\a #f))
    (check-equal? (read-starting-spec-line "") '())
    (check-equal? (read-starting-spec-line "   ") '(#f))
    (check-equal? (read-starting-spec-line "[a] [b] [c]") '(#\a #\b #\c))))

;;; Pad lst up to length len with elem.
(define (pad-right lst elem len)
  (define missing (- len (length lst)))
  (if (not (positive? missing))
      lst
      (append lst (make-list missing elem))))

(module+ test
  (test-case "pad-right"
    (check-equal? (pad-right '() #t 3) '(#t #t #t))
    (check-equal? (pad-right '(#f #f) #t 3) '(#f #f #t))
    (check-equal? (pad-right '(#f #f #f #f) #t 3) '(#f #f #f #f))))

;;; Read multiple spec lines, and return the list of starting towers. The
;;; first element in each list is the head of the tower - there ought to be no
;;; #f in each tower, unless the spec is wrong and contains buried #f.
(define (read-starting-spec-lines spec-lines)
  (define level-lines (map read-starting-spec-line spec-lines))
  ;; The levels are going to need to all be the same length, so find the max.
  (define max-length (foldl (λ (elem acc) (max acc (length elem))) 0 level-lines))
  (define norm-level-lines (map (λ (l) (pad-right l #f max-length)) level-lines))
  (define tower-mtx (reflect-rectangular-matrix norm-level-lines))
  (map (λ (l) (dropf l false?)) tower-mtx))

(module+ test
  (test-case "read-starting-spec-lines"
    (define case-a (read-starting-spec-lines '("[a]" "[b] [c]")))
    (check-equal? case-a '((#\a #\b) (#\c)))
    (define case-b (read-starting-spec-lines '("[a]" "    [c]")))
    (check-equal? case-b '((#\a #f) (#\c)))
    (define case-c (read-starting-spec-lines '("[a]" "    [c] [d]")))
    (check-equal? case-c '((#\a #f) (#\c) (#\d)))
    (define case-d (read-starting-spec-lines '("[a]     [a]"
                                               "    [c] [d]"
                                               "[e] [e] [e]")))
    (check-equal? case-d '((#\a #f #\e) (#\c #\e) (#\a #\d #\e)))))

(define (read-move-line move-line)
  (define matches (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" move-line))
  (if matches
      (list (string->number (list-ref matches 1))
            (string->number (list-ref matches 2))
            (string->number (list-ref matches 3)))
      #f))

(module+ test
  (test-case "read-move-line"
    (check-equal? (read-move-line "move 1 from 22 to 333") (list 1 22 333))))

(define (apply-moves spec moves [reverse? #t])
  (if (empty? moves)
      spec
      (match-let ([(cons (list move-count from-idx% to-idx%) rest-moves) moves])
        ;; These are 1-indexed, we want 0-indexed
        (define from-idx (sub1 from-idx%))
        (define to-idx (sub1 to-idx%))

        (define items (take (list-ref spec from-idx) move-count))
        (define new-from (drop (list-ref spec from-idx) move-count))
        (define new-to (append (if reverse?
                                   (reverse items)
                                   items)
                               (list-ref spec to-idx)))
        (apply-moves (~> spec
                         (list-set from-idx new-from)
                         (list-set to-idx new-to))
                     rest-moves
                     reverse?))))

;;; Split problem lines. The starting specifier is terminated by a line
;;; consisting only of numbers.
(define (split-problem-lines problem-lines)
  (define spec-lines (takef problem-lines
                            (λ (l) (not (regexp-match-exact? "[0-9 ]*" l)))))
  (define move-lines (drop problem-lines (+ 2 (length spec-lines))))
  (values spec-lines move-lines))

(module+ main
  (define input (sequence->list (in-lines)))
  (define-values (spec-lines move-lines) (split-problem-lines input))
  (define spec (read-starting-spec-lines spec-lines))
  (define moves (map read-move-line move-lines))
  (define (do-part n)
    (define after-moves (apply-moves spec moves (if (= n 1) #t #f)))
    (displayln (format "Final stacks: ~a" after-moves))
    (displayln (format "Top crates are: ~a"
                       (map (lambda (x) (if (empty? x) #f (car x)))
                            after-moves))))
  (displayln "Part One")
  (do-part 1)
  (displayln "Part Two")
  (do-part 2))
