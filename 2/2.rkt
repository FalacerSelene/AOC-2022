#lang racket

(require threading)

(module+ test
  (require rackunit))

(define rock 0)
(define paper 1)
(define scissors 2)

(define (game-result player-symbol opponent-symbol)
  (define result (modulo (- player-symbol opponent-symbol) 3))
  (define victor-points
    (case result
     ;; Draw
     [(0) 3]
     ;; Player win
     [(1) 6]
     ;; Opponent win
     [(2) 0]))
  ;; Overall score = victory-score + symbol-score + 1
  (+ victor-points player-symbol 1))

(module+ test
  (test-case "game-results"
    (check-equal? 8 (game-result paper rock))
    (check-equal? 1 (game-result rock paper))
    (check-equal? 6 (game-result scissors scissors))))

(define (play-guide-round guide-line)
  (let ([guide-length (string-length guide-line)])
    (when (> 3 guide-length)
      (error 'too-short "Guide line ~a chars, needed 3" guide-length)))
  (define (char-2-int c)
    (case (char-downcase c)
      [(#\a #\x) rock]
      [(#\b #\y) paper]
      [(#\c #\z) scissors]))
  (define opponent (char-2-int (string-ref guide-line 0)))
  ;; Part 1
  ;;(define player (char-2-int (string-ref guide-line 2)))
  ;; Part 2
  (define player (~> (string-ref guide-line 2)
                     char-2-int
                     sub1
                     (+ opponent)
                     (modulo 3)))
  (game-result player opponent))

(module+ test
  ;;(test-case "play-guide-round-part-one"
  ;;  (check-equal? 8 (play-guide-round "A Y"))
  ;;  (check-equal? 1 (play-guide-round "B X"))
  ;;  (check-equal? 6 (play-guide-round "C Z")))
  (test-case "play-guide-round-part-two"
    (check-equal? 4 (play-guide-round "A Y"))
    (check-equal? 1 (play-guide-round "B X"))
    (check-equal? 7 (play-guide-round "C Z"))))

(module+ main
  (define guide-lines (~>> (in-lines)
                           sequence->list
                           (map string-trim)
                           (filter non-empty-string?)))
  (define expected-score (foldl + 0 (map play-guide-round guide-lines)))
  ;; Print number of rounds as sanity check
  (displayln (format "Playing ~a rounds, expected score ~a"
                     (length guide-lines)
                     expected-score)))
