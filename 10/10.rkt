#lang racket

(require "../utils.rkt")

(module+ test
  (require rackunit))

;;; CPU has two instructions
;;; - addx V, 2 cycles, adds V to the register.
;;; - noop, 1 cycle, noop
;;;
;;; Register X starts at value 1
;;;
;;; Signal strength is the cycle count times the value in the register.

(define (signal-strength cycle-count register-value)
  (* cycle-count register-value))

;;; Let an instruction be represented by a 3 list: (name cycle-count-left data)
;;;
;;; Let the statemachine have 3 parameters: the current cycle count, the
;;; register value, and a list of instruction. There are 2 operations
;;; available on the statemachine: turn, which advances the cycle count by
;;; one, and reset, which resets and installs a new instruction list.

(define (sm-new instruction-list)
  (list 0 1 instruction-list))

(define/ec (sm-turn sm)
  (match-define (list cycle-count register-value instruction-list) sm)

  ;; If there are no instructions, do nothing.
  (when (empty? instruction-list)
    (return (list (add1 cycle-count) register-value '())))

  ;; Get head instruction
  (define-values (inst insts) (values (car instruction-list)
                                      (cdr instruction-list)))
  (match-define (list name inst-cycles-left data) inst)

  ;; If there's more than one cycle left, turn and do nothing
  (when (> inst-cycles-left 1)
    (define new-inst (list name (sub1 inst-cycles-left) data))
    (return (list (add1 cycle-count) register-value (cons new-inst insts))))

  ;; One cycle left, carry out this instruction
  (case name
    [(noop)
     ;; Do nothing
     (list (add1 cycle-count) register-value insts)]
    [(addx)
     ;; Add the value
     (list (add1 cycle-count) (+ register-value data) insts)]))

(define (line-to-instruction line)
  (define inst-name (substring line 0 4))
  (case inst-name
    [("addx") (list 'addx 2 (string->number (substring line 5)))]
    [("noop") (list 'noop 1 #f)]
    [else (error 'unknown-instruction "Unknown instruction ~a" inst-name)]))

(define (subvector v start [end #f])
  (define vtail (vector-drop v start))
  (if end
      (vector-take vtail (- end start))
      vtail))

(define (draw-screen s)
  (define r1 (subvector s 0 40))
  (define r2 (subvector s 40 80))
  (define r3 (subvector s 80 120))
  (define r4 (subvector s 120 160))
  (define r5 (subvector s 160 200))
  (define r6 (subvector s 200 240))
  (define (v->chars v)
    (list->string (vector->list (vector-map (λ (e) (if e #\# #\.)) v))))
  (define lines (list r1 r2 r3 r4 r5 r6))
  (for ([line (in-list lines)])
    (displayln (v->chars line))))

(module+ main
  (define input (sequence->list (in-lines)))
  (define insts (map line-to-instrucion input))
  (define sm-0 (sm-new insts))
  (define print-set (set 20 60 100 140 180 220))
  (define sum-of-strengths 0)
  (void (foldl (λ (turn-count sm)
                 (define new-sm (sm-turn sm))
                 (when (set-member? print-set turn-count)
                   ;; We care about the inst count of the new SM,
                   ;; but the register value of the old one. For
                   ;; reasons.
                   (define strength (* (car new-sm) (cadr sm)))
                   (add! sum-of-strengths strength)
                   (displayln (format "Turn ~a, signal strength ~a"
                                      turn-count
                                      strength)))
                 new-sm)
               sm-0
               (range 1 221)))
  (displayln (format "Part One: ~a" sum-of-strengths))
  ;; Part two
  (displayln "Part Two:")
  (define screen (make-vector 240 #f))
  (void (foldl (λ (turn-count sm)
                 (define new-sm (sm-turn sm))
                 (define ppos (cadr sm))
                 (define 40-turn-count (modulo turn-count 40))
                 (when (or (= 40-turn-count ppos)
                           (= (abs (- 40-turn-count ppos)) 1))
                   ;; Draw this pixel
                   (vector-set! screen turn-count #t))
                 new-sm)
               sm-0
               (range 0 240)))
  (draw-screen screen))
