#lang racket

(require "../tree.rkt")

(module+ test
  (require rackunit)

  (define spec-lines (list "$ cd /"
                           "$ ls"
                           "dir a"
                           "14848514 b.txt"
                           "8504156 c.dat"
                           "dir d"
                           "$ cd a"
                           "$ ls"
                           "dir e"
                           "29116 f"
                           "2557 g"
                           "62596 h.lst"
                           "$ cd e"
                           "$ ls"
                           "584 i"
                           "$ cd .."
                           "$ cd .."
                           "$ cd d"
                           "$ ls"
                           "4060174 j"
                           "8033020 d.log"
                           "5626152 d.ext"
                           "7214296 k")))

(define (read-spec-line line)
  (cond
    [(equal? line "$ ls") '(ls)]
    [(equal? line "$ cd /") '(cd-root)]
    [(equal? line "$ cd ..") '(cd-up)]
    [(regexp-match-exact? #rx"\\$ cd (.*)" line)
     (let ([matches (regexp-match #rx"\\$ cd (.*)" line)])
       `(cd-dir ,(cadr matches)))]
    [(regexp-match-exact? #rx"dir (.*)" line)
     (let ([matches (regexp-match #rx"dir (.*)" line)])
       `(entry-dir ,(cadr matches)))]
    [(regexp-match-exact? #rx"([0-9]+) (.*)" line)
     (let ([matches (regexp-match #rx"([0-9]+) (.*)" line)])
       `(entry-file ,(caddr matches) ,(string->number (cadr matches))))]
    [else (error 'unknown-command "Unknown command ~a" line)]))

(module+ test
  (test-case "read-spec-line"
    (check-equal? (read-spec-line "$ cd foo") '(cd-dir "foo"))
    (check-equal? (read-spec-line "$ ls") '(ls))
    (check-equal? (read-spec-line "dir 17") '(entry-dir "17"))
    (check-equal? (read-spec-line "16 19") '(entry-file "19" 16))))

(define (lines-to-tree spec-lines)
  (let @ ([spec-lines spec-lines]
          [tree tree-base]
          [cwd '()])
    (if (empty? spec-lines)
        tree
        (let* ([first-line (car spec-lines)]
               [rest-lines (cdr spec-lines)]
               [cmd (car first-line)]
               [args (cdr first-line)])
          (case cmd
            [(ls)
             ;; Do nothing
             (@ rest-lines tree cwd)]
            [(cd-root)
             ;; Move to root
             (@ rest-lines tree '())]
            [(cd-up)
             ;; Up one dir
             (@ rest-lines tree (cdr cwd))]
            [(cd-dir)
             ;; Push into new dir
             (let ([new-tree (tree-add tree
                                       (car args)
                                       'dir
                                       '()
                                       (reverse cwd))])
               (@ rest-lines new-tree (cons (car args) cwd)))]
            [(entry-dir)
             ;; List a new dir
             (let ([new-tree (tree-add tree
                                       (car args)
                                       'dir
                                       '()
                                       (reverse cwd))])
               (@ rest-lines new-tree cwd))]
            [(entry-file)
             ;; List a new file
             (let ([new-tree (tree-add tree
                                       (car args)
                                       'file
                                       (cadr args)
                                       (reverse cwd))])
               (@ rest-lines new-tree cwd))]
            [else (error 'unmatch "Unmatched ~a" cmd)])))))


(define (tree-size tree)
  (match-define (list-rest name type data) tree)
  (if (equal? type 'file)
      data
      (for/sum ([subelem (in-list data)])
        (tree-size subelem))))

(module+ test
  (test-case "tree-size"
    (define parsed (map read-spec-line spec-lines))
    (define tree (lines-to-tree parsed))
    (check-equal? (tree-size tree) 48381165)))

(module+ test
  (test-case "example"
    (define parsed (map read-spec-line spec-lines))
    (define tree (lines-to-tree parsed))
    (define dirs-below-hundok
      (tree-find tree (λ (e) (and (equal? (cadr e) 'dir)
                                  (< (tree-size e) 100000)))))
    (define total-size
      (for/sum ([d (in-list dirs-below-hundok)])
        (tree-size d)))
    (check-equal? total-size 95437)))

(module+ main
  (define lines (sequence->list (in-lines)))
  (define spec (map read-spec-line lines))
  (define tree (lines-to-tree spec))
  (define dirs-below-hundok
    (tree-find tree (λ (e) (and (equal? (cadr e) 'dir)
                                (< (tree-size e) 100000)))))
  (define total-size
    (for/sum ([d (in-list dirs-below-hundok)])
      (tree-size d)))
  (displayln (format "Part one: ~a" total-size))

  (define total-size-used (tree-size tree))
  (define disk-size 70000000)
  (define update-size 30000000)
  (define to-delete-at-least (- total-size-used (- disk-size update-size)))
  ;; Find the smallest dir bigger than to-delete-at-least
  (define dirs-big-enough
    (tree-find tree (λ (e) (and (equal? (cadr e) 'dir)
                                (>= (tree-size e) to-delete-at-least)))))
  (define smallest-big-enough (argmin tree-size dirs-big-enough))
  (displayln (format "Part two: ~a" (tree-size smallest-big-enough))))
