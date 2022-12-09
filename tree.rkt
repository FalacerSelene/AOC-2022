#lang racket

(module+ test
  (require rackunit))

(provide tree-add
         tree-base
         tree-get
         tree-find)

;;; TREE = DIR | FILE
;;; DIR = (list name 'dir . TREES)
;;; FILE = (list name 'file . size)
(define (tree-add tree entry-name entry-type data parent-path)
  ;; Dispatch
  (if (empty? parent-path)
      (tree-add-base tree entry-name entry-type data)
      (tree-add-nest tree entry-name entry-type data parent-path)))

;;; Base case - CWD empty.
(define (tree-add-base tree entry-name entry-type entry-data)
  (match-define (list-rest name type data) tree)
  (when (equal? type 'file)
    (error 'bad-tree "Can't add file to file ~a" name))
  (define this-entry (findf (λ (e) (equal? (car e) entry-name)) data))
  (cond
    ;; If the tree does not contain the entry, append it.
    [(not this-entry)
     (list* name type (cons (list* entry-name entry-type entry-data) data))]
    ;; If the tree contains the entry and it's of the right type, return the
    ;; tree as is.
    [(equal? (cadr this-entry) entry-type)
     tree]
    ;; If the tree contains the entry and it's of the wrong type, error.
    [else
      (error 'duplicate-file "Duplicate file/dir in ~a, ~a" name entry-name)]))

;;; Nest case - push into dir
(define (tree-add-nest tree e-name e-type e-data parent-path)
  (match-define (list-rest name type data) tree)
  (define next-parent (car parent-path))
  (define rest-parent-path (cdr parent-path))

  (define this-idx (index-where data (λ (e) (equal? (car e) next-parent))))
  (define this-entry (list-ref data this-idx))

  (define new-data
    (list-set data this-idx
              (tree-add this-entry e-name e-type e-data rest-parent-path)))
  (list* name type new-data))

(define tree-base (list "" 'dir))

(define (tree-get tree entry-path)
  (if (empty? entry-path)
      tree
      (match-let ([(cons first-path rest-path) entry-path]
                  [(list-rest name type data) tree])
        (define this-entry (findf (λ (e) (equal? (car e) first-path)) data))
        (if this-entry
            (tree-get this-entry rest-path)
            #f))))

(module+ test
  (test-case "tree-add"
    (define with-dir (tree-add tree-base "etc" 'dir '() '()))
    (define with-nested-dir (tree-add with-dir "opt" 'dir '() '("etc")))
    (define with-file (tree-add with-nested-dir "foo" 'file 27 '("etc" "opt")))
    (check-equal? with-file `("" dir ("etc" dir ("opt" dir ("foo" file . 27)))))
    (define dir-two (tree-add with-file "var" 'dir '() '()))
    (check-equal? dir-two `("" dir ("var" dir)
                            ("etc" dir ("opt" dir ("foo" file . 27)))))
    (define foo-elem (tree-get dir-two '("etc" "opt" "foo")))
    (check-equal? foo-elem '("foo" file . 27))))

;;; Return all subtrees where proc returns true
(define (tree-find tree proc #:prune? [prune? #f])
  (match-define (list-rest name type data) tree)
  (define this-elem? (proc tree))
  (if (equal? type 'file)
      (if this-elem? (list tree) '())
      (if (and prune? (not this-elem?))
          '()
          (let* ([children (map (λ (t) (tree-find t proc #:prune? prune?)) data)]
                 [flat-children (append* children)])
            (if this-elem? (cons tree flat-children) flat-children)))))

