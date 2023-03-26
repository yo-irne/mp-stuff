#lang racket

; ZAD2

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

( define t
   ( node
     ( node ( leaf ) 2 ( leaf ) )
     5
     ( node ( node ( leaf ) 6 ( leaf ) )
           8
           ( node ( leaf ) 9 ( leaf ) ) ) ) )


(define h
  (node
   (node
    (node
     (node (leaf) 11 (leaf))
      3
      (leaf))
     8
     (leaf))
   5
   (node
    (leaf)
    9
    (node (leaf) 6 (leaf)))))

(define (fold-tree f x tree)
  (if (leaf? tree)
      x
      (f (node-elem tree) (fold-tree f x (node-l tree)) (fold-tree f x (node-r tree)))))

(define (tree-sum t)
  (fold-tree + 0 t))

(define (tree-flip t)
  (fold-tree (lambda (e l r) (node r e l)) (leaf) t))

(define (tree-height t)
  (fold-tree (lambda (e l r) (+ 1 (max l r))) 0 t))

(define (maxl t)
  (if (leaf? node-l) node-elem (maxl node-l)))

(define (maxr t)
  (if (leaf? node-r) node-elem (maxl node-r)))


(define (tree-span t)
  (cons (fold-tree (lambda (e l r) (if (leaf? l) e l)) (leaf) t) (fold-tree (lambda (e l r) (if (leaf? r) e r)) (leaf) t)))
 

(define (flatten t)
  (fold-tree (lambda (elem l r) (append l (cons elem r))) '() t))

; ZAD 3

;(define-struct leaf () #:transparent)
;(define-struct node (l elem r) #:transparent)

;(define t
;   (node
;     (node (leaf) 2 (leaf) )
;     5
;     (node (node (leaf) 6 (leaf))
;           8
;           (node (leaf) 9 (leaf)))))

(define (bst? t)
  (define (bst-helper t min max)
    (cond [(leaf? t) #t]
           [(and (<= min (node-elem t)) (<= (node-elem t) max))
            (and (bst-helper (node-l t) min (node-elem t))
                 (bst-helper (node-r t) (node-elem t) max))]
           [else #f]))
  (bst-helper t -inf.0 +inf.0))

; ZAD 4

(define (flatten-tree tree)
  (define (flat-append t xs)
    (if (leaf? t)
        xs
        (flat-append (node-l t)
                     (cons (node-elem t)
                           (flat-append (node-r t) xs)))))
  (flat-append tree '()))

; ZAD 5

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

( define t
   ( node
     ( node ( leaf ) 2 ( leaf ) )
     5
     ( node ( node ( leaf ) 6 ( leaf ) )
           8
           ( node ( leaf ) 9 ( leaf ) ) ) ) )

(define (insert-bst-pow x t)
  (cond [(leaf? t) (node (leaf) x (leaf))]
        [(node? t)
         (cond [(< x (node-elem t))
                 (node (insert-bst-pow x (node-l t))
                       (node-elem t)
                       (node-r t))]
                [else
                 (node (node-l t)
                       (node-elem t)
                       (insert-bst-pow x (node-r t)))])]))

(define (fold-tree f x tree)
  (if (leaf? tree)
      x
      (f (node-elem tree) (fold-tree f x (node-l tree)) (fold-tree f x (node-r tree)))))

(define (flatten t)
  (fold-tree (lambda (elem l r) (append l (cons elem r))) '() t))

(define (treesort xs)
  (define (pom xs t)
    (if (empty? xs) (flatten t)
        (pom (cdr xs) (insert-bst-pow (car xs) t))))
  (pom xs (leaf)))

(define l '(5 3 2 7 8))

(treesort '(5 3 2 7 8))

; ZAD 6

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

( define t
   ( node
     ( node ( leaf ) 2 ( leaf ) )
     5
     ( node ( node ( leaf ) 6 ( leaf ) )
           8
           ( node ( leaf ) 9 ( leaf ) ) ) ) )


(define (lefttt t)
  (cond [(leaf? t) t]
        [(leaf? (node-l t)) (node-elem t)]
        [else (lefttt (node-l t))]))


(define (delete-bst x t)
  (cond [(leaf? t) t]
        [(= x (node-elem t))
         (cond [(leaf? (node-l t)) (node-r t)]
               [(leaf? (node-r t)) (node-l t)]
               [else (node (node-l t)
                     (lefttt (node-r t))
                     (delete-bst (lefttt (node-r t)) (node-r t)))])]
        [(< x (node-elem t))
          (node (delete-bst x (node-l t))
                (node-elem t)
                (node-r t))]
        [else
         (node (node-l t)
               (node-elem t)
               (delete-bst x (node-r t)))]))

; ZAD 7

(define empty-queue (cons '() '()))

(define (empty? q)
  (null? (car q)))

(define (push-back x q)
  (if (empty? q)
      (cons (list x) '())
       (cons (car q) (cons x (cdr q)))))

(define (front q)
  (car (car q)))

(define (pop q)
  (cond
    [(empty? q) empty-queue]
    [(null? (cdr(car q))) 
      (cons (reverse (cdr q)) '())]
    [else
      (cons (cdr (car q)) (cdr q))]))
      
; ZAD 8

(define-struct ord (val priority) #:transparent )
(define-struct hleaf () )
(define-struct hnode (elem rank l r) #:transparent )

(define (hord? p h)
  (or
   (hleaf? h)
   (<= p (ord-priority (hnode-elem h)))))

(define (rank h)
  (if (hleaf? h) 0 (hnode-rank h)))

(define (heap? h)
  (or (hleaf? h)
      (and (hnode? h)
           (heap? (hnode-l h))
           (heap? (hnode-r h))
           (<= (rank (hnode-r h))
               (rank (hnode-l h)))
           (= (hnode-rank h) (+ 1 (hnode-rank (hnode-r h))))
           (hord? (ord-priority (hnode-elem h))
                 (hnode-l h))
           (hord? (ord-priority (hnode-elem h))
                 (hnode-r h)))))

(define (make-node elem heap-a heap-b)
  (if (hord? heap-a)
      (if (hord? heap-b)
          (if (< (ord-priority elem) (ord-priority (hnode-elem heap-a)))
              (make-node-1 elem heap-a heap-b)
              (make-node-1 (hnode-elem heap-a) (hnode-l heap-a)
                           (make-node (hnode-elem elem) (hnode-r heap-a) heap-b)))
          heap-a)
      heap-b))

(define (heap-merge h1 h2)
  (cond
    ((hleaf? h1) h2)
    ((hleaf? h2) h1)
    (else
     (let* ((e1 (hnode-elem h1))
            (e2 (hnode-elem h2)))
       (if (< (ord-priority e1) (ord-priority e2))
           (make-node e1 (hnode-l h1) (heap-merge (hnode-r h1) h2))
           (make-node e2 (hnode-l h2) (heap-merge h1 (hnode-r h2))))))))
; ZAD 9

(define empty-pq '())

(define (pq-insert pq elem)
(heap-merge pq (make-node (ord elem 0) empty-pq empty-pq)))

(define (pq-pop pq)
  (cond [(empty? pq) '()]
        [(hleaf? pq) '()]
        [else (let ((e (hnode-elem pq)))
                (heap-merge (hnode-l pq) (hnode-r pq)))]))

(define (pq-min pq)
  (cond [(empty? pq) '()]
        [(hleaf? pq) '()]
        [else (hnode-elem pq)]))

(define (pq-empty? pq) (empty? pq))

(define (pqsort xs)
  (let loop ((pq (foldl (lambda (elem pq) (pq-insert pq elem)) empty-pq xs)) (result '())) (if (pq-empty? pq) result (loop (pq-pop pq) (cons (pq-min pq) result)))))