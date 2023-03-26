#lang plait

; ZAD 1

(define (pp1 x y) x)

(define (pp2 f g x) (f x (g x)))

(define (id x) x)
(define (f h j) (j (h j)))
(define (pp3 h) (f h id))

(define (pp4 f g) (λ (x) (pair (f x) (g x))))

(define (pp5 f a)
  (type-case (Optionof 'a) (f a)
    [(none) '()]
    [(some n)
     (if (some? (f (fst n)))
         (list (snd n))
         '())]))

; ZAD 2

(apply : (('a -> 'b) 'a -> 'b))
(define (apply f x) (f x))

(compose : (('a -> 'b) ('c -> 'a) -> ('c -> 'b)))
(define (compose f g) (λ (x) (f (g x))))

(flip : (('a 'b -> 'c) -> ('b 'a -> 'c)))
(define (flip f) (λ (x y) (f y x)))

(curry : (('a 'b -> 'c) -> ('a -> ('b -> 'c))))
(define (curry f) (λ (x) (λ (y) (f x y))))

; ZAD 3

;(('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
(curry compose)

;(('a -> ('b -> 'c)) -> ('a -> (('d -> 'b) -> ('d -> 'c))))
((curry compose) (curry compose))

;(('a -> ('b -> 'c)) -> ('a -> ('b -> 'c)))
((curry compose) (curry apply))

;(('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
((curry apply) (curry compose))

;(('a 'b -> 'c) -> ('b -> ('a -> 'c)))
(compose curry flip)

; ZAD 4

; ZAD 5

(define-type (Tree 'a)
  (leaf)
  (node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)]))

(define mytree
  (node
   (node (leaf) 2 (leaf))
   1
   (node (leaf) 3 (leaf))))

(process-tree : (('a 'b 'c 'b -> 'b) ('a -> 'b) ('a 'c -> 'a) ('a 'c -> 'a) 'a (Tree 'c) -> 'b))
(define (process-tree nd lf left right a t)
  (cond [(leaf? t) (lf a)]
        [(node? t) (nd a
                       (process-tree nd lf left right (left a (node-elem t)) (node-l t))
                       (node-elem t)
                       (process-tree nd lf left right (right a (node-elem t)) (node-r t)))]))

(define (tree-max t)
  (process-tree (lambda (x l c r) (max c (max l r))) (lambda (x) 0) (lambda (x y) x) (lambda (x y) x) 0 t))

(define (tree-min t)
  (process-tree (lambda (x l c r) (min c (min l r))) (lambda (x) +inf.f) (lambda (x y) x) (lambda (x y) x) 0 t))

(define (sum-paths t)
  (process-tree (lambda (x l c r) (node l (+ x c) r))
                (lambda (x) (leaf))
                (lambda (x y) (+ x y))
                (lambda (x y) (+ x y))
                0
                t))

; ZAD 7

(define-type Prop
(var [v : String])
(conj [l : Prop] [r : Prop])
(disj [l : Prop] [r : Prop])
(neg [f : Prop]))

(define (in? x xs) (if (empty? xs) #f (or (equal? x (first xs)) (in? x (rest xs)))))
(define (union l1 l2)
  (foldl (lambda (x ys) (if (in? x ys) ys (cons x ys))) l1 l2))

(define f1 (conj (var "p") (neg (var "p"))))

(define (free-var phi) (type-case Prop phi
                         [(var v) (if (equal? v "") '() (list v))]
                         [(conj l r) (union (free-var l) (free-var r))]
                         [(disj l r) (union (free-var l) (free-var r))]
                         [(neg f) (free-var f)]))

(define f1 (conj (var "p") (neg (var "p"))))
(free-var f1)

;(define f2 (disj (conj (var "p")) (conj)))

