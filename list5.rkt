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

(define-type (Zipperof 'a)
  (Zipper [pref : (Listof 'a)]
          [suf : (Listof 'a)]))

(define essa (Zipper '(1 2) '(2 1)))

(List->Zipper : ((Listof 'a) -> (Zipperof 'a))) 
(define (List->Zipper xs) (Zipper '() xs))

(Zipper->List : ((Zipperof 'a) -> (Listof 'a)))
(define (Zipper->List z) (foldl cons (Zipper-suf z) (Zipper-pref z)))

(move-right : ((Zipperof 'a) -> (Zipperof 'a)))
(define (move-right z)
  (Zipper (cons (first (Zipper-suf z)) (Zipper-pref z)) (rest (Zipper-suf z))))

(move-left : ((Zipperof 'a) -> (Zipperof 'a)))
(define (move-left z)
  (Zipper (rest (Zipper-pref z)) (cons (first (Zipper-pref z)) (Zipper-suf z))))

(insert : ((Zipperof 'a) 'a -> (Zipperof 'a)))
(define (insert z elem)
  (Zipper (Zipper-pref z) (cons elem (Zipper-suf z))))

(define (end? z) (empty? (Zipper-suf z)))
(define (beg? z) (empty? (Zipper-pref z)))




(place : ((Listof 'a) 'a -> (Listof (Listof 'a))))
(define (place xs elem)
  (letrec [(inner (λ(z l elem)
          (cond [(end? z) (cons (Zipper->List (insert z elem)) l)]
                [else (cons (Zipper->List (insert z elem)) (inner (move-right z) l elem))])))]
  (inner (List->Zipper xs) '() elem)))


(concat : ((Listof 'a) (Listof 'a) -> (Listof 'a)))
(define (concat xs ys)
  (foldl cons xs ys))


(define (rank-down xs)
  (foldr concat '() xs))


(perm : ((Listof 'a) -> (Listof (Listof 'a))))
(define (perm xs)
  (if (empty? (rest xs)) (list xs)
      (let [(head (first xs)) (tail (rest xs))]
      (rank-down (map (λ(x)(place x head)) (perm tail))))))

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

; TODO BST DLA 5 ZADANIA

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

; ZAD 8

(define (unwrap-or op def)
  (type-case (Optionof 'a) op
    [(some v) v]
    [(none) def]))

(eval : ((Hashof String Boolean) Prop -> Boolean))
(define (eval val psi)
  (type-case Prop psi
    [(var v) (unwrap-or (hash-ref val v) #f)]
    [(conj l r) (and (eval val l) (eval val r))]
    [(disj l r) (or (eval val l) (eval val r))]
    [(neg f) (not (eval val f))]))
    
; ZAD 9

(gen-vals : ((Listof String) -> (Listof (Hashof String Boolean)))) 
(define (gen-vals xs)
  (foldl (λ(a b)
           (append (map (λ(x)(hash-set x a #f)) b)
           (map (λ(x)(hash-set x a #t)) b)))
         (list(hash '())) xs))

(define (tautology? psi)
  (foldl (λ(a b)(and a b)) #t (map (λ(x)(eval x psi)) (gen-vals (free-vars psi)))))
