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

(process-tree : (('a 'b 'c 'b -> 'b) ('a -> 'b) ('a 'c -> 'a) ('a 'c -> 'a) 'a (Tree 'c) -> 'b))
(define (process-tree node leaf left right acc tree) 2)

; ZAD 7

(define-type Prop
(var [v : String])
(conj [l : Prop] [r : Prop])
(disj [l : Prop] [r : Prop])
(neg [f : Prop]))

;(define-type (Vars 'a)
;  (lit [v : 'var])
;  (con [l : (Vars 'a)] [r : (Vars 'a)]))

;(in? : (String Listof String -> Boolean))
(define (in? x xs) (or (equal? x (first xs)) (in? x (rest xs))))

;(union : (Listof String Listof String -> ListofString))
(define (union l1 l2) (cond
                        [(in? (first l1) l2) (union (rest l1) l2)]
                        [else (union (rest l1) (append (first l1) l2))]))

(free-var : (Prop -> (Listof String)))
(define (free-var phi) (type-case Prop phi
                         [(var v) (if (equal? v "") '() (list v))]
                         [(conj l r) (union (free-var l) (free-var r))]
                         [(disj l r) (union (free-var l) (free-var r))]
                         [(neg f) (free-var f)]))



(define f1 (conj (var "p") (neg (var "p"))))
(free-var f1)

;(define f2 (disj (conj (var "p")) (conj)))

