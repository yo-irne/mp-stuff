#lang racket
; ZAD 2
(define (product xs)
  (foldl * 1 xs)) ; dla xs = '() zwraca 1 bo wsm czemu nie podobne do silni wtedy

; ZAD 4
(define (my-compose f g)
  (λ(x) (f (g x))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

; ZAD 5
(define (negatives n)
  (build-list n (λ (x) (- -1 x))))

(define (reciprocals n)
  (build-list n (λ (x) (/ 1 (+ x 1)))))

(define (evens n)
  (build-list n (λ (x) (* x 2))))

(define (rec n x)
  (if (= n 0) (list)
      (if (= n (+ x 1)) (cons 1 (rec (- n 1) x))
          (cons 0 (rec (- n 1) x) ) ) ) )

(define (identityM n)
  (build-list n (λ (x) (rec n (- (- n x) 1)))))

; ZAD 6
(define (empty-set a)
  #f)

(define (singleton a)
  (λ (x) (equal? x a)))

(define (in a s)
  (s a))

(define (union s t)
  (λ (x) (or (s x) (t x))))

(define (intersect s t)
  (λ (x) (and (s x) (t x))))

; ZAD 7 todo
(define (foldr-reverse xs)
  (foldr (λ (y ys) (append ys (list y))) null xs))

(length (foldr-reverse (build-list 10000 identity)))

; ZAD 8 todo
(define (list->llist x)
  (λ(y)(foldr cons y x)))

(define (llist->list f)
  (f '()))

(define llist-null
  (λ(y) y))

(define (llist-singleton x) (list->llist (list x)))

(define (list-append f g)
  (λ(x)(f(g x))))
