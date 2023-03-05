#lang racket

(define (fib n)
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

; TODO: fib-iter

(define-struct matrix (a b c d)
  #:auto-value 0
  #:transparent)

(define (matrix-mult m n)
  (make-matrix (+ (* (matrix-b n) (matrix-c m)) (* (matrix-a n) (matrix-a m)))
               (+ (* (matrix-b n) (matrix-d m)) (* (matrix-a n) (matrix-b m)))
               (+ (* (matrix-d n) (matrix-c m)) (* (matrix-c n) (matrix-a m)))
               (+ (* (matrix-d n) (matrix-d m)) (* (matrix-c n) (matrix-b m)))))

(define (matrix-id)
  (make-matrix 1 0 0 1))

(define (matrix-expt m k)
  (if (= k 0) (matrix-id) (matrix-mult m (matrix-expt m (- k 1)))))

(define (fib-matrix k)
  (matrix-b (matrix-expt (make-matrix 1 1 1 0) k)))

(define (matrix-expt-fast m k)
  (cond
    [(= k 0) (matrix-id)]
    [(= (modulo k 2) 0) (matrix-mult (matrix-expt-fast m (/ k 2)) (matrix-expt-fast m (/ k 2)))]
    [else (matrix-mult m (matrix-expt-fast m (- k 1)))]))

(define (fib-matrix-fast k)
  (matrix-b (matrix-expt-fast (make-matrix 1 1 1 0) k)))

(define (elem? x xs)
  (if (null? xs) #f
      (if (equal? x (car xs)) #t (elem? x (cdr xs)))))

(define (max a b)
  (if (< a b) b a))

(define (min a b)
  (if (< a b) a b))

(define (max-mix a b)
  (max (if (list? a) (car a) a) (if (list? b) (car b) b)))

(define (min-mix a b)
  (min (if (list? a) (car a) a) (if (list? b) (car b) b)))

(define (maximum xs)
  (cond
    [(null? xs) -inf.0]
    [(null? (cdr xs)) xs]
    [else (max-mix (car xs) (maximum (cdr xs)))]))

(define (minimum xs)
  (cond
    [(null? xs) +inf.0]
    [(null? (cdr xs)) xs]
    [else (min-mix (car xs) (maximum (cdr xs)))]))

; TODO: fix sorted?
(define (sorted? xs)
  (cond
    [(null? xs) #t]
    [(null? (cdr xs)) #t]
    [(equal? (min-mix (car xs) (car (cdr xs))) (car xs)) (sorted? (cdr xs)) #f]))
