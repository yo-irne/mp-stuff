#lang racket

(define lista (mcons 1 (mcons 2 (mcons 3 null))))

; ZAD 1

(define (cycle! xs)
    (define (cyc front xs)
        (if (null? (mcdr xs))
            (set-mcdr! xs front)
            (cyc front (mcdr xs))))
    (cyc xs xs))

; ZAD 2

(define (mreverse! xs)
    (define (mrev left right)
        (if (not (null? right))
            (let [(next (mcdr right))]
                (set-mcdr! right left)
                (mrev right next))
            left))
    (mrevc null xs))

