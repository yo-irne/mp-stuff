#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define rocket .)

(define scene-width 300)
(define scene-height 300)
(define rocket-x (/ scene-width 2))
(define rocket-a 0.05)

(define scene (empty-scene scene-width scene-height))

(define rocket-center-to-top
  (- scene-height (/ (image-height rocket) 2)))

(define (scene-with-rocket rocket-y)
  (place-image rocket rocket-x rocket-y scene))

(define count-start 3)
(define (countdown t)
  (sleep 1)
  (place-image (text (number->string (- count-start t)) 30 "red") (/ scene-width 2) (/ scene-height 2) (scene-with-rocket rocket-center-to-top)))

(define (fly t)
  (define distance (- scene-height (/ (* rocket-a (* t t)) 2)))
  (if (< distance rocket-center-to-top)
      (scene-with-rocket distance)
      (scene-with-rocket rocket-center-to-top)))

(define (run t)
  (if (< t (+ count-start 2))
      (countdown t)
      (fly t)))
