;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |527|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/math)

; ==================== Exercise 527 ====================

(define (segment img x y r t)
  (scene+line 
   img
   x y
   (+ x (* r (inexact->exact (cos t))))
   (+ y (* r (inexact->exact (sin t))))         
   "black"))

(define (add-savannah img x y length angle)
  (cond
    [(< length 10) img]
    [else
     (local ((define x0 x)
             (define y0 y)
             (define x1 (+ x (* length (inexact->exact (cos angle)))))
             (define y1 (+ y (* length (inexact->exact (sin angle)))))
             (define x0+x1 (+ x0 x1))
             (define y0+y1 (+ y0 y1))
             (define newX (* 0.5 x0+x1))
             (define newY (* 0.5 y0+y1))
             (define root-image
               (segment img x y length angle))
             (define left-branch
               (add-savannah root-image newX newY (* length 0.7) (- angle 0.15))))
       (add-savannah left-branch newX newY (* length 0.8) (+ angle 0.2)))]))

(add-savannah (empty-scene 400 400) 150 400 170 4.7)

; =================== End of exercise ==================
