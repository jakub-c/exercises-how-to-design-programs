;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 525-526) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/math)

; ==================== Exercise 525 ====================

(define TRESHOLD 10)

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
; (define (add-triangle scene a b c) scene) ;stub

(check-expect (add-triangle (empty-scene 100 100)
                            (make-posn 50 0) (make-posn 100 100) (make-posn 0 100))
              (scene+line
               (scene+line
                (scene+line (empty-scene 100 100)
                            50 0 100 100 "black")
                100 100 0 100 "black")
               0 100 50 0 "black"))

(define (add-triangle scene a b c)
  (local ((define ax (posn-x a))
          (define ay (posn-y a))
          (define bx (posn-x b))
          (define by (posn-y b))
          (define cx (posn-x c))
          (define cy (posn-y c)))
    (scene+line
     (scene+line
      (scene+line scene
                  ax ay bx by "black")
      bx by cx cy "black")
     cx cy ax ay "black")))
 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
; (define (too-small? a b c) #false)

(check-expect (too-small? (make-posn 10 10)
                          (make-posn 4 4)
                          (make-posn 2 2))
              #t)

(define (too-small? a b c)
  (or (< (exact-round (sqrt (+ (expt
                                (- (posn-x a) (posn-x b))
                                2)
                               (expt
                                (- (posn-y a) (posn-y b))
                                2))))
         TRESHOLD)
      (< (exact-round (sqrt (+ (expt
                                (- (posn-x b) (posn-x c))
                                2)
                               (expt
                                (- (posn-y b) (posn-y c))
                                2))))
         TRESHOLD)
      (< (exact-round (sqrt (+ (expt
                                (- (posn-x c) (posn-x a))
                                2)
                               (expt
                                (- (posn-y c) (posn-y a))
                                2))))
         TRESHOLD)))
 
; Posn Posn -> Posn 
; determines the midpoint between a and b
(check-expect
 (mid-point  (make-posn -1 2) (make-posn 3 -6))
 (make-posn 1 (* 2 -1)))

(define (mid-point a b)
  (local ((define x0+x1 (+ (posn-x a) (posn-x b)))
          (define y0+y1 (+ (posn-y a) (posn-y b))))
    (make-posn
     (* 0.5 x0+x1)
     (* 0.5 y0+y1))))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to s, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

(add-sierpinski MT A B C)

; =================== End of exercise ==================

; ==================== Exercise 526 ====================

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER 
; and RADIUS whose angle is 
 
; examples
; what are the x and y coordinates of the desired 
; point, when given: 120/360, 240/360, 360/360
 
(define (circle-pt factor)
  (make-posn 0 0))

; =================== End of exercise ==================