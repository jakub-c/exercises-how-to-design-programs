;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |455|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 455 ====================

(define EPSILON 0.1)

; [Number -> Number] Number -> Number
; maps function f and a number r1 to the slope of f at r1
; (define (slope f r1) 0) ;stub

(check-expect (slope (lambda (x) 2) 10) 0)
(check-expect (slope (lambda (x) x) 2) 1)
(check-expect (slope (lambda (x) (* x 3)) 2) 3)

(define (slope f r1)
  (* (/ 1 (* 2 EPSILON))
     ( - (f (+ r1 EPSILON))
         (f (- r1 EPSILON)))))

; =================== End of exercise ==================

; ==================== Exercise 456 ====================

; [Number->Number] Number -> Number
(check-error (root-of-tangent (lambda (x) 2) 10))
(check-expect (root-of-tangent (lambda (x) x) 2) 0)
(check-expect (root-of-tangent (lambda (x) (* x 3)) 2) 0)
(check-expect (root-of-tangent (lambda (x) (+ x 1)) 6) -1)


; (define (root-of-tangent f r1) 0) ;stub

(define (root-of-tangent f r1)
  (- r1
     (/ (f r1)
        (slope f r1))))

; =================== End of exercise ==================
