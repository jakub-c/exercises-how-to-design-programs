;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |412|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).


; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(check-expect (inex->number (create-inex 50 -1 20))
              (inex->number (create-inex 5 -1 19)))


; ==================== Exercise 412 ====================

; note: I think there's more functional and cleaner
;       solution to this problem. I may revisit this.

; Inex Inex -> Inex
; add two Inex representations of numbers that have the same exponent
; or the exponent differs by one
; (define (inex+ a b) (create-inex 1 1 1)) ;stub

(check-expect (inex+ (create-inex 1 1 0)
                     (create-inex 1 1 0))
              (create-inex 2 1 0))

(check-expect (inex+ (create-inex 99 1 0)
                     (create-inex 11 1 0))
              (create-inex 11 1 1))

(check-error  (inex+ (create-inex 99 1 99) (create-inex 99 1 99))
                "exponent out of range")

(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
                (create-inex 11 -1 1))

(check-expect 
 (inex+ (create-inex 10 1 1) (create-inex 10 1 1))
 (create-inex 20 1 1)
 )
(check-expect 
 (inex+ (create-inex 80 1 1) (create-inex 30 1 1))
 (create-inex 11 1 2)
 )
(check-expect
   (inex+ (create-inex 1 1 0) (create-inex 1 1 1))
   (create-inex 11 1 0)
   )
(check-expect
   (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
   (create-inex 11 -1 1)
   )
(check-expect
   (inex+ (create-inex 9 1 90) (create-inex 9 1 89))
   (create-inex 99 1 89)
   )       

(define (inex+ a b)
  (local (
          ; input constants
          (define man-a (inex-mantissa a))
          (define man-b (inex-mantissa b))
          (define sign-a (inex-sign a))
          (define sign-b (inex-sign b))
          (define exp-a (inex-exponent a))
          (define exp-b (inex-exponent b))
          (define lower-exp (if (> exp-a exp-b)
                                exp-a
                                exp-b))
          (define higher-exp (if (> exp-a exp-b)
                                exp-b
                                exp-a))
          (define signed-exp-a (* sign-a exp-a))
          (define signed-exp-b (* sign-b exp-b))
          ; sum helpers
          (define exp-diff (abs (- signed-exp-a signed-exp-b)))
          (define sum-man (+ man-a man-b))
          ; output values
          (define output-man (if (> sum-man 99)
                                 (/ sum-man 10)
                                 sum-man))
          (define output-sign (* sign-a sign-b)))
    (cond [(and (> sum-man 99)
                (= exp-a 99)) (error "exponent out of range")]
          [(= 0 exp-diff)
           (if (< sum-man 99)
               (make-inex output-man output-sign exp-a)
               (make-inex output-man output-sign (+ exp-a 1)))]
          [(= 1 exp-diff)
           (if (= -1 output-sign)
               (make-inex (+ (* 10 man-a) man-b) output-sign lower-exp)
               (make-inex (+ (* 10 man-b) man-a) output-sign higher-exp))])))

; =================== End of exercise ==================