;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |495|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number

(check-expect (sum.v1 '(1 2 3)) 6)
(check-expect (sum.v2 '(1 2 3)) 6)

(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))


(define (sum.v2 alon0)
  (local (; [List-of Number] ??? -> Number
          ; computes the sum of the numbers on alon
          ; accumulator a is the sum of the numbers 
          ; that alon lacks from alon0
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                           (+ (first alon) a))])))
    (sum/a alon0 0)))

; ==================== Exercise 495 ====================

; sum.v1
; (+ 1 (sum.v1 '(2 3)))
; (+ 1 (+ 2 (sum.v1 '(3))))
; (+ 1 (+ 2 (+ 3 (sum.v1 '()))))
; (+ 1 2 3)

; sum.v2
#; (sum/a '(2 3)
       (+ 1 0))

#; (sum.v2 '(3)
        (+ 2 (+ 1 0)))

#; (+ 3 (+ 2 (+ 1 0)))

; =================== End of exercise ==================

; ==================== Exercise 496 ====================

#;(define (!.v2 n0)
  (local (; N ??? -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator ...
          (define (!/a n a)
            (cond
              [(zero? n) ...]
              [else (... (!/a (sub1 n)
                              ... a ...) ...)])))
    (!/a n0 ...)))

; Q: What should the value of a be when n0 is 3 and n is 1?
;      How about when n0 is 10 and n is 8?

; A: When n0 is 3  and n is 1 - a is 1
;    When n0 is 10 and n is 8 - a is 8

#;(define (!.v2 n0)
  (local (; N N -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator a is the product of the 
          ; natural numbers in the interval [n0,n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))

; =================== End of exercise ==================

(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

; (time (map (lambda (_) (!.v1 20)) (range 1 1001 1)))
; takes 3 units of time