;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |501|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 501 ====================

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi.v2 0) pi 0.001)
(check-within (add-to-pi.v2 2) (+ 2 pi) 0.001)
(define (add-to-pi.v2 n0)
  (local ((define (add-to-pi.v2/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi.v2/a (sub1 n)
                                    (add1 a))])))
    (add-to-pi.v2/a n0 pi)))

; =================== End of exercise ==================
