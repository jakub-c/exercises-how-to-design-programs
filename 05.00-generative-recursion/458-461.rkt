;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |458|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 458 ====================

(define ε-01 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε-01)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε-01)
#; (check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
                 1000
                 ε-01) ; this test fails by 500
 
; (define (integrate-kepler f a b) #i0.0) ;stub

(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (area f L R) (* 0.5
                                  (- R L)
                                  (+ (f L)
                                     (f R)))))
    (area f a b)))


; =================== End of exercise ==================

; ==================== Exercise 459 ====================

; settings to make the tests pass for ε 0.1:
; (define ε-02 0.1)
; (define R 50)

; settings to make the tests pass for ε 0.01:
(define ε-02 0.01)
(define R 160)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b, using
; "sampling" method
; assume (< a b) holds

 
(check-within (integrate-divide (lambda (x) 20) 12 22) 200 ε-02)
(check-within (integrate-divide (lambda (x) (* 2 x)) 0 10) 100 ε-02)
(check-within (integrate-divide (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε-02)
 
; (define (integrate-divide f a b) #i0.0) ;stub

(define (integrate-divide f a b)
  (local ((define width (/ (- b a) R))
          (define rectangle-midpoint (/ width 2))
          (define (area i)
            (* width (f (- (* width i) rectangle-midpoint))))
          (define (calculate n)
            (cond [(= (* n width) (- b a)) (area n)]
                  [else
                   (+ (area n)
                      (calculate (add1 n)))])))
    (calculate 1)))

; =================== End of exercise ==================

