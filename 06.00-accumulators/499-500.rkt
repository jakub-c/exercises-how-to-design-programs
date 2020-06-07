;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 499-500) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 499 ====================

; [List-of Number] -> Number
; compute the product of a list of numbers
; (define (product lon) 0) ;stub

(check-expect (product '()) 0)
(check-expect (product '(4)) 4)
(check-expect (product '(4 4)) 16)
(check-expect (product '(1 1 3)) 3)
(check-expect (product '(2 3 1)) 6)

(define (product lon0)
  (local ((define (product/a lon a)
            (cond [(empty? lon) 0]
                  [(empty? (rest lon)) (* (first lon) a)]
                  [else (product/a (rest lon)
                                   (* (first lon) a))])))
    (product/a lon0 1)))

; Q: The performance of product is O(n) where n is the length of the list.
;      Does the accumulator version improve on this?

; A: Yes, we can skip the evaluation of an empty list for non-empty input,
;      therefore I think that the performance is O(n-1).

; =================== End of exercise ==================

; ==================== Exercise 450 ====================

; [X] [List-of X] -> Number
; determine the number of items on a list
; (define (how-many l) 0) ;stub

(check-expect (how-many '()) 0)
(check-expect (how-many '("a")) 1)
(check-expect (how-many '(1 2)) 2)
(check-expect (how-many '(#t #t #f)) 3)

(define (how-many l)
  (local ((define (how-many/a l a)
            (cond [(empty? l) a]
                  [(empty? (rest l)) (add1 a)]
                  [else (how-many/a (rest l)
                                    (add1 a))])))
    (how-many/a l 0)))

; Q: The performance of how-many is O(n) where n is the length of the list.
;     Does the accumulator version improve on this?

; A: It's the same case as in 499, I think that because of the
;     [(empty? (rest l)) (add1 a)] condition the performance is O(n-1)

; =================== End of exercise ==================