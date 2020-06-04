;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |485|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 485 ====================

; Tree is one of:
;  - Number
;  - (list Tree Tree)

(define t1 4)
(define t2 (list 3 1))
(define t3 (list (list (list 4 5) (list 2 1)) 4))

; Tree -> Number
; determine the sum of the numbers in a tree
; (define (sum-tree t) 0) ;stub

(check-expect (sum-tree t1) 4)
(check-expect (sum-tree t2) 4)
(check-expect (sum-tree t3) 16)

(define (sum-tree t)
  (cond [(number? t) t]
        [(empty? t) 0]
        [else
         (+ (sum-tree (first t))
            (sum-tree (rest t)))]))

; Q: What is its abstract running time?
; A: The program sum-tree “on the order of n^2 steps”,
;     because it's running two recursive calls per step
;     n - parses or adds a number represented by the tree

; Q: What is an acceptable measure of the size of such a tree?
; A: I'm not sure about the answer here, should be tree be
;     "measured" by the number of lists?

; Q: What is the worst possible shape of the tree?
; A: Best case: single number (return immediately)
;    Worst case: sum-tree needs on the order of n^2 recursive steps
;     for a list of n lists when the number of lists if at it's maximum

; NOTE: I'd be happy to get my n^2 answer verified

; =================== End of exercise ==================