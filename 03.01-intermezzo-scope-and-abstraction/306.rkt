;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |306|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require racket/list)

; Number -> List-of Number
; create the list (list 0 ... (- n 1)) for any natural number n
(check-expect (create-nat-numbers 3) '(0 1 2))
(check-expect (create-nat-numbers 0) '())

; (define (create-nat-numbers n) '()) ;stub
(define (create-nat-numbers n)
  (for/list ([i n])
    i))

; create the list (list 1 ... n) for any natural number n;
(check-expect (create-nat-numbers-from-1 3) '(1 2 3))
(check-expect (create-nat-numbers-from-1 0) '())

; (define (create-nat-numbers-from-1 n) '()) ;stub
(define (create-nat-numbers-from-1 n)
  (for/list ([x n] [i (in-naturals 1)])
    i))

; create the list (list 1 1/2 ... 1/n) for any natural number n;
(check-expect (create-frac 3) `(1 ,(/ 1 2)  ,(/ 1 3)))
(check-expect (create-frac 1) '(1))

(define (create-frac n)
  (for/list ([x n] [i (in-naturals 1)])
    (/ 1 i)))

; create the list of the first n even numbers; and
(check-expect (n-even-numbers 3) '(0 2 4))

; (define (n-even-numbers n) '()) ;stub
(define (n-even-numbers n)
  (for/list ([i n])
    (* 2 i)))

; create a diagonal square of 0s and 1s; see exercise 262.

(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; (define (identityM n) '()) ;stub
(define (identityM n)
  (for*/list ([i n])
    (list-set
     (make-list n 0)
     i
     1)))


; Number -> [List-of Number]
(define (tabulate number function)
  (for/list ([x number] [i (in-naturals 1)])
    (function i)))