;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |251|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))
  
; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [List-of Number] -> Number
(define (fold1 list function minVal)
  (cond
    [(empty? list) minVal]
    [else
     (function
      (first list)
      (fold1 (rest list) function minVal))]))

(define (product-from-fold l)
  (fold1 l * 1))

(define (sum-from-fold l)
  (fold1 l + 0))