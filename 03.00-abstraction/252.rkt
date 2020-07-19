;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |252|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
         (rest l)))]))
  
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

; [List-of ITEM] -> ITEM2
(define (fold2 list function defaultValue)
  (cond
    [(empty? list) defaultValue]
    [else
     (function
      (first list)
      (fold2 (rest list) function defaultValue))]))

(define (product-fold l)
  (fold2 l * 1))

(check-expect (product '(1 2 3))
              (product-fold '(1 2 3)))

(define (image*-fold l)
  (fold2 l place-dot emt))

(check-expect (image*`(,(make-posn 2 2) ,(make-posn 2 3)))
              (image*-fold `(,(make-posn 2 2) ,(make-posn 2 3))))