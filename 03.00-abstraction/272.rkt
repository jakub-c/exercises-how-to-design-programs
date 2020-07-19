;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |272|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List List -> List
; (define (append-from-fold l1 l2) '()) ;stub
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))


; Use foldr to define append-from-fold.
(equal? (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
        (list 1 2 3 4 5 6 7 8))

; List-of-Number -> Number
; sum all numbers in the list
(check-expect (sum-from-fold '(1 3 5)) 9)

; (define (sum-from-fold lon) 0) ;stub

(define (sum-from-fold lon)
  (foldr + 0 lon))

; List-of-Number -> Number
; sum all numbers in the list
(check-expect (product-from-fold '(1 3 5)) 15)

; (define (product-from-fold lon) 0) ;stub

(define (product-from-fold lon)
  (foldr * 1 lon))

(require 2htdp/image)
(define CIRCLES (list
                 (circle 20 "solid" "green")
                 (circle 20 "solid" "blue")
                 (circle 20 "solid" "violet")))

; List-of-Image -> Image
; compose list of images horizontaly
;(define (compose-hor l) empty-image) ;stub

(define (compose-hor l)
  (foldr beside empty-image l))

(compose-hor CIRCLES)

(define (compose-hor-v2 l)
  (foldl beside empty-image l))

(compose-hor-v2 CIRCLES)

; List-of-Image -> Image
; compose list of images vertically
;(define (compose-ver l) empty-image) ;stub

(define (compose-ver l)
  (foldr above empty-image l))

(compose-ver CIRCLES)