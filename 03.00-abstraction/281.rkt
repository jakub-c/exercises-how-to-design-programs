;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |281|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Write down a lambda expression that:
; - consumes a number and decides whether it is less than 10;
((lambda (x) (< x 10)) 5)

;multiplies two given numbers and turns the result into a string;
((lambda (x y) (number->string (* x y))) 2 4)

;consumes a natural number and returns 0 for evens and 1 for odds;
((lambda (n) (if (even? n)
                 0
                 1))
 2)

;consumes two inventory records and compares them by price
(define-struct ir [name price])

((lambda (ir1 ir2) (> (ir-price ir1) (ir-price ir2)))
 (make-ir "item1" 10)
 (make-ir "item2" 20))

;adds a red dot at a given Posn to a given Image.
((lambda (p img)
   (local (
           (define dot (circle 5 "solid" "red")))
     (place-image dot
                  (posn-x p)
                  (posn-y p)
                  img)))
 (make-posn 3 3)
 (square 20 "solid" "blue"))