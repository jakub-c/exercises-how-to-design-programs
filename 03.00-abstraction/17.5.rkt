;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |17.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Shape is a function: 
;   [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p) 
; produces #true if p is in s, #false otherwise

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))


; Number Number -> Shape 
(define (mk-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))
 
(define a-sample-shape (mk-point 3 4))


; represents a point at (x,y)