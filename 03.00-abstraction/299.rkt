;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |299|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a function: 
;   [Naural -> Boolean]
; interpretation if s is a set and n Natural, (s p) 
; produces #true if p is in s, #false otherwise

; Set Natural -> Boolean
(define (inside? s n)
  (s n))

; [Number -> Boolean] -> [Number -> Boolean]
(define (mk-set predicate)
  (lambda (n)
    (predicate n)))

(check-expect [(mk-set odd?) 4] #false)
(check-expect [(mk-set odd?) 3] #true)

; Set Natural -> Boolean
(define (add-element s n)
  (s n))

(define positive-numbers (mk-set positive?))
(define odd-numbers (mk-set odd?))

(define (lower-than x)
  (lambda (n)
    (< n x)))
(define numbers-below-100 (mk-set (lower-than 100)))

(add-element positive-numbers 4)

; Set Set -> Boolean
(define (union s1 s2)
  (lambda (x)
    (or (inside? s1 x) (inside? s2 x))))

; Set Set -> Boolean
(define (intersect s1 s2)
  (lambda (x)
    (and (inside? s1 x) (inside? s2 x))))

(define odd-or-below-100 (union odd-numbers numbers-below-100))
(define odd-and-below-100 (intersect odd-numbers numbers-below-100))

