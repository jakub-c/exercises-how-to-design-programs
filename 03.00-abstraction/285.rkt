;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |285|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Use map to define the function convert-euro,
; which converts a list of US$ amounts into a
; list of € amounts based on an exchange rate of US$1.06 per €.

(define RATE 1.06)

; [List-of Number] -> [List-of Number]
(check-expect (convert-euro '(1 2 3)) (list (* 1 RATE) (* 2 RATE) (* 3 RATE)))
;(define (convert-euro lon) '()) ;stub

(define (convert-euro lon)
  (map (lambda (n) (* n RATE)) lon))

;converts a list of temperatures in F to C
; [List-of Numbers] Number -> [List-of Numbers]
(check-expect (convertFC '(84)) (list (* (- 84 32) (/ 5 9))))

(define (convertFC lot)
  (map (lambda (f) (* (- f 32) (/ 5 9))) lot))

; [List-of Posn] -> [List-of [List-of Number]]
; translate List-of Posn into a List-of List-of Numbers
(check-expect (translate `(,(make-posn 10 2) ,(make-posn 30 40)))
              '((10 2) (30 40)))

; (define (translate lop) '()) ;stub
(define (translate lop)
  (map (lambda (p) `(,(posn-x p) ,(posn-y p))) lop))