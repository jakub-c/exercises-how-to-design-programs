;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |316|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of: 
; – Number
; – String
; – Symbol

(check-expect (atom? 1) #t)
(check-expect (atom? "a") #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? '()) #f)

; (define (atom? a) #f) ;stub

(define (atom? a)
  (or
   (number? a)
   (string? a)
   (symbol? a)))