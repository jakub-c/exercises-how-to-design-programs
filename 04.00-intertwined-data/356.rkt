;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |356|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name ex])

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)
; - (make-fun Symbol BSL-var-expr)

; (k (+ 1 1))
(make-fun 'k (+ 1 1))

; (* 5 (k (+ 1 1)))
(make-mul 5
          (make-fun 'k (+ 1 1)))

;(* (i 5) (k (+ 1 1)))
(make-mul (make-fun 'i 5)
          (make-fun 'k
                    (make-add 1 1)))
