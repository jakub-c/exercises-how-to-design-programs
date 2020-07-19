;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |239|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A [List Number Number] is a strucutre:
;   (cons X (cons Y '()))

(define NN-example (cons 1 (cons 1 '())))

; A [List Number 1String] is a strucutre:
;   (cons X (cons Y '()))

(define NS-example (cons 1 (cons 'a' '())))

; A [List String Boolean] is a strucutre:
;   (cons X (cons Y '()))

(define SB-example (cons 'abc' (cons #true '())))
