;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |240|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

(define EXAMPLE-string (make-layer "abc"))
    
; An LNum is one of: 
; – Number
; – (make-layer LNum)

(define EXAMPLE-num (make-layer 1))

; An [ITEM] is one of:
; - ITEM
; - (make-layer ITEM)

; An L-Num is one of:
; - Number
; - (make-layer L-Num)

; An L-String is one of:
; - String
; - (make-layer L-String)