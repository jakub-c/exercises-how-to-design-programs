;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |282|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> Boolean
#;(define (compare x)
  (= (f-plain x) (f-lambda x)))

(define-struct ir (name price))
(define th 11)

((lambda (ir) (<= (ir-price ir) th))
 (make-ir "bear" 10))