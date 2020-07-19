;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |245|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Function Function -> Boolean
(check-expect (function=? (+ 1 1) (+ 0 2)) #t)
(check-expect (function=? (+ 0 1) (+ 0 2)) #f)

(define (function=? f1 f2)
  (= f1 f2))
  