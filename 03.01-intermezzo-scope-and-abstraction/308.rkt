;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |308|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])
; a Phone is (make-phone Number Number Number)

; [List-of Phone] -> [List-of Phone]
; substitutes the area code 713 with 281 in a list of phone records
; (define (replace lop) '()) ;stub

(define input
  `(,(make-phone 713 664 9993)
    ,(make-phone 113 664 9993)))

(define expect
  `(,(make-phone 281 664 9993)
    ,(make-phone 113 664 9993)))

(check-expect (replace input) expect)

(define (replace lop)
  (for/list ((p lop))
    (match p
      [(phone 713 swich four) (make-phone 281 swich four)]
      [(phone area swich four) (make-phone area swich four)])))