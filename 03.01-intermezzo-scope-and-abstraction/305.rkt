;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |305|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define EXRATE 1.06)

;converts a list of US$ amounts into a list of € amounts based on an exchange rate
; [List-of Numbers] Number -> [List-of Numbers]
(check-expect (convert-euro '(1 2)) `(,(/ 1 EXRATE) ,(/ 2 EXRATE)))

;(define (convert-euro lou exchange) '()) ;stub

#;(define (convert-euro lou)
  (local (; Number Number -> Number
          ; apply the currencty convetion
          (define (make-convertion currency) (/ currency EXRATE)))
    (map make-convertion lou)))

(define (convert-euro lou)
  (for/list ([i lou])
    (/ i EXRATE)))