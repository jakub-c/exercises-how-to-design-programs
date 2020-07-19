;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |267|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define EXRATE 1.06)

;converts a list of US$ amounts into a list of € amounts based on an exchange rate
; [List-of Numbers] Number -> [List-of Numbers]
(check-expect (convert-euro '(1 2)) `(,(/ 1 EXRATE) ,(/ 2 EXRATE)))

;(define (convert-euro lou exchange) '()) ;stub

(define (convert-euro lou)
  (local (; Number Number -> Number
          ; apply the currencty convetion
          (define (make-convertion currency) (/ currency EXRATE)))
    (map make-convertion lou)))


;converts a list of temperatures in F to C
; [List-of Numbers] Number -> [List-of Numbers]
(check-expect (convertFC '(84)) (list (* (- 84 32) (/ 5 9))))

(define (convertFC lot)
  (local (; Number Number -> Number
          ; apply the Farenheit to Celcius convetion
          (define (make-convertion f) (* (- f 32) (/ 5 9))))
    (map make-convertion lot)))


; [List-of Posn] -> [List-of [List-of Number]]
; translate List-of Posn into a List-of List-of Numbers
(check-expect (translate `(,(make-posn 10 2) ,(make-posn 30 40)))
  '((10 2) (30 40)))

; (define (translate lop) '()) ;stub
(define (translate lop)
  (local (; Posn -> List-of Numbers
          (define (to-list p) `(,(posn-x p) ,(posn-y p))))
    (map to-list lop)))