;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9.2-checked-avarage) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -273.

; List-of-temperatures -> Number
; computes the checked-average temperature
(check-expect
  (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)
(check-error (checked-average '()) "the list can not be empty")

(define (checked-average alot)
  (cond
    [(= (how-many alot) 0) (error "the list can not be empty")]
    [else (/ (sum alot) (how-many alot))]))
 
; List-of-temperatures -> Number 
; adds up the temperatures on the given list
(check-expect (sum '()) 0)
(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)

;(define (sum alot) 0) ;stub
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else
     (+ (first alot)
        (sum (rest alot)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list
(check-expect (how-many '()) 0)
(check-expect
  (how-many (cons 1 (cons 2 (cons 3 '())))) 3)

; (define (how-many alot) 0) ;stub
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else
     (+ 1 (how-many (rest alot)))]))