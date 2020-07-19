;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |236|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))

(check-expect (add1* '(1 2 3)) '(2 3 4))
     
; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

(check-expect (plus5 '(5 10 15)) '(10 15 20))

; Lon -> Lon
; add x to each item on l
(define (addx* x l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ x (first l))
      (addx* x (rest l)))]))

(define (add1-abstracted l)
  (addx* 1 l))

(check-expect (add1-abstracted '(1 2 3)) '(2 3 4))

(define (plus5-abstracted l)
  (addx* 5 l))

(check-expect (plus5-abstracted '(5 10 15)) '(10 15 20))

(define (subtract2 l)
  (addx* -2 l))
 
(check-expect (subtract2 '(1 2 3)) '(-1 0 1))