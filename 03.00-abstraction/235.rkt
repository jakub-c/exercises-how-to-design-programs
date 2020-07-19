;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |235|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

(define (contains-atom? l)
  (contains? "atom" l))

(check-expect (contains-atom? '("atom")) #t)
(check-expect (contains-atom? '("not-atom" "something-else" )) #f)

(define (contains-basic? l)
  (contains? "basic" l))

(check-expect (contains-basic? '("basic")) #t)