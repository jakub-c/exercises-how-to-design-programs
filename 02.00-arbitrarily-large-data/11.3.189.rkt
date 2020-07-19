;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 11.3.189) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Number List-of-numbers -> Boolean
; search for the number in a sorted list
; the list has to be sorted in an descending order

(check-expect (search 4 '()) #false)
(check-expect (search 2 (list 3 2 1)) #true)
(check-expect (search 5 (list 10 7 5 2)) #true)
(check-expect (search 5 (list 10 7 4 2)) #false)

(define (search n alon)
  (cond
    [(empty? alon) #false]
    [(> n (first alon)) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))
