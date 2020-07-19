;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |255|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; map-n, which consumes a list of numbers and a function from numbers to numbers to produce a list of numbers.

; [List-of Number] (Number -> Number) -> [List-of Number]

; map-s, which consumes a list of strings and a function from strings to strings and produces a list of strings.

; [List-of String] (String -> String) -> [List-of String]

;; abstracted signature
; [X] [List-of X] (X -> X) -> [List-of X]

(define (map1 k g)
  (cond
    [(empty? k) '()]
    [else
     (cons
       (g (first k))
       (map1 (rest k) g))]))


; [Number] [List-of Number] (Number -> Number) -> [List-of Number]
(define (add-01 l)
  (map1 l add1))

(check-expect (add-01 '(1 2))
              '(2 3))
             