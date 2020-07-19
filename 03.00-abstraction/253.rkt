;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |253|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Boolean]
(number? 1)


; [Boolean String -> Boolean]

; [Number Number Number -> Number]
(+ 1 1 1)

; [Number -> [List-of Number]]
(define (generateNumbers n)
  (cond
    [(= n 0) '()]
    [else
     (cons n
           (generateNumbers (- n 1)))]))

(generateNumbers 3)

; [[List-of Number] -> Boolean]
(define (isListOfNumbers? l)
  (cond
    [(empty? l) #f]
    [(and
      (number? (first l))
      (empty? (rest l))) #t]
    [else
     (and (number? (first l))
          (isListOfNumbers? (rest l)))]))

(isListOfNumbers? '(1 2))
(isListOfNumbers? '())
(isListOfNumbers? '(1 2 "A"))