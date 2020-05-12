;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |437|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#;(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
      P
      (special (rest P)))]))


; ==================== Exercise 437 ====================

; [X] [List-of X] -> Number
; computes the length of the input
; (define (special-length l) ...) ;stub

(check-expect (special-length '()) 0)
(check-expect (special-length '(1)) 1)
(check-expect (special-length '(1 2 3 4)) 4)

(define (special-length P)
  (cond
    [(empty? P) 0]
    [else
     (combine-solutions-length
      1
      (special-length (rest P)))]))

; Number Number -> Number
; add two numbers
(define (combine-solutions-length n1 n2)
  (+ n1 n2))


; [List-of Number] -> [List-of Number]
; negate each number on the given list of numbers
; (define (special-negate l) '()) ;stub

(check-expect (special-negate '()) '())
(check-expect (special-negate '(1)) '(-1))
(check-expect (special-negate '(-1 0 1)) '(1 0 -1))

(define (special-negate P)
  (cond
    [(empty? P) '()]
    [else
     (combine-solutions-negate
      (first P)
      (special-negate (rest P)))]))

; Number Number -> [List-of Number]
; negate the first argument and add it to the list with the second argument

(define (combine-solutions-negate n1 n2)
  (cons
   (* -1 n1)
   n2))

; [List-of String] -> [List-of String]
; convert given list to uppercase
; (define (special-uppercase l) '()) ;stub

(check-expect (special-uppercase '()) '())
(check-expect (special-uppercase '("hello")) '("HELLO"))
(check-expect (special-uppercase '("hello world")) '("HELLO WORLD"))

(define (special-uppercase P)
  (cond
    [(empty? P) '()]
    [else
     (combine-solutions-uppercase
      (first P)
      (special-uppercase (rest P)))]))

; String String -> [List-of String]
; convert given string to uppercase and add it to the list
(define (combine-solutions-uppercase s1 s2)
  (cons
   (string-upcase s1)
   s2))

; Q: What do you conclude from these exercises?
; A: The generative recursion from this exercise
;    follows the same design process as the
;    natural recursion described earlier in the book

; =================== End of exercise ==================
