;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 493-494) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 493 ====================

; Q: Argue that, in the terminology of
;      Intermezzo 5: The Cost of Computation,
;      invert consumes O(n2) time when the given list
;      consists of n items.

; A: Just like in the exercise 490 the answer is
;      O(n2) because of the two recursive calls
;      (add-as-last and invert iteself) are made
;      per each step of the function

; =================== End of exercise ==================

; ==================== Exercise 494 ====================

; Q: Does the insertion sort> function from Auxiliary Functions
;     that Recur need an accumulator? If so, why? If not, why not?

; A: Yes, sort> does fall into the category described in the
;     Accumulator recipe:
;     "If a structurally recursive function traverses
;     the result of its natural recursion with an auxiliary,
;     recursive function, consider the use of an accumulator parameter."

; =================== End of exercise ==================

; NOTE
; after comparing the execution time of inver and invert.v2
; it looks like invert.v2 takes 0 time units for a 1000 item list
; the regular invert used about 7 time units,
; therefore we can conclude that accumulator-style functions are
; faster