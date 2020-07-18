;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 518-519) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 518 ====================

; Q: Argue that our-cons takes a constant amount
;     of time to compute its result,
;     regardless of the size of its input

; A: our-cons takes a constant amount of time
;     because it does not need to traverse
;     the whole list, it just looks a the counter.
;     The follwing line of the function's body is responsible
;     for the number retrieval:
;       make-cpair (+ (cpair-count r) 1) f r)

; =================== End of exercise ==================

; ==================== Exercise 519 ====================

; Q: Is it acceptable to impose the extra cost on cons
;     for all programs to turn length into a constant-time function?

; A: It depends. These days getting the size getting a lenght of
;     a list is not very CPU intensive.
;     Possibly if our programs would need to get lenghts of huge
;     lists per every CPU clock cycle, such an optimization
;     would yield some benefits.

; =================== End of exercise ==================
