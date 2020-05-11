;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 431-432) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 431 ====================

; 1. the bundle problem

; Q: What is a trivially solvable problem?
; A: '() passed as an argument
;
; Q: How are trivial solutions solved?
; A: solvable with a single: [(empty? s) '()]

; Q: How does the algorithm generate new problems that are more easily solvable than the original one?
;    Is there one new problem that we generate or are there several?
; A: The new problem consists of taking X elements of the list and dropping X elements from the list.
;    The two new ones are indeed easier to solve compared with the original problem.

; Q: Is the solution of the given problem the same as the solution of (one of) the new problems?
;    Or, do we need to combine the solutions to create a solution for the original problem?
;    And, if so, do we need anything from the original problem data?
; A: The solition to drop and take functions is different than the solution to the bundle problem.
;    Therefore the two new functions need to be combined with the recursive call to the bundle problem.

; 2. the quick-sort problem

; Q: What is a trivially solvable problem?
; A: '() passed as an argument
;
; Q: How are trivial solutions solved?
; A: solvable with a single: [(empty? s) '()]

; Q: How does the algorithm generate new problems that are more easily solvable than the original one?
;    Is there one new problem that we generate or are there several?
; A: Two new problems are generated - how do we filter out numbers higher and lower than the pivot.

; Q: Is the solution of the given problem the same as the solution of (one of) the new problems?
;    Or, do we need to combine the solutions to create a solution for the original problem?
;    And, if so, do we need anything from the original problem data?
; A: The two new problems are solved with one filter function that is usead in the following ways:
;     - default mode - filter according to the cmd value
;     - inverted move - use the same filter function and combine it with not operator


; =================== End of exercise ==================
