;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |462|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; ==================== Exercise 462 ====================

; [List-of Number] Solution -> Number
; calculate out the value of the left-hand side
; when the numbers from the solution are plugged in for the variables
; (define (plug-in lhs solution) 0) ;stub

(check-expect (plug-in '(2 2 3) '(1 1 2)) 10)

(define (plug-in lhs solution)
  (for/sum ([l lhs] [s solution])
    (* l s)))

; SOE Solution -> Boolean
; produce #true if plugging in the numbers from
; the Solution for the variables in
; the Equations of the SOE produces equal
; left-hand-side values and right-hand-side values
; (define (check-solution soe solution) #false) ;stub

(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(10 2 3)) #false)

(define (check-solution soe solution)
  (for/and ([e soe])
    (= (plug-in (lhs e) solution)
       (rhs e))))

; =================== End of exercise ==================

; ==================== Exercise 463 ====================

; check equations for '(1 1 2)
; 2x + 2y + 3z = 10
;      3y + 9z = 21
;           1z = 2
; ===
; 2  + 2  + 6  = 10
;      3  + 18 = 21
;           2  = 2

(define M-463 ; an SOE 
  (list (list 2 2 3  10) ; an Equation 
        (list 0 3 9  21)
        (list 0 0 1  2)))

(define S-463 '(1 1 2)) ; a Solution

(check-expect (check-solution M-463 S-463) #true)

; =================== End of exercise ==================

; ==================== Exercise 464 ====================

; check equations for '(1 1 2)
; 2x + 3y + 3z =  10
;      3y + 9z =  21
;     -3y - 8z = -19
; ===
; 2  + 3  + 6  =  10
;      3  + 18 =  21
;     -3  - 16 = -19

(define M-464 ; an SOE 
  (list (list 2  2  3    10) ; an Equation 
        (list 0  3  9    21)
        (list 0 -3 -8   -19)))

(define S-464 '(1 1 2)) ; a Solution

(check-expect (check-solution M-464 S-464) #true)

; =================== End of exercise ==================

; ==================== Exercise 465 ====================

; Equation Equation -> [List-of Number]
; “subtract” a multiple of the second equation from the first,
; item by item, so that the resulting Equation has a 0 in the first position
; skip the leading 0 when returning a value
; (define (subtract eq1 eq2) '(1 2 3)) ;stub

(check-expect (subtract '(2 2 3  10) '(2 5 12 31))
              (list 3 9 21))
(check-expect (subtract '(3 9 21) '(-3 -8 -19))
              '(1 2))

(define (subtract eq1 eq2)
  (local ((define operator (if (>= (first eq2) (first eq1))
                               -
                               +))
          (define result (map operator eq2 eq1))
          (define first-number (first result)))
    (cond  [(= first-number 0) (rest result)]
           [else (subtract eq1 result)])))

; =================== End of exercise ==================

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; ==================== Exercise 466 ====================

; SOE -> TM
; triangulates the given system of equations 
; (define (triangulate M) '(1 2)) ;stub

(check-expect (triangulate M)
              '((2  2  3   10) ; an Equation 
                (   3  9   21)
                (      1   2)))

; Q: What is a trivially solvable problem?
; A: M is empty

; Q: How are trivial solutions solved?
; A: Return an empty array

; Q: How does the algorithm generate new problems that are
;        more easily solvable than the original one?
; A: It runs the same operations on the list that is
;        getting shorter with each run


; Q:Is the solution of the given problem the same
;        as the solution of (one of) the new problems?
; A: Yes, the list is just getting shorter


(define (triangulate M)
  (cond [(empty? M) '()]
        [else
         (cons (first M)
               (triangulate
                (map (lambda (el) (subtract (first M) el))
                     (rest M))))]))

; =================== End of exercise ==================
