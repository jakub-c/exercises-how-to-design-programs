;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 462-470) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(check-expect (subtract '(2 4 8 2) '(6 9 21 3))
              '(-3 -3 -3))

(define (subtract eq-1 eq-2)
  (local
    ((define factor (/ (first eq-2) (first eq-1))))

    ; -- IN --
    (for/list [(a (rest eq-1)) (b (rest eq-2))]
      (- b (* a factor))          
      )))

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

; ==================== Exercise 467 ====================

(check-expect (triangulate.v2
               '((2  3  3 8)
                 (2  3 -2 3)
                 (4 -2  2 4)))
              '((2   3   3   8)
                (   -8  -4  -12)
                (       -5  -5)))

(define (triangulate.v2 M)
  (cond [(empty? M) '()]
        [(= (first (first M)) 0)
         (local ((define rotated-matrix (append (rest M) (list (first M)))))
           (triangulate.v2 rotated-matrix))]
        [else
         (cons (first M)
               (triangulate.v2
                (map (lambda (el) (subtract (first M) el))
                     (rest M))))]))

; =================== End of exercise ==================

; ==================== Exercise 468 ====================

(check-error (triangulate.v3
              '((2 2 2  6)
                (2 2 4  8)
                (2 2 1  2))))

(define (triangulate.v3 M)
  (cond [(empty? M) '()]
        [; all leading coefficients are 0
         (for/and ([el M]) (zero? (first el)))
         (error "this system of equations does not have a sollution")]
        [(= (first (first M)) 0)
         (local ((define rotated-matrix (append (rest M) (list (first M)))))
           (triangulate.v3 rotated-matrix))]
        [else
         (cons (first M)
               (triangulate.v3
                (map (lambda (el) (subtract (first M) el))
                     (rest M))))]))

; =================== End of exercise ==================

; ==================== Exercise 469 ====================

; This could be solved via a foldr function as well
; I might refactor it

; TM -> [List-of Number]
; solve a given SOE
; (define (solve M) 0) ;stub

(check-expect (solve '((2   3   3   8)
                       (   -8  -4  -12)
                       (       -5  -5)))
              '(1 1 1))
(check-expect (solve '((2  2  3   10) ; an Equation 
                       (   3  9   21)
                       (      1   2)))
              '(1 1 2))

(check-expect (solve '((3  9   21)
                       (   1   2)))
              '(1 2))
(check-expect (solve '((-5 -5)))
              '(1))

(define (solve TM)
  (foldr (lambda (eq solution)
           (append (solve-eq eq solution) solution))
         '()
         TM))

; this is an old soltution, without foldr
; clearly it's way worse and more convoluted than the simpler one above
#;(define (solve M)
  (local ((define (solve-tri tri sol)
            (local ((define solved-eq-value (solve-eq (first tri) sol)))
              (cond [(empty? tri) '()]
                    [(= 1 (length tri)) solved-eq-value]
                    [else
                     (append solved-eq-value
                             (solve-tri (rest tri)
                                        (append solved-eq-value sol)))]))))
    (reverse (solve-tri (reverse M) '()))))

; Equation Solution -> Number
; (define (solve-eq eq sol) 0) ;stub

(check-expect (solve-eq '(2 2 3 10) '(1 2))
              '(1))
(check-expect (solve-eq '(-8 -4 -12) '(1))
              '(1))
(check-expect (solve-eq '(1 2) '())
              '(2))

(define (solve-eq eq solution)
  (local ((define first-coefficient (first eq))
          (define coefficients-to-solve (rest (lhs eq)))
          (define coefficients-plugged-values
            (foldr (lambda (coefficient value result)
                     (+ result (* coefficient value)))
                   0
                   coefficients-to-solve
                   solution))
          (define rhs-val (rhs eq)))
    (list (/
           (- rhs-val coefficients-plugged-values)
           first-coefficient))))

; =================== End of exercise ==================

; ==================== Exercise 470 ====================

; SOE -> Solution
; solve SOE using Gaussian Elimination
; (define (gauss M) '()) ;stub

(check-expect (gauss M) S)

(define (gauss M)
  (solve (triangulate.v3 M)))

; =================== End of exercise ==================
